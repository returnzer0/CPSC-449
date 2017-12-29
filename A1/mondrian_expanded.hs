-- mondrian_base.hs
-- Munib Rahman
-- 10128547
-- Region: A place on the canvas that is identifiable via its x and y coordinates representing the top left point. A width and height that are treated as offsets from the x,y.

import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)

width :: Int
width = 1024

height :: Int
height = 768

randomList :: Int -> [Float]
randomList seed = take 200000 (rl_helper (mkStdGen seed))

rl_helper :: StdGen -> [Float]
rl_helper g = fst vg : rl_helper (snd vg)
  where vg = randomR (0.0, 1.0 :: Float) g

randomInt :: Int -> Int -> Float -> Int
randomInt low high x = round ((fromIntegral (high - low) * x) + fromIntegral low)

title :: String
title = "Pillars - Munib Rahman 2017"

-- This function decides which stroke to use depending on the x,y coordinates of an region.
-- The stroke values are loosely decided by current coordinates of the region.
stroke :: Int -> Int -> Float
stroke x y
   | x < 270 && y < 300 = 0.0
   | x < 500 && y < 500 = 0.5
   | x < 700 && y < 700 = 1.0
   | otherwise          = 1.5

-- The color of an object is semi-dependant on the x,y coordinates of the region, other part of the randomness is decided by using the float values.
-- This function returns an rgb string.
color :: Int -> Int -> Float -> Float -> Float -> String
color x y r s t = "rgb(" ++ 
                         (show red) ++ "," ++
                         (show green) ++ "," ++
                         (show blue) ++ ")"

                         where
                          red   = (randomInt x 255 r) `mod` 255
                          green = (randomInt y 255 s) `mod` 255
                          blue  = (randomInt (x+y) 255 t) `mod` 255

-- A function that splits a given region, either vertically, horizontally or both at the same time. The retun type is a list of the newly split regions as tuples.
--          x: x coordinate of the region
--          y: y coordinate of the region
--          w: width of the region
--          h: height of the region
--   vertical: A boolean that decides if a region is to be split vertically
-- horizontal: A boolean that decides if a region is to be split horizontally
splitRegion :: Int -> Int -> Int -> Int -> Float -> Bool -> Bool -> [(Int, Int, Int, Int)]
splitRegion x y w h r vertical horizontal
   | vertical && horizontal = [topLeft, topRight, bottomLeft, bottomRight]
   | vertical               = [left, right]
   | horizontal             = [top, bottom]
    where
      -- We allow the region to be randomly split between three-tenths and seven-tenths of a given side, either width or height.
      splitPointH = randomInt (round ((3 / 10) * fromIntegral w)) (round ((7 / 10) * fromIntegral w)) r
      splitPointV = randomInt (round ((3 / 10) * fromIntegral h)) (round ((7 / 10) * fromIntegral h)) r
      
      -- Splitting the regions into their proper sections.
      topLeft     = (              x,               y,       splitPointH,       splitPointV)
      topRight    = (x + splitPointH,               y, (w - splitPointH),       splitPointV)
      bottomLeft  = (              x, splitPointV + y,       splitPointH, (h - splitPointV))
      bottomRight = (splitPointH + x, splitPointV + y, (w - splitPointH), (h - splitPointV))

      left        = (              x,               y,       splitPointH,                 h)
      right       = (splitPointH + x,               y, (w - splitPointH),                 h)

      top         = (              x,               y,                 w,       splitPointV)
      bottom      = (              x, splitPointV + y,                 w, (h - splitPointV))

-- This function draws a border frame around the whole pallete, essentially making it look a little more fancy.
drawFrame :: (Int, Int, Int, Int) -> ((Int, Int, Int, Int), String)
drawFrame (x, y, w, h) = ((x + borderWidth, y + borderWidth, w - (borderWidth * 2), h - (borderWidth * 2)),
                         "<rect x=" ++ (show x) ++ 
                         " y=" ++ (show y) ++
                         " width=" ++ (show w) ++ 
                         " height=" ++ (show h) ++ 
                         " stroke=\"black\"" ++
                         " stroke-width=\"" ++ "2" ++ "\"" ++
                         " fill=\"" ++ borderColor ++ "\" />\n" ++ 
                         " <text x=" ++ show borderWidth ++  
                         " y=" ++ "763" ++
                         " font-family=" ++ "Verdana" ++ 
                         " font-size=" ++ "20>" ++
                         title ++ "</text>\n")
   where
    borderWidth = 25
    -- I decided to keep the border color static because I think it looks nice.
    borderColor = "rgb(218,165,32)"

-- The main mondrian function. Requires the x,y coordinates of the top left of the region along with the width and height.
mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:s:t:rs)

   -- The very first call will result in adding a border/frame to the whole canvas.
   | w == 1024 && h == 768 =

    let ((x1, y1, w1, h1), border)   = drawFrame (x, y, w, h)
        (rs', resultantArea)         = mondrian x1 y1 w1 h1 rs
    in
        (rs', border ++ resultantArea)

   | (regionIsWideEnough && regionIsTallEnough) || (okToSplitHorizontally && okToSplitVertically) = 
    let [(x1, y1, w1, h1), (x2, y2, w2 , h2), (x3 , y3, w3, h3), (x4, y4, w4, h4)] = splitRegion x y w h r True True

        (rs'   , topLeftSVGTag)      = mondrian x1 y1 w1 h1 rs
        (rs''  , topRightSVGTag)     = mondrian x2 y2 w2 h2 rs'
        (rs''' , bottomLeftSVGTag)   = mondrian x3 y3 w3 h3 rs''
        (rs'''', bottomRightSVGTag)  = mondrian x4 y4 w4 h4 rs'''

    in  (rs'''', topLeftSVGTag ++ topRightSVGTag ++ bottomLeftSVGTag ++ bottomRightSVGTag)

   | regionIsWideEnough || okToSplitHorizontally =

   -- We only want to split the region vertically, thus the first boolean is true and the second is false.
    let [(x1, y1, w1, h1), (x2, y2, w2, h2)] = splitRegion x y w h s True False
        (rs'   , leftSVGTag)         = mondrian x1 y1 w1 h1 rs
        (rs''  , rightSVGTag)        = mondrian x2 y2 w2 h2 rs'

    in  (rs''  , leftSVGTag ++ rightSVGTag)

   | regionIsTallEnough || okToSplitVertically   =
    let [(x1, y1, w1, h1), (x2, y2, w2, h2)] = splitRegion x y w h t False True
        (rs'   , topSVGTag)          = mondrian x1 y1 w1 h1 rs
        (rs''  , bottomSVGTag)       = mondrian x2 y2 w2 h2 rs'

    in  (rs''  , topSVGTag ++ bottomSVGTag)
   
   | otherwise =
      if r < 0.5 then paintARectangle else paintATriangle

   where
    regionIsWideEnough    = w > round (fromIntegral width / 5.0)
    regionIsTallEnough    = h > round (fromIntegral height / 5.0)

    okToSplitVertically   = (randomInt 50 h r) < h
    okToSplitHorizontally = (randomInt 50 w r) < w

    paintARectangle       = (rs, rectangle x y w h r s t)
    paintATriangle        = (rs, triangle x y w h r s t)

-- This function returns an svg rectangle string for a rectangle. It paints the rectangle using the x,y points and the width and height values.
-- The three random float values are used to paint the rectangle.
rectangle :: Int -> Int -> Int -> Int -> Float -> Float -> Float -> String
rectangle x y w h r s t = 
  "<rect x=" ++ (show x) ++ 
       " y=" ++ (show y) ++ 
       " rx=" ++ (show (randomInt 0 0 r)) ++
       " ry=" ++ (show (randomInt 0 0 s)) ++
       " width=" ++ (show w) ++ 
       " height=" ++ (show h) ++ 
       " stroke=\"black\"" ++
       " stroke-width=\"" ++ (show (stroke x y)) ++ "\"" ++
       " fill=\"" ++ color x y r s t ++ "\" />\n"

-- Fills a triangle region with the appropriate color based on the value of the given random float.
triangle :: Int -> Int -> Int -> Int -> Float -> Float -> Float -> String
triangle x y w h r s t 
   | r < 0.5   = leftToRightCut
   | otherwise = bottomToTopCut

    where
      leftSideColor  = color x y r s t
      rightSideColor = color x y t r s
      bottomLeftY    = y + h
      topRightX      = x + w

   -- To fill the left side of the triangle
      leftToRightCut =
        "<polygon points=\"" ++ (show x) ++ 
        "," ++ (show y) ++ 
        " " ++ (show x) ++
        "," ++ (show bottomLeftY) ++
        " " ++ (show topRightX) ++
        "," ++ (show bottomLeftY) ++
        "\"" ++ 
        " stroke=\"black\"" ++
        " stroke-width=\"" ++ (show (stroke x y)) ++ "\"" ++
        " fill=\"" ++ leftSideColor ++ "\" />\n" ++

        "<polygon points=\"" ++ (show x) ++ 
        "," ++ (show y) ++ 
        " " ++ (show topRightX) ++
        "," ++ (show y) ++
        " " ++ (show topRightX) ++
        "," ++ (show bottomLeftY) ++
        "\"" ++ 
        " stroke=\"black\"" ++
        " stroke-width=\"" ++ (show (stroke x y)) ++ "\"" ++
        " fill=\"" ++ rightSideColor ++ "\" />\n"

      -- To fill the right side of the triangle
      bottomToTopCut =
        "<polygon points=\"" ++ (show x) ++ 
        "," ++ (show bottomLeftY) ++ 
        " " ++ (show topRightX) ++
        "," ++ (show bottomLeftY) ++
        " " ++ (show topRightX) ++
        "," ++ (show y) ++
        "\"" ++ 
        " stroke=\"black\"" ++
        " stroke-width=\"" ++ (show (stroke x y)) ++ "\"" ++
        " fill=\"" ++ leftSideColor ++ "\" />\n" ++

        "<polygon points=\"" ++ (show x) ++ 
        "," ++ (show bottomLeftY) ++ 
        " " ++ (show x) ++
        "," ++ (show y) ++
        " " ++ (show topRightX) ++
        "," ++ (show y) ++
        "\"" ++ 
        " stroke=\"black\"" ++
        " stroke-width=\"" ++ (show (stroke x y)) ++ "\"" ++
        " fill=\"" ++ rightSideColor ++ "\" />\n"

main :: IO ()
main = do
  seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed
  let prefix = "<html><head></head><body>\n" ++
               "<svg width=\"" ++ (show width) ++ 
               "\" height=\"" ++ (show height) ++ "\">"
      image  = snd (mondrian 0 0 width height randomValues)
      suffix = "</svg>\n</html>"

  writeFile "mondrian_expanded.html" (prefix ++ image ++ suffix)