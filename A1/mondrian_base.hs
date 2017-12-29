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

red :: String
red = "rgb(255,0,0)"

yellow :: String
yellow = "rgb(255,255,0)"

skyblue :: String
skyblue = "rgb(0,255,255)"

white :: String
white = "rgb(255,255,255)"

randomList :: Int -> [Float]
randomList seed = take 20000 (rl_helper (mkStdGen seed))

rl_helper :: StdGen -> [Float]
rl_helper g = fst vg : rl_helper (snd vg)
  where vg = randomR (0.0, 1.0 :: Float) g

randomInt :: Int -> Int -> Float -> Int
randomInt low high x = round ((fromIntegral (high - low) * x) + fromIntegral low)

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
      splitPointH = randomInt (round ((1 / 3) * fromIntegral w)) (round ((2 / 3) * fromIntegral w)) r
      splitPointV = randomInt (round ((1 / 3) * fromIntegral h)) (round ((2 / 3) * fromIntegral h)) r
      
      topLeft     = (              x,               y,       splitPointH,       splitPointV)
      topRight    = (x + splitPointH,               y, (w - splitPointH),       splitPointV)
      bottomLeft  = (              x, splitPointV + y,       splitPointH, (h - splitPointV))
      bottomRight = (splitPointH + x, splitPointV + y, (w - splitPointH), (h - splitPointV))

      left        = (              x,               y,       splitPointH,                 h)
      right       = (splitPointH + x,               y, (w - splitPointH),                 h)

      top         = (              x,               y,                 w,       splitPointV)
      bottom      = (              x, splitPointV + y,                 w, (h - splitPointV))

-- The main mondrian function. Requires the x,y coordinates of the top left of the region along with the width and height and an array of floats.
-- It will perform checks at each iteration and then call the splitRegion function if/when necessary. The result will then recursively call itself.
-- In the end case, we will simply paint the region with a random color.
mondrian :: Int -> Int -> Int -> Int -> [Float] -> ([Float], String)
mondrian x y w h (r:s:t:rs)
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
      (rs, fillAreaWithColor x y w h t)

   where
    -- Below are the required boolean checks to see whether a given region can be further divided or not.
    regionIsWideEnough    = w > round (fromIntegral width / 2.0)
    regionIsTallEnough    = h > round (fromIntegral height / 2.0)

    okToSplitVertically   = (randomInt 120 (round(fromIntegral h * 1.5)) r) < h
    okToSplitHorizontally = (randomInt 120 (round(fromIntegral w * 1.5)) r) < w

-- Fills a given region with the appropriate color based on the value of the random float.
fillAreaWithColor :: Int -> Int -> Int -> Int -> Float -> String
fillAreaWithColor x y w h r = 
  "<rect x=" ++ (show x) ++ 
       " y=" ++ (show y) ++ 
       " width=" ++ (show w) ++ 
       " height=" ++ (show h) ++ 
       " stroke=\"black\"" ++
       " stroke-width=\"" ++ (show stroke) ++ "\"" ++
       " fill=\"" ++ color ++ "\" />\n"
    where
      color  = if r < 0.0833 then red else (if r < 0.1667 then skyblue else (if r < 0.25 then yellow else white))
      -- We keep the stroke constant because its more aesthetic.
      stroke = 2

main :: IO ()
main = do
  seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed
  let prefix = "<html><head></head><body>\n" ++
               "<svg width=\"" ++ (show width) ++ 
               "\" height=\"" ++ (show height) ++ "\">"
      image  = snd (mondrian 0 0 width height randomValues)
      suffix = "</svg>\n</html>"

  writeFile "mondrian_base.html" (prefix ++ image ++ suffix)