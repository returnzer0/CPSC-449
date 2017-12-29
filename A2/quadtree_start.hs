--
-- CPSC 449 Assignment 2 Starter Code
--
import qualified Data.ByteString.Lazy as BS
import Data.Word
import Data.Bits
import Data.Char
import Codec.Compression.Zlib as Z
import Numeric (showHex)

--
-- Define your algebraic types for Part 1 here
--
data QuadTree  = Node QuadTree QuadTree QuadTree QuadTree | Fill (Int, Int, Int) deriving (Show)
data ImageTree = Image Int QuadTree deriving (Show)

--
-- The following functions are a simple PNG file loader.  Note that these
-- functions will not load all PNG files.  They makes some assumptions about
-- the structure of the file that are not required by the PNG standard.
--

--
-- Convert 4 8-bit words to a 32 bit (or larger) integer
--
make32Int :: Word8 -> Word8 -> Word8 -> Word8 -> Int 
make32Int a b c d = ((((fromIntegral a) * 256) + 
                       (fromIntegral b) * 256) + 
                       (fromIntegral c) * 256) + 
                       (fromIntegral d)

--
-- Get a list of all of the PNG blocks out of a list of bytes
--
getBlocks :: [Word8] -> [(String, [Word8])]
getBlocks [] = []
getBlocks (a:b:c:d:e:f:g:h:xs) = (name, take (size+12) (a:b:c:d:e:f:g:h:xs)) : getBlocks (drop (size + 4) xs)
  where
    size = make32Int a b c d
    name = (chr (fromIntegral e)) : (chr (fromIntegral f)) :
           (chr (fromIntegral g)) : (chr (fromIntegral h)) : []

--
-- Extract the information out of the IHDR block
--
getIHDRInfo :: [(String, [Word8])] -> (Int, Int, Int, Int)
getIHDRInfo [] = error "No IHDR block found"
getIHDRInfo (("IHDR", (_:_:_:_:_:_:_:_:w1:w2:w3:w4:h1:h2:h3:h4:bd:ct:_)) : _) = (make32Int w1 w2 w3 w4, make32Int h1 h2 h3 h4, fromIntegral bd, fromIntegral ct)
getIHDRInfo (x : xs) = getIHDRInfo xs

--
-- Extract and decompress the data in the IDAT block.  Note that this function
-- only handles a single IDAT block, but the PNG standard permits multiple
-- IDAT blocks.
--
getImageData :: [(String, [Word8])] -> [Word8]
getImageData [] = error "No IDAT block found"
getImageData (("IDAT", (_:_:_:_:_:_:_:_:xs)) : _) = BS.unpack (Z.decompress (BS.pack (take (length xs - 4) xs)))
getImageData (x:xs) = getImageData xs

--
-- Convert a list of bytes to a list of color tuples
--
makeTuples :: [Word8] -> [(Int, Int, Int)]
makeTuples [] = []
makeTuples (x : y : z : vals) = (fromIntegral x, fromIntegral y, fromIntegral z) : makeTuples vals

--
-- Convert a list of bytes that have been decompressed from a PNG file into
-- a two dimensional list representation of the image
--
imageBytesToImageList :: [Word8] -> Int -> [[(Int, Int, Int)]]
imageBytesToImageList [] _ = []
imageBytesToImageList (_:xs) w = makeTuples (take (w * 3) xs) : imageBytesToImageList (drop (w * 3) xs) w

--
-- Determine how many IDAT blocks are in the PNG file
--
numIDAT :: [(String, [Word8])] -> Int
numIDAT vals = length (filter (\(name, dat) -> name == "IDAT") vals)

--
-- Convert the entire contents of a PNG file (as a ByteString) into 
-- a two dimensional list representation of the image
--
decodeImage :: BS.ByteString -> [[(Int, Int, Int)]]
decodeImage bytes
  | header == [137,80,78,71,13,10,26,10] &&
    colorType == 2 &&
    bitDepth == 8 = imageBytesToImageList imageBytes w
  | numIDAT blocks > 1 = error "The image contained too many IDAT blocks"
  | otherwise = error ("Invalid header\ncolorType: " ++ (show colorType) ++ "\nbitDepth: " ++ (show bitDepth) ++ "\n")
  where
    header = take 8 $ BS.unpack bytes
    (w, h, bitDepth, colorType) = getIHDRInfo blocks
    imageBytes = getImageData blocks
    blocks = getBlocks (drop 8 $ BS.unpack bytes)

--
-- Insert your code here for Parts 2, 3 and 4 here
--
-- Part 2:
createTree :: [[(Int, Int, Int)]] -> ImageTree
createTree list
   | height /= width = error "Input is not square"
   | otherwise = Image height (createQTree list)
   where
    height = length list
    width  = length (head list)

-- This function acts as a helper, allows us to use recursion without having to worry about dealing with the width/height.
createQTree :: [[(Int, Int, Int)]] -> QuadTree
createQTree list
   | isHomogenous = Fill (head (head list))
   | otherwise    = Node (createQTree topLeft) (createQTree topRight) (createQTree bottomLeft) (createQTree bottomRight)
   where
    (topLeft, topRight, bottomLeft, bottomRight) = divideEqually list
    -- We flatten the 2D list into a 1D list 
    flattenedList = [y | x <- list, y <- x]
    isHomogenous  = and $ map (== head flattenedList) (tail flattenedList)

-- This function divides up each image represenation of type a into a tuple of (a,a,a,a), each representing a specific side of the Image.
divideEqually :: [[(Int, Int, Int)]] -> ([[(Int, Int, Int)]], [[(Int, Int, Int)]], [[(Int, Int, Int)]], [[(Int, Int, Int)]])
divideEqually list = (topLeft, topRight, bottomLeft, bottomRight)
  where 
    size        = length list
    half        = size `div` 2
    top         = take half list
    bottom      = drop half list
    topLeft     = [take half x | x <- top ]
    topRight    = [drop half x | x <- top ]
    bottomLeft  = [take half x | x <- bottom]
    bottomRight = [drop half x | x <- bottom]

-- Part 3

treeMap :: (QuadTree -> QuadTree) -> QuadTree -> QuadTree
treeMap f (Fill (a, b, c)) = f (Fill (a, b, c))
treeMap f (Node p q r s)   = f (Node (treeMap f p) (treeMap f q) (treeMap f r) (treeMap f s))

-- Mirror function
mirror :: ImageTree -> ImageTree
mirror (Image size node) = (Image size (treeMap mirrorHelper node))

-- Flipping the nodes, dependant on the constructor.
mirrorHelper :: QuadTree -> QuadTree
mirrorHelper (Fill (a, b, c)) = (Fill (a, b, c))
mirrorHelper (Node p q r s)   = (Node q p s r)

-- Grey Scale
grayscale :: ImageTree -> ImageTree
grayscale (Image size node) = (Image size (treeMap grayscaleHelper node))

-- Averaging the rgb values. I tried to make a local variable for the average but it kept giving me an error, even after adjusting the indentation.
grayscaleHelper :: QuadTree -> QuadTree
grayscaleHelper (Fill (a, b, c)) = (Fill ((a+b+c) `div` 3, (a+b+c) `div` 3, (a+b+c) `div` 3))
grayscaleHelper (Node p q r s)   = (Node p q r s)
   -- where
   --  avg = a + b + c

-- Part 4
-- We only create the prefix and the suffix in this function, the nodes are visited and their tages are created in the helper function.
toHTML :: ImageTree -> String
toHTML (Image size q) = "<html><head></head><body>\n" ++
                     "<svg width=\"" ++ (show size) ++ 
                     "\" height=\"" ++ (show size) ++ "\">" ++
                     (toHTMLHelper 0 0 size q) ++
                     "</svg>\n</html>"

toHTMLHelper :: Int -> Int -> Int -> QuadTree -> String
toHTMLHelper x y size (Fill (a, b, c)) = "<rect x=" ++ (show x) ++ 
                                          " y=" ++ (show y) ++ 
                                          " width=" ++ (show size) ++ 
                                          " height=" ++ (show size) ++ 
                                          " stroke=\"black\"" ++
                                          " stroke-width=\"" ++ (show 0) ++ "\"" ++
                                          " fill=\"rgb(" ++ 
                                          (show a) ++ "," ++
                                          (show b) ++ "," ++
                                          (show c) ++ ")\" />\n"

toHTMLHelper x y size (Node a b c d)  = topLeft ++ topRight ++ bottomLeft ++ bottomRight
   where
    -- Using the correct offsets for each part of the Image.
    newWidth    = size `div` 2
    topLeft     = toHTMLHelper  x              y             newWidth a
    topRight    = toHTMLHelper (x + newWidth)  y             newWidth b
    bottomLeft  = toHTMLHelper  x             (y + newWidth) newWidth c
    bottomRight = toHTMLHelper (x + newWidth) (y + newWidth) newWidth d
   

--
-- Load a PNG file, convert it to a quad tree, mirror it, grayscale it,
-- and write all three images to an .html file.
--
main :: IO ()
main = do
  -- Change the name inside double quotes to load a different file
  -- Change the name inside double quotes to load a different file
  input <- BS.readFile "Mondrian.png"
  -- input <- BS.readFile "Test_2x2.png"
  -- input <- BS.readFile "Test_512x512.png"
  -- input <- BS.readFile "ThisSideUp.png"
  -- input <- BS.readFile "TwoSquares.png"
  -- input <- BS.readFile "Test_Rectangle_1.png"
  -- input <- BS.readFile "Test_Rectangle_2.png"
  -- input <- BS.readFile "cross_15_15.png"
  -- input <- BS.readFile "cross_33_17.png"
 
  -- image is the list representation of the image stored in the .png file
  let image = decodeImage input

  -- Convert the list representation of the image into a tree representation
  let qtree_image = createTree image

  -- print qtree_image
  -- Gray scale the tree representation of the image
  let gs = grayscale qtree_image

  -- Mirror the tree representation of the image
  let mi = mirror qtree_image
 
  -- Write the original, mirrored and grayscale images to quadtree.html
  -- writeFile "quadtree.html" "" -- take this line out and use the lines below 
  --                              -- instead once you have your functions written

  writeFile "quadtree.html" ((toHTML qtree_image) ++ "<br><br><br>" ++ 
                            (toHTML gs) ++ "<br><br><br>" ++
                            (toHTML mi) ++ "<br><br><br>")