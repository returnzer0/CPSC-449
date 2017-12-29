

percentLookup :: [a] -> Float -> a
percentLookup [] _  = error "List can't be empty"
percentLookup xs p
   | p < 0 = head xs
   | p > 100 =  xs !! (xsLen - 1)
   | otherwise = elementToReturn
   
    where
      xsLen = length xs
      elementToReturn = xs !! round (((p / 100.0) * (fromIntegral (xsLen - 1))))
-- values !! floor (((p / 100.0) * (fromIntegral xsLen)))