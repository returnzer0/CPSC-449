getSequence :: Integer -> [Integer]
getSequence n
  | n > 1 = [n] ++ getSequence newVal
  | otherwise = [1]
   where
   newVal = if (n `mod` 2 == 0) then (n `div` 2) else (3 * n + 1)


-- findSequences :: Integer -> Integer -> [Integer]
-- findSequences numOfSequences lengthOfSequence = 