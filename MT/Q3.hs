-- pathLength :: [(Float, Float)] -> Float
-- pathLength [] = 0.0
-- pathLength (x: v: xs) = distance x v + pathLength xs

-- -- Sorry i ran out of time... so i started doing recursion

-- distance :: (Float, Float) -> (Float, Float) -> Float
-- distance (x1, y1) (x2, y2) = sqrt ((x2-x1)**2 + (y2-y1) **2)



-- Solutions
pathHelper :: ((Float, Float), Float) -> (Float, Float) -> ((Float, Float), Float)
pathHelper ((x1, y1), soFar) (x2,y2) = ((x2, y2), soFar + distance)
   where
   	distance = sqrt ((x1-x2) ** 2 + (y1-y2) ** 2)


pathLength :: [(Float, Float)] -> Float
pathLength [] = 0
pathLength coords = snd ( foldl pathHelper (head coords, 0) (tail coords))























































