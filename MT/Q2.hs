

wci :: Float -> Float -> Float
wci temp speed
   | temp >= 10 || speed < 4.8 = temp
   | otherwise = windChillIndex
    where
     windChillIndex = 13.12 + 0.6215 * temp - 11.37 * speed ** 0.16 + 0.3965 * temp * speed ** 0.16


windChills :: [(Float, Float)] -> [Float]
windChills xs = [ wci temp speed | (temp, speed) <- xs]

-- windChills xs = map (uncurry wci) values