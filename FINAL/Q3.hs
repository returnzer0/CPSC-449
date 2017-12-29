data Eon = Hadean | Archean | Proterozoic | Phanerozoic deriving (Eq, Ord, Bounded, Enum)

instance Show (Eon) where
 show Hadean =  "Hadean (4.6 Billion to 4.0 Billion year ago)"
 show Archean = "Archean (4.0 Billion to 2.5 Billion year ago)"
 show Proterozoic = "Proterozoic (2.5 Billion to 541 Million years ago)"
 show Phanerozoic = "Phanerozoic (541 Million years to present)"


yearToEon :: Integer -> String
yearToEon year
  | year >= (-460000000) && year <= (-400000000) = show Hadean
  | year >= (-4000000000) && year <=  (-2500000000 ) = show Archean
  | year >= (-2500000000) && year <=  (-541000000 ) = show Proterozoic
  | year >= (-541000000) && year <=  2017 = show Phanerozoic
  | otherwise = error "wrong year"


