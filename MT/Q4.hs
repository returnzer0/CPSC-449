-- limits the number of items that are removed from a list.

filterLimit :: (a -> Bool) -> Int -> [a] -> [a]
filterLimit _ _ []         = []
filterLimit _ 0 xs         = xs
filterLimit f limit (x:xs) = if f x then x : filterLimit f limit xs else filterLimit f (limit - 1) xs