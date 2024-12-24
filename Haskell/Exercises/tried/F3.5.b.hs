foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f xs = foldl f (head xs)(tail xs) 