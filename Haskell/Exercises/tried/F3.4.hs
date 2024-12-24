isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = foldr x (isort xs) 0