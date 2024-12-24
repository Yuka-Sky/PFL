-- 3.1
-- f1 :: (a -> b) -> (a -> Bool) -> p -> [b]
-- f1 f p xs = map f (filter p xs)

-- 3.2 foldl aplica da esquerda p direita
dec2int :: [Int] -> Int
-- dec2int f b [] = b
-- dec2int f b (x:xs) = foldl f (f b x) xs
dec2int xs = foldl (\x y -> 10*x+y) 0 xs

-- 3.3
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- 3.4
insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (x:xs)
    | x >= a = a : x : xs
    | otherwise = x : insert' a xs

isort' :: Ord a => [a] -> [a]
isort' = foldr insert' []


-- 3.7
-- a
(++++) :: [a] -> [a] -> [a]
xs ++++ ys = foldr (:) xs ys

-- b
concat' :: [[a]] -> [a]
concat' = foldr (++) []

-- c
reverse' :: [a] -> [a]
reverse' = foldr (\h acc -> h : acc) []

-- d
reverse'' :: [a] -> [a]
reverse'' = foldl (\acc h -> h : acc) []

-- e
--elem' :: Eqa a => [a] -> Bool
--elem' 
