import Data.List (intercalate)

myand :: [Bool]->Bool
myand xs = foldr (&&) True xs

myor :: [Bool]->Bool
myor xs = foldr (||) True xs

myconcat :: [[a]] -> [a]
myconcat xs = foldr (++) [] xs
{-
myand :: [Bool]->Bool
myand [] = True
myand (x:xs) = x && myand xs

myor :: [Bool] -> Bool
myor [] = True
myor (x:xs) = x || myor xs

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ concat xs
-}

rep :: Int -> a -> [a]
rep n elem
  | n <= 0    = []
  | otherwise = elem : rep (n-1) elem


myintersperse :: a -> [a] -> [a]
myintersperse _ [] = []
myintersperse _ [y] = y : [y]
myintersperse x (y:ys) = y : x : myintersperse x ys

insert' :: Ord a => a -> [a] -> [a]
insert' _ [] = []
insert' a (x:xs)
    | x <= a = x : a : xs 
    | otherwise = x : insert' a xs

isort' :: Ord a => [a] -> [a]
isort' [] = []
isort' (x:xs) = insert' x (isort' xs)


sumQuad:: Integer
sumQuad = sum[x^2 | x <- [1..100]]
-- OU
sum' :: Int -> Int
sum' n = helper n 0
    where
        helper :: Int -> Int -> Int
        helper n acc
            | n <= 0 = acc
            | otherwise = helper (n-1) (n^2+acc)