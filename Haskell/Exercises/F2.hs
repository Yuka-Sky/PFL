main :: IO ()
main = do
    putStrLn "1.1 Triangulo existe: "
    -- print (tt 3 4 5)
    putStrLn "1.2 Area do triangulo: "

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs 

or' :: [Bool] -> Bool
or' [] = True
or' (x:xs) = x || or' xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' n x
    | n < 0 = []
    | otherwise = x : replicate' (n-1) x

arroz :: [a] -> Int -> a
arroz (x:xs) n
    | n == 0 = x
    | n > 0 = arroz xs (n-1)
    | otherwise = error "Erro"

elem' :: Eq a => a -> [a] -> Bool
elem' gato (x:xs)
    | gato == x = True
    | otherwise = elem' gato xs

intersperse' :: a -> [a] -> [a]
intersperse' elem [] = []
intersperse' elem (x:xs) = x : elem : intersperse' elem xs

mdc' :: Integer -> Integer -> Integer
mdc' a b
    | b == 0 = a
    | otherwise = mdc' b (a `mod` b)

insert' :: Ord a => a -> [a] -> [a]
insert' a [] = [a]
insert' a (x:xs)
    | x >= a = a : x : xs
    | otherwise = x : insert' a xs

isort' :: Ord a => [a] -> [a]
isort' [] = []
isort' (x:xs) = insert' x (isort' xs)

min' :: Ord a => [a] -> a
min' batata = head (isort' batata)

delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' del (x:xs)
    | x == del = xs
    | otherwise = x : delete' del xs

ssort' :: Ord a => [a] -> [a]
ssort' lista 
    | lista == [] = []
    | otherwise = min' lista ++ ssort'(delete' (min' lista) lista)

sumQua :: [a] -> a
sumQua [] = 0
sumQua (x:xs) = x^2 + sumQua xs

aprox' :: Int -> Double
aprox' 0 = 1
aprox' n = ((-1)^n)/((n + 1)^2) + aprox'(n-1)

banana :: [FLoat] -> [Float] -> [Float]
banana (x:_) (y:_) = [(xy)]

dotprod :: [Float] -> [Float] -> Float
dotprod (x:xs) (y:ys) = [w| ]
