main :: IO ()
main = do
    putStrLn "1.1 Triangulo existe: "
    print (tt 3 4 5)
    putStrLn "1.2 Area do triangulo: "
    print (areaTriang 3 4 5)
    putStrLn "Lista dividida: "
    print (metade [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    putStrLn "Ultimo: "
    print (last1 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    putStrLn "Primeiro: "
    print (init1 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])

--1.1
tt :: Float -> Float -> Float -> Bool
tt a b c = a < (b+c) && b < (a+c) && c < (a+b)

--1.2
s :: Float -> Float -> Float -> Float
s a b c = (a+b+c)/2
areaTriang:: Float -> Float -> Float -> Float
areaTriang a b c = s a b c *(s a b c - a)*(s a b c - b)*(s a b c -c)

--1.3
-- half :: [a] -> Int
-- half lista = length lista `div` 2
metade :: [a] -> ([a], [a])
metade lista = (take half lista, drop half lista)
    where half = length lista `div` 2

--1.4
last1 :: [a] -> a
last1 lista = head (reverse lista)
last2 :: [a] -> [a]
last2 lista = drop ((length lista)-1) lista

init1 :: [a] -> [a]
init1 lista = take ((length lista)-1) lista

--1.5
