{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use splitAt" #-}
testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c = c < a+b && a < b+c && b < a+c

s :: Float -> Float -> Float -> Float
s a b c = (a+b+c)/2

areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt (s a b c*(s a b c - a)*(s a b c - b)*(s a b c - c))

half :: [a] -> Int
half lista = div (length lista) 2

metades :: [a] -> ([a], [a])
metades lista = (take (half lista) lista, drop (half lista) lista)

last1 :: [a] -> [a]
last1 lista = drop ((length lista)-1) lista

raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = ((-b + sqrt del) / (2*a), (-b - sqrt del) / (2*a))
    where
        del = b*b - 4*a*c

classifica :: Int -> String
classifica n = "O número é " ++ cond
    where 
        cond
            | n <= 9     = "reprovado"
            | n <= 11    = "suficiente"
            | n <= 14    = "bom"
            | n <= 17    = "muito bom"
            | n <= 20    = "muito bom com distinção"
            | otherwise  = "foda lek"

