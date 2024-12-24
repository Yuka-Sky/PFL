data Arv a = Vazia | Node a (Arv a) (Arv a)
    deriving (Show)

exemplo :: Arv Int
exemplo = (Node 5 (Node 2 Vazia (Node 3 Vazia Vazia)) Vazia)

-- 4.1
sumArv :: Num a => Arv a -> a
sumArv Vazia = 0
sumArv (Node valor esq dir) = valor + sumArv esq + sumArv dir

-- 4.2
listar :: Arv a -> [a] 
listar Vazia = []
listar (Node valor esq dir) = listar dir ++ [valor] ++ listar esq
-- OU mete em uma lista e depois organiza

-- 4.3
nivel :: Int -> Arv a -> [a]
nivel 0 (Node valor _ _) = [valor]
nivel andar (Node valor esq dir) = nivel (andar-1) esq ++ nivel (andar-1) dir

-- 4.5
fun :: Int -> Int
fun a = a*2

mapArv :: (a -> b) -> Arv a -> Arv b
mapArv _ Vazia = Vazia
mapArv f (Node valor esq dir) = Node (f valor) (mapArv f esq) (mapArv f dir)

-- 4.6
-- a
maisDir :: Arv a -> a
maisDir (Node valor _ Vazia) = valor
maisDir (Node valor esq dir) = maisDir dir

-- b
remover :: Ord a => a -> Arv a -> Arv a
remover x Vazia = Vazia
remover x (Node y Vazia dir)
    | x==y = dir
remover x (Node y esq Vazia)
    | x==y = esq
remover x (Node y esq dir)
    | x>y = Node y (remover x esq) dir
    | x<y = Node y esq (remover x dir)
    | x==y = let z = maisDir dir
        in Node z (remover z esq) dir

-- 4.7
data Expr = Lit Integer | Op Ops Expr Expr | If BExp Expr Expr

data Ops = Add | Sub | Mul | Div
    deriving (Show)

data BExp = BoolLit Bool | And BExp BExp | Not BExp | Equal Expr Expr | Greater Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Op Add esq dir) = (eval esq) + (eval dir)
eval (Op Sub esq dir) = (eval esq) - (eval dir)
eval (Op Mul esq dir) = (eval esq) * (eval dir)
eval (Op Div esq dir) = (eval esq) `div` (eval dir)

bEval :: BExp -> Bool 
bEval (BoolLit n) = n 
bEval (And esq dir)     = (bEval esq) && (bEval dir)
bEval (Not esq)         = not $ bEval esq
bEval (Equal esq dir)   = (bEval esq) == (bEval dir)
bEval (Greater esq dir) = (bEval esq) >  (bEval dir)


instance Show Expr where
    show (Lit n) = show n
    show (Op Add esq dir) = show esq ++ " + " ++ show dir
    show (Op Sub esq dir) = show esq ++ " - " ++ show dir
    show (Op Mul esq dir) = show esq ++ " * " ++ show dir
    show (Op Div esq dir) = show esq ++ " / " ++ show dir
    show (If cond e1 e2) = "if " ++ show cond ++ " then " ++ show e1 ++ " else " ++ show e2

instance Show BExp where
    show (BoolLit n) = show n 
    show (And esq dir) =    show esq ++ " && " ++ show dir
    show (Not esq) =    show esq
    show (Equal esq dir) =  show esq ++ " == " ++ show dir
    show (Greater esq dir) = show esq ++ " > " ++ show dir

size :: Expr -> Integer
size (Lit n) = 1
size (Op _ esq dir) = size esq + size dir

