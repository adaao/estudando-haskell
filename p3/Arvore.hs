module Arvore where

data Arvore a = Nulo | Leaf a | Branch a (Arvore a) (Arvore a) deriving Show

arvore :: Arvore Int
arvore = Branch 50 (Branch 30 (Leaf 20) (Leaf 40)) (Branch 90 Nulo (Leaf 100))

-- emOrdem :: Arvore Int -> [Int]
emOrdem (Branch x l r) = emOrdem l ++ [x] ++ emOrdem r
emOrdem (Leaf x) = x
emOrdem Nulo = [] 