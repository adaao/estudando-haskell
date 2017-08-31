module Aula5 where

-- HIGH-ORDER FUNCTION: EH UMA FUNCAO QUE:
-- a) RECEBE VIA PARAMETRO UMA FUNCAO E/OU
-- b) RETORNA UMA FUNCAO

-- map joga uma função em uma lista
-- filter fltra os elementos de uma lista de acordo com um predicado (a->Bool)
-- 

h :: Int -> (Int -> Int)
h x = \y -> x*y

g :: (Int -> Int) -> Int
g f = f 1

foo :: Int -> (Int -> Int) -> Int
foo x f = x + f 1

dobro :: Int -> Int
dobro x = 2*x

data Correncia = Euro | Dollar | Real deriving Show

data Dinheiro = Dinheiro {valor :: Double,
                          curr :: Correncia
                         } deriving Show

somar :: Int -> Int -> Int -> Int
somar x y z = x+y+z

ehPrimo :: Int -> Bool
ehPrimo n = filter (\x -> mod n x == 0) [1 .. n-1] == [1]


tamanho :: String -> Int
tamanho = length 

ehPrimo :: Int -> Bool
ehPrimo n = filter (\x -> mod n x == 0) [1 .. n-1] == [1]

fat :: Int -> Int
fat n = foldl (*) 1 [1 .. n]


-- OPERADOR, OU FUNCAO INFIXA
infixl 0 |>

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

foo :: Int -> Int
foo n = n |> \x -> 2*x |> \y -> y+x+1 



