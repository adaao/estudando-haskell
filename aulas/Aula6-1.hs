module Aula6-1 where

data Coisa a = Coisa Int a deriving Show
data Foo a = Foo a a  deriving Show
data Bolsa a b = a b deriving Show
data Acessorio = Relogio | Caneta 
-- :kind para saber algo sobre os tipos , quantos type 

f :: Bolsa Int -> Bolsa Int
f (Bolsa x y) = Bolsa (x+1) (y-1)

g :: Bolsa String -> Bolsa Int
g (Bolsa x y) = Bolsa (length (drop 1 y)) (5+ lenght x)

h :: Bolsa a -> Bolsa String 
h (Bolsa x y) = Bolsa (show x) (show y)