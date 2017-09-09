module Aula6 where

data Status = Magro | Gordo | Saudavel deriving Show

elimVogal :: String -> String
elimVogal [] = []
elimVogal (x:xs)
    | elem x "AEIOUaeiou" = elimVogal xs
    | otherwise = x : elimVogal xs
    
imc :: Double -> Double -> Status
imc peso altura
    | calc <= 18 = Magro
    | calc <= 25 = Saudavel
    | otherwise = Gordo
        where calc = peso / (altura*altura)

-- Polimorfismo parametrico ->>>> VER :KIND
data Bolsa a b = Nada | UmItem a | DoisItens a b deriving Show

-- phantom type -> tem um parametro que nao e usado

-- TIPO FNTASMA -> ghost type
data Metros a = Metros Double deriving Show

data Um

data Dois 

areaQuadrado :: Metros Um -> Metros Dois
areaQuadrado (Metros m) = Metros (m*m)
