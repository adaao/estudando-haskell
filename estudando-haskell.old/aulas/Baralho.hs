module Baralho where

data Naipe = Copas | Ouros | Espadas | Paus deriving (Eq, Show)

--data Valor = A | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | J | Q | K deriving Show

data Valor = A |Dois|Tres|Quatro|Cinco|Seis|Sete|Oito|Nove|Dez| J | Q | K deriving (Eq, Show)

data Carta = Carta {valor::Valor, naipe::Naipe} deriving (Eq, Show)

fazCarta :: Valor -> Naipe -> Carta
fazCarta v n = Carta v n

retornaValor :: Carta -> Valor
retornaValor (Carta v n) = v

retornaNaipe :: Carta -> Naipe
retornaNaipe (Carta v n) = n

c = [Carta K Ouros, Carta Dois Paus, Carta Dez Copas, Carta Tres Espadas]

filtraRed :: [Carta] -> [Carta]
filtraRed x = [y | y <- x, isVermelho y /= True]


isVermelho :: Carta -> Bool
isVermelho (Carta v Copas) = True
isVermelho (Carta v Ouros) = True
isVermelho (Carta v _ ) = False