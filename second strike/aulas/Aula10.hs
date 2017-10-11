module Aula10 where

-- Qual o kind? * -> *
-- Eh polimorfico? Sim.
-- Quantos VC? 1
-- Quantos campos tem o VC? 1
-- Qual o tipo do campo? a
data Id a = Id a deriving Show

instance Functor Id where
    fmap ab (Id a) = Id (ab a)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

-- Polimorfica? Sim, recebe um parâmetro variável
-- [a] == [] a
-- f :: Id a -> [a] 
-- TRANSFORMACAO NATURAL: EH UMA FUNCAO QUE "TROCA"
-- FUNTOR E NAO POSSUI RESTRICAO ALGUMA.
-- f :: Id -> []
-- TEM A FORMA GENERICA: 
-- f :: (Functor F, Functor G) => F a -> G a 
-- ONDE F eh uma instancia de Functor e G tb.
f :: Id a -> [] a 
f (Id x) = [x]

f' :: a -> [] a
f' x = [x]

-- F = Id, G = []
g :: a -> [] a 
g x = [x]

-- F = [], G = Id
h :: [] a -> Id a 
h xs = Id (head xs)

-- F = [], G = Maybe
h2 :: [] a -> Maybe a 
h2 [] = Nothing
h2 xs = Just (head xs)