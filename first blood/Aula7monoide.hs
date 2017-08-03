module Aula7 where

import Data.Monoid

data Bolsa a = Nada | UmaCoisa a deriving Show

instance (Monoid a) => Monoid (Bolsa a) where
    mempty = Nada
    mappend Nada x = x
    mappend x Nada = x
    mappend (UmaCoisa x) (UmaCoisa y) = UmaCoisa (x <> y)
    
--goku ++ vegeta = gogeta

data Binario = Zero | Um deriving Show

-- QUANDO DEFINIMOS O mappend GANHAMOS DE GRACA o <>
instance Monoid Binario where
    mempty = Zero
    
    mappend Zero Zero = Zero
    mappend Um Zero = Um
    mappend Zero Um = Um
    mappend Um Um = Um
    
--nao esquecer mconcat
--ver tipo mconcat

