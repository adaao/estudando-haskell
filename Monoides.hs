module Monoides where

import Data.Monoid

data Valor = Zero | Um | Dois | Und deriving Show

instance Monoid Valor where
    mempty = Zero
--    mappend x Zero = x
--    mappend Zero x = x
    mappend Um Um = Dois
    mappend _ _ = Und
    
    
    
data Min = Min Int deriving (Show, Eq, Ord)

instance Monoid Min where
    mempty = Min (maxBound :: Int)
    mappend (Min x) (Min y) = Min $ min x y
    
    
-- import Data.Functor.Contravariant

data NovoPred a = NovoPred {runNovoPred :: Maybe a -> Bool} 

instance Contravariant NovoPred where

{-
Procurei a typeclass Contravariant para terminar o exercício e não consigo importar.
O Haskell diz que não existe.
De todo jeito, seguindo os exemplos do livro, o resto do código seria: -}

instance Contravariant NovoPred where
  contramap func (NovoPred pred) = NovoPred (pred . func)
  
  
  
  
  
  