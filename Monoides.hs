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
    
    

  
  
  
  
  