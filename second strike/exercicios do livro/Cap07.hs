module Cap07 where

import Data.Monoid
import Data.Functor

data Fantasma a = Fantasma a deriving Show

instance Functor Fantasma where
  fmap g (Fantasma a) = Fantasma (g a)

--------------------------------------------------------------



data Min = Min Int deriving (Show, Eq, Ord)

instance Monoid Min where
    mempty = Min (maxBound :: Int)
    mappend (Min x) (Min y) = Min $ min x y

---------------------------------------------------------------

data NovoPred a = NovoPred {runNovoPred :: Maybe a -> Bool} 

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a


instance Contravariant NovoPred where
  contramap func (NovoPred pred) = NovoPred (pred.(fmap func))
  
-------------------------------------------------------------
