module Simulado where
-- \x -> reverse [x] ++ [x]
verificaSoma :: [Int] -> Bool
verificaSoma x = mod (foldl (+) 0 x) 2 == 0

prova :: [Int] -> Bool
prova x = mod (length x) 2 == 0 && length x > 7

-- (\x -> min x 0)


class Functor f where
    fmap :: (a->b) -> f a -> f b
    
data Gf4 = U | A | B deriving Show

instance Monoid (Gf4 a)  where
    mempty = u
    mapend u x = x
    