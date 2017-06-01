module Monoides where

import Data.Monoid

data Valor = Zero | Um | Dois | Und deriving Show

instance Monoid Valor where
    mempty = Zero
--    mappend x Zero = x
--    mappend Zero x = x
    mappend Um Um = Dois
    mappend _ _ = Und