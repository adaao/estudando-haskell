module Testes where

import Control.Monad

infixl 0 |>

-- (|>) :: a -> (a -> b) -> b
-- (|>) x f = f x 
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

expr2 :: String
expr2 = "HASKELL" 
      |> \x -> reverse x 
      |> \y -> (head x) : (tail y)

expr3 :: Maybe String
expr3 =  Just "HASKELL" >>= \x -> Just (reverse x) >>= \y -> Just((head x) : (tail y))

expr3' :: Maybe String
expr3' = do
    x <- Just "HASKELL"  
    y <- Just (reverse x)  
    return ((head x) : (tail y))