module Aux where
    
import Control.Monad

foo :: Int -> Int
foo x = (*3) (+100) $ x