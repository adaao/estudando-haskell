module Hack where

foo :: Int -> Bool
foo ano = (\ano -> ((mod ano 4 == 0) && (mod ano 100 /= 0)) || (mod ano 400 == 0)) $ ano