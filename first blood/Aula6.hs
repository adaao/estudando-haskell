module Aula6 where 

fat :: Int -> Int
fat n = foldl (\ffat vi -> fat * vi) 1 [1..n]

-- FACA UMA FUNCAO QUE RECEBA UM VETOR DE STRINGS 
-- E RETORNE UM INTEIRO QUE REPRESENTA A SOMA DOS TAMANHOS
-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- a = Strings
-- b= Int

somarTamanhos :: [String] -> Int
somarTamanhos ls = foldl (\soma l -> soma + length l) 0 ls

--acumulador
dentro :: Int -> String -> Int
dentro soma l =soma + length l

--foldl
contaPar ::  [Int] -> Int
contaPar ls = foldl checaPar 0 ls

--contador
checaPar :: Int -> Int ->Int
checaPar  cont l
     |even l = cont + 1
     |otherwise = cont 