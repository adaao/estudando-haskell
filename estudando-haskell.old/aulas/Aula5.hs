module Aula5 where

fat :: Int -> Int
fat 0 = 1
fat n = n * fat (n - 1)

fat' :: Int -> Int 
fat' n
    | n <= 0 = 1
    | otherwise = n * fat (n-1)
    
fib :: Int -> Int
fib t
    | t <= 0 = 0
    | t == 1 = 1
    | otherwise = fib (t-1) + fib (t-2)
    
{-*
filter
exemplo: eliminar as vogais de uma palavra
String = [char]
(x:xs) um ou mais elementos
*-}

eliminarVogal :: String -> String
eliminarVogal [] = []
eliminarVogal (x:xs)
   | elem x "AEIOUaeiou" = eliminarVogal xs
   | otherwise = x : eliminarVogal xs




{-*
filter (\x -> notElem x "AEIOUaeiou") "Juliana"
"JLN"
utilizando filter .... acima foi feito na mão

--MAP : "joga" uma função para dentro de cada
--elemento de uma lista
--Filter elimina elemntos que dao false na condição

https://ide.c9.io/romefeller/hask0
*-}