module Main where

-- Exercício 6.1 Faça um programa que receba um número inteiro e mostre na tela
-- se este é par ou ímpar usando a Monad IO.

-- Para compilar e usar o codigo objeto, 
-- deve se digitar no terminal:
-- ghc -o nomeDoExecutavel nomeDoModulo
-- lembrando que isso deve ser feito no bash e o nome do executavel pode ser qqr um
-- Se não houverem erros, para executar bastar digitar 
-- ./nomeDoExecutavel

main :: IO ()
main = do
    putStrLn "Digite um número:"
    nro <- getLine
    resposta <- return $ isPar $ verifica (read nro :: Int)
    putStrLn $ resposta
    

verifica :: Int -> Bool
verifica x = mod x 2 == 0

isPar :: Bool -> String
isPar True = "O número digitado é par"
isPar False = "O número digitado não é par"