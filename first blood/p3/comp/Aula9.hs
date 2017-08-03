module Main where

-- QUANDO HA UMA OPERACAO NAO MONADICA DENTRO DE UMA MONADA,
-- TEMOS QUE USAR O return.
main :: IO ()
main = do
    putStrLn "Digite um nome: "
    nome <- getLine
    rev <- return $ reverse nome
    putStrLn $ "Olá " ++ rev
    
main' :: IO ()
main' = putStrLn "Digite um nome" >> getLine >>= \nome -> putStrLn $ "Olá " ++ nome
    