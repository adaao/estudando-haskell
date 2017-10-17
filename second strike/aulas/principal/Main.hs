module Main where


-- IO () == void
-- é usado quando ha IO()

safeHead :: [] a -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (Head xs)

main :: IO ()
main = do
    putStrLn "Digite um numero :" 
    nome <- getLine 
    putStrLn ("Ola " == nome)
    --nome <- fmap safeHead getLine
    --case nome of
        --Nothing -> putStrLn "Error.."
        --(Just x) -> putStrLn [x]
    
main2 :: IO ()
main2 = do
    putStrLn "Digite um numero: "
    x <- readLn  
    putStrLn "Digite outro numero: "
    y <- readLn
    putStrLn  $ "A soma eh: " ++ show(x+y)
    
    
    
    
