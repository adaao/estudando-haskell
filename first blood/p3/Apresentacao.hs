module Apresentacao where

import Control.Monad

--data Animal = Dragao | PolvoGigante

{-
Exercício 6.4 Crie uma instância de Monad para o tipo Coisa definido no exercício ??.
Seu return deve ser o value constructor UmaCoisa. 
Referência: http://learnyouahaskell.com/a-fistful-of-monads
-}
                         --
data Coisa a = UmaCoisa a | DuasCoisas a a| TresCoisas a a a | Nada deriving Show

instance Monad Coisa where
    --return::a -> Coisa a                       
    return a = UmaCoisa a
    --(>>=)::Coisa a -> (a -> Coisa b) -> Coisa b
    Nada >>= f = Nada
    (UmaCoisa a) >>= f = f a

{-
As leis a serem seguidas

A primeira lei define que, se pegarmos um valor, colocarmos em um contexto mínimo usando a 
função return e então passar para função »=, o resultado será o mesmo que pegar o valor e aplicar a função.

ghci> somaCoisa 1

ghci> return 1 >>= somaCoisa

A segunda lei define que, se temos um valor que é uma monad, e nos usarmos 
a função »= para passar o valor para a função return, 
o resultado deve ser o nosso valor original.

ghci> UmaCoisa 1 >>= return
UmaCoisa 1

A terceira e última lei diz que, dado um encadeamento de chamadas de função, 
a ordem que as mesmas são aninhadas não deve influenciar no resultado.

ghci> UmaCoisa 1 >>= (\n -> UmaCoisa (n + 1) >>= somaCoisa)
UmaCoisa 3

ghci> (UmaCoisa 1 >>= \n -> UmaCoisa (n + 1)) >>= somaCoisa
UmaCoisa 3

-}

coisa::a->Coisa a
coisa a = UmaCoisa a

somaCoisa :: Int -> Coisa Int
somaCoisa x = UmaCoisa (x + 1)


nada :: a -> Coisa a
nada _ = Nada




{-
Exercício 6.5 Crie uma função mult234 :: Double → Coisa Double
que multiplica por 2 a primeira coordenada, por 3 a segunda e por 4 a terceira o
parâmetro x recebido.
-}

mult234::Double -> Coisa Double              --FEITO
mult234 x = TresCoisas (2* x) (3 * x) (4* x)  

{-
Exercício 6.7 Faça um exemplo, usando a notação do, de um trecho qualquer
de código usando sua Monad Coisa
-}

coisas :: Coisa Int
coisas = do
    x <- UmaCoisa 3
    y <- UmaCoisa 4
    UmaCoisa (x + y)
 
foo' :: Coisa String  
foo' = do  
    x <- UmaCoisa "Exercicio"  
    y <- UmaCoisa " concluido!"  
    UmaCoisa (x ++ y) 
    