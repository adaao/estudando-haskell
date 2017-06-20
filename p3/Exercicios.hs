module Exercicios where
import Control.Applicative
import Control.Monad

data Coisa a = Nada | UmaCoisa a | DuasCoisas a a | TresCoisas a a a deriving Show

{-
Exercício 6.2 
Faça uma instância de
Functor para o tipo Coisa definido no 
exercício ??. 
A função deve ser aplicada em 
todas as coordenadas de Coisa.
-}

instance Functor Coisa where
    fmap f Nada = Nada
    fmap f (UmaCoisa a) = UmaCoisa (f a)
    fmap f (DuasCoisas a b) = DuasCoisas (f a) (f b)
    fmap f (TresCoisas a b c) = TresCoisas (f a) (f b) (f c)
    

{-
Exercício 6.3 
Aproveitando o exercício anterior, 
faça uma instância de Applicative Functor 
para o tipo Coisa definido no exercício ??. 
Para definir um Applicative
Functor é necessário definir seu elemento
neutro pure(TresCoisas) e seu operador <*>. 
A regra para <*> deve ser a distribuição das
funções dos campos para
o campo do argumento de forma ordenada. 
Por exemplo,

DuasCoisas (+4) (+5) <*> DuasCoisas 2 1 =
ghci> DuasCoisas 6 6

Cada value constructor deve se combinar com o
mesmo, caso contrário o valor
retornado é sempre Nada.
Referência: http://learnyouahaskell.com/functors-applicative-functors-and-monoids
-}

{-
Exercício 6.4 
Crie uma instância de Monad para o 
tipo Coisa definido no exercício ??. 
Seu return deve ser o value constructor UmaCoisa.
Referência: http://learnyouahaskell.com/a-fistful-of-monads
-}

{-
Exercício 6.5 
Crie uma função
mult234 :: Double → Coisa Double
que multiplica por 2 a primeira coordenada, por 3
a segunda e por 4 a terceira o
parâmetro x recebido.32 6 Aula 10
-}


{-
Exercício 6.6 
Determine o valor das expressões (caso seja
possível) abaixo:
-}
--1. TresCoisas 1 2 3 »= mult234

--2. (*2) <$> DuasCoisas 2 4

--3. :kind Coisa

--4. DuasCoisas (*2) (*3) <*> DuasCoisas 3 4

--5. (*2) <$> DuasCoisas 7 9

--6. DuasCoisas (*2) (*3) <*> UmaCoisa 5

--7. pure (*2) <*> DuasCoisas 4 5

--8. pure (+) <*> DuasCoisas 1 2 <*> DuasCoisas 2 1

--9. (*) <$> TresCoisas 1 2 3 <*> TresCoisas 1 2 3

--10. (*) <$> (TresCoisas 1 2 3 »= mult234) <*> (TresCoisas 1 2 3 »= mult234)

{-
Exercício 6.7 
Faça um exemplo, usando a notação do, de um trecho qualquer
de código usando sua Monad Coisa.
-}
{-
Exercício 6.8 
Escreva a função do exercício 6.5 em termos dos operadores Applicative.
-}

{-
Exercício 6.9 
Escreva uma instância para Functor e Applicative Functor para
o tipo (Arvore a) visto na Aula 4, 
quadro 4.2. A regra para estas instâncias são
análogas (a menos de recursão).
-}

{-
Exercício 6.10 Crie 5 expressões usando o Applicative Functor de seu tipo Tree.
-}
