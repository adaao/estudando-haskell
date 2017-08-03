module Exercicios where
import Control.Applicative
import Control.Monad

data Coisa a = Nada | UmaCoisa a | DuasCoisas a a | TresCoisas a a a deriving Show

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

instance Applicative Coisa where
    pure = UmaCoisa 
    Nada <*> _ = Nada
    (UmaCoisa f) <*> x = fmap f (x)
-------------------------------------------------------------------------------    
    (UmaCoisa f) <*> DuasCoisas _ _ = Nada
    (UmaCoisa f) <*> TresCoisas _ _ _ = Nada
------------------------------------------------------------------------------    
    (DuasCoisas f f') <*> (DuasCoisas z y) = DuasCoisas (f z) (f' y)
-------------------------------------------------------------------------------    
    (DuasCoisas f f') <*> UmaCoisa _ = Nada
    (DuasCoisas f f') <*> TresCoisas _ _ _ = Nada
-------------------------------------------------------------------------------    
    (TresCoisas f j k) <*> (TresCoisas x y z) = TresCoisas (f x) (j y) (k z)
------------------------------------------------------------------------------
    (TresCoisas f j k) <*> DuasCoisas _ _ = Nada
    (TresCoisas f j k) <*> UmaCoisa _ = Nada

{-
Exercício 6.4 
Crie uma instância de Monad para o 
tipo Coisa definido no exercício ??. 
Seu return deve ser o value constructor UmaCoisa.
Referência: http://learnyouahaskell.com/a-fistful-of-monads
-}
{-
class Monad m where  
    return :: a -> m a  
  
    (>>=) :: m a -> (a -> m b) -> m b  
  
    (>>) :: m a -> m b -> m b  
    x >> y = x >>= \_ -> y  
  
    fail :: String -> m a  
    fail msg = error msg  

-}
instance Monad Coisa where
--  return :: a -> Coisa a    
    return x = UmaCoisa x

--  (>>=) :: Coisa a -> (a -> Coisa b) -> Coisa b
    Nada >>= f = Nada
    (UmaCoisa a) >>= f = f a
    
    fail _ = Nada
    
teste :: Coisa Int -> Coisa Int
teste (DuasCoisas x y) = DuasCoisas (x*2) (y+3)
teste x = x
{-
Exercício 6.5 
Crie uma função
mult234 :: Double → Coisa Double
que multiplica por 2 a primeira coordenada, por 3
a segunda e por 4 a terceira o
parâmetro x recebido.32 6 Aula 10
-}

mult234::Double -> Coisa Double
mult234 x = TresCoisas (2* x) (3 * x) (4* x)  


{-
Exercício 6.6 
Determine o valor das expressões (caso seja
possível) abaixo:
-}
--1. TresCoisas 1 2 3 »= mult234
-- TresCoisas 2 6 12

--2. (*2) <$> DuasCoisas 2 4
--DuasCoisas 4 8

--3. :kind Coisa
-- * -> *

--4. DuasCoisas (*2) (*3) <*> DuasCoisas 3 4
-- DuasCoisas 6 12

--5. (*2) <$> DuasCoisas 7 9
-- DuasCoisas 14 18

--6. DuasCoisas (*2) (*3) <*> UmaCoisa 5
-- Nada

--7. pure (*2) <*> DuasCoisas 4 5
-- DuasCoisas 8 10

--8. pure (+) <*> DuasCoisas 1 2 <*> DuasCoisas 2 1
-- DuasCoisas 3 3

--9. (*) <$> TresCoisas 1 2 3 <*> TresCoisas 1 2 3
-- TresCoisas 1 4 9

--10. (*) <$> (TresCoisas 1 2 3 »= mult234) <*> (TresCoisas 1 2 3 »= mult234)



{-
Exercício 6.7 
Faça um exemplo, usando a notação do, de um trecho qualquer
de código usando sua Monad Coisa.
-}

idade :: Coisa Int -> Coisa Int -> Coisa Int
idade anoNasc anoAtual = do
    x <- anoNasc
    y <- anoAtual
    return (y - x)
    
{-
Exercício 6.8 
Escreva a função do exercício 6.5 em termos dos operadores Applicative.
-}

-- (TresCoisas (*2) (*3) (*4)) <*> ((\x -> (TresCoisas x x x)) $ 3)

{-
Exercício 6.9 
Escreva uma instância para Functor e Applicative Functor para
o tipo (Arvore a) visto na Aula 4, 
quadro 4.2. A regra para estas instâncias são
análogas (a menos de recursão).
-}

{-
ARVORE DO LEARN YOU A HASKELL
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right) 

treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right 
-}
 -- ARVORE DO PROF
data Arvore a = Nulo | Leaf a | Branch a (Arvore a) (Arvore a) deriving Show

arvore :: Arvore Int
arvore = Branch 50 (Branch 30 (Leaf 20) (Leaf 40)) (Branch 90 Nulo (Leaf 100))

instance Functor Arvore where
    fmap f Nulo = Nulo
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Branch a b c) = Branch (f a) (fmap f b) (fmap f c)

instance Applicative Arvore where
    pure = Leaf
    Nulo <*> _ = Nulo
    (Leaf f) <*> (Leaf a) = Leaf (f a)
    (Branch f j k) <*> (Branch a b c) = Branch (f a) (j <*> b) (k <*> c)
    _ <*> _ = Nulo

{-
Exercício 6.10 Crie 5 expressões usando o Applicative Functor de seu tipo Tree.
-} 
 
-- (*2) <$> arvore

--  (Branch (+50) (Leaf (*2)) (Leaf (+2))) <*> (Branch 1 (Leaf 2) (Leaf 5))