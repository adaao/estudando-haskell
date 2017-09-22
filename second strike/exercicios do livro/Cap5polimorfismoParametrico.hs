{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Cap5polimorfismoParametrico where

import Data.Monoid
{-
5.1)	 Crie	 o	 tipo	 	 TipoProduto	 	 que	 possui	 os	 values
constructors	 	Escritorio	 ,	 	Informatica	 ,	 	Livro	 ,	 	Filme	 	 e
	Total	 .	 O	 tipo	 	 Produto	 	 possui	 um	 value	 constructor	 -	 de
mesmo	 nome	 -	 e	 os	 campos	 	 valor	 	 ( 	 Double	 ),	 	 tp
( 	TipoProduto	 )	e	um	value	constructor	 	Nada	 ,	que	representa	a
ausência	de	um	 	Produto	 .
Deseja-se	calcular	o	valor	total	de	uma	compra,	de	modo	a	não
ter	 nenhuma	 conversão	 para	 inteiro	 e	 de	 forma	 combinável.	 Crie
uma	instância	de	monoide	para	 	Produto	 ,	de	modo	que	o	retorno
sempre	tenha	 	Total	 	no	campo	 	tp	 	e	a	soma	dos	dois	produtos
em	 	 valor	 .	 Explique	 como	 seria	 o	 exercício	 sem	 o	 uso	 de
monoides.	Qual(is)	seria(m)	a(s)	diferença(s)?
-}
data TipoProduto = Escritorio | Informatica | Livro | Filme | Total deriving Show

data Produto = Produto {valor :: Double,
                        tp :: TipoProduto} | Nada deriving Show

instance Monoid Produto where
    mempty = Produto 0 Total
    mappend (Produto x _) (Produto y _) = Produto (x + y) Total

carrinho :: [Produto]
carrinho = [Produto 50.99 Livro, Produto 40 Escritorio, Produto 99.99 Informatica]

{-

5.2)	 Crie	 uma	 função	 	totalGeral	 	 que	 recebe	 uma	 lista	 de
produtos	e	retorna	o	preço	total	deles	usando	o	monoide	anterior.
-}

totalGeral :: [Produto] -> Produto
totalGeral xs = foldl mappend mempty xs

{-
5.3)	 A	 função	 	min	 	 no	 Haskell	 retorna	 o	 menor	 entre	 dois
números,	por	exemplo,	 	min	4	5	=	4	 .
Crie	 um	 tipo	 	Min	 	 com	 um	 campo	 inteiro,	 que	 seja
instância	de	 	Ord	 ,	 	Eq	 	e	 	Show	 	(deriving).
Crie	 uma	 instância	 de	 	 Monoid	 	 para	 	 Min
( 	maxBound	 	 representa	 o	 maior	 inteiro	 existente	 no
Haskell).
Quanto	vale	a	expressão	 	Min	 (-32)	 <>	 Min	 (-34)
<>	Min	(-33)	 ?
Explique	sua	escolha	para	o	 	mempty	 .
-}

data Min = Min Int deriving (Show, Eq, Ord)

instance Monoid Min where
    mempty = Min (maxBound :: Int)
    mappend (Min x) (Min y) = Min $ min x y

{-
escolhi o valor máximo do Int porque qualquer valor comparado seria
menor ou igual a tal valor
-}

{-
5.4)	 Crie	 uma	 função	 	minAll	 	 que	 recebe	 um	 	 [Min]	 	 e
retorna	um	 	Min	 	contendo	o	menor	valor.
-}

minAll :: [Min] -> Min
minAll xs = foldl mappend mempty xs

{-
5.5)	Crie	o	tipo	 	Paridade	 	com	os	values	constructors	 	Par	 	e
	Impar	 .	Crie	o	typeclass	 	ParImpar	 	que	contém	a	função	 	decide
::	a	->	Paridade	 	e	possui	as	instâncias:
Para	 	Int	 :	noção	de	Par/Impar	de	 	Int	 .
Para	 	[a]	 :	uma	lista	de	elementos	qualquer	é	 	Par	 	se
o	número	de	elementos	o	for.
	Bool	 :	 	False	 	como	 	Par	 ,	 	True	 	como	 	Impar	 .
-}

data Paridade = Par | Impar deriving (Show)

class ParImpar a where
  decide :: a -> Paridade

instance ParImpar Int where
  decide i
    | mod i 2 == 0 = Par
    | otherwise = Impar

instance ParImpar Integer where
  decide i
    | mod i 2 == 0 = Par
    | otherwise = Impar

instance ParImpar [a] where
  decide a
    | mod (length a) 2 == 0 = Par
    | otherwise = Impar


{-
5.6)	 A	 função	 	max	 	 no	 Haskell	 retorna	 o	 maior	 entre	 dois
números,	por	exemplo:	 	max	4	5	=	5	 .
Crie	 um	 tipo	 	Max	 	 com	 um	 campo	 inteiro	 que	 seja
instância	de	 	Ord	 ,	 	Eq	 	e	 	Show	 	(deriving).
Crie	 uma	 instância	 de	 	 Monoid	 	 para	 	 Max
( 	minBound	 	 representa	 o	 menor	 inteiro	 existente	 no
Haskell).
Quanto	vale	a	expressão	 	Max	10	<>	Max	13	<>	Max
5	 ?
Explique	sua	escolha	para	o	 	mempty	 .
Crie	 uma	 função	 	maxAll	 	 que	 recebe	 um	 	[Max]	 	 e
retorna	um	 	Max	 	contendo	o	maior	valor.
-}

data Max = Max Int deriving (Show, Ord, Eq)

instance Monoid Max where
  mempty = Max (minBound :: Int)
  mappend (Max x) (Max y) = Max $ max x y

-- escolhi o mempty pq qualquer Int comparado sera maior ou igual

{-
5.7)	Usando	a	estrutura	de	árvore,	monte	uma	função	 	mapa	 ,
que	 jogue	 uma	 função	 passada	 por	 parâmetro	 para	 todos	 os
elementos	de	uma	árvore.	Deixe	explícito	o	tipo	desta	função.
-}

data Arvore a = Nulo | Leaf a | Branch a (Arvore a) (Arvore a) deriving (Show, Eq)

pBrasil = Branch 50 (Branch 30 (Leaf 20) (Leaf 40)) (Branch 90 Nulo (Leaf	100))

mapa :: (a -> b) -> Arvore a -> Arvore b
mapa f Nulo = Nulo
mapa f (Leaf a) = Leaf (f a)
mapa f (Branch x y z) = Branch (f x) (mapa f y) (mapa f z)

instance Functor Arvore where
  fmap f Nulo = Nulo
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Branch x y z) = Branch (f x) (fmap f y) (fmap f z)

{-
5.8)	 Usando	 o	 exercício	 anterior,	 some	 5	 a	 cada	 elemento	 de
uma	árvore	de	inteiros.
5.9)	 Uma	 lista	 ordenada	 é	 uma	 lista	 cujos	 elementos	 são
inseridos	de	forma	ordenada	(crescente).	Usando	o	tipo	 	ListaOrd
a	 =	 a	 :?:	 (ListaOrd	 a)	 |	 Nulo	 deriving	 Show	 ,	 crie	 as
funções:
inserir	 ::	 (Ord	 a)	 =>	 a	 ->	 ListaOrd	 a	 ->
ListaOrd	a
 remover	 ::	 (Eq	 a)	 =>	 a	 ->	 ListaOrd	 a	 ->
ListaOrd	a
tamanho	::	ListaOrd	a	->	Int
Observação:	a	função	 	remover	 	deve	buscar	um	elemento.	Se
não	achar,	a	lista	deve	se	manter	intacta.
-}


{-
5.10)	Usando	a	estrutura	de	árvore	vista,	faça	uma	função	que
some	todos	os	elementos	de	uma	árvore	de	números.
-}


{-
5.11)	 Implemente	 os	 percursos	 pós-ordem	 e	 pré-ordem.	 Via
comentário,	 faça	 os	 "testes	 de	 mesa"	 para	 os	 dois	 percursos	 da
árvore	 	Raiz	 15	 (Raiz	 11	 (Folha	 6)	 (Raiz	 12	 (Folha	 10)
Nula))	(Raiz	20	Nula	(Raiz	22	(Folha	21)	Nula))	 .
-}


{-
5.12)	 Faça	 uma	 função	 para	 inserir	 um	 elemento	 em	 uma
árvore	de	busca	binária	(use	a	mesma	estrutura	vista).
-}


{-
5.13)	Faça	uma	função	que,	a	partir	de	uma	lista	de	elementos
de	tipo,	insira	todos	os	elementos	desta	lista	na	árvore	e	retorne-a,
usando	o	exercício	anterior.
-}
