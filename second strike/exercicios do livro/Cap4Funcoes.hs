module Cap4Funcoes where


(|>) :: a -> (a -> b) -> b
(|>) x f = f x

infixl 0 |>

doubles = [1.2, 2.3, 2, 5.12]

inteiros :: [Int]
inteiros = [-5..9]

strings = ["adaao", "php", "ovo", "haskell"]

s = "LAVEMOPATOPATOAQUIPATOACOLA"

carteira = [Dinheiro 3.4 Dollar, Dinheiro 2 Dollar, Dinheiro 5 Real]

dias = [Segunda, Terca, Quarta, Terca, Sexta]

{-
4.1)	Faça	uma	função	que	retorne	a	média	de	um	 	[Double]	 ,
usando	 	foldl	 .
-}
mediaDouble :: [Double] -> Double
mediaDouble l = (/) (foldl (+) 0.0 l) $ fromIntegral (length l)

{-
4.2)	 Faça	 uma	 função	 que	 receba	 uma	 	[String]	 	 e	 retorne
todos	os	elementos	palíndromos.	Ver	exercício	3.7.
-}

filtroDePalindromos :: [String] -> [String]
filtroDePalindromos stgs = filter (\x -> reverse x == x) stgs

{-
4.3)	 Implemente	 uma	 função	 que	 filtre	 os	 números	 pares	 e
outra	que	filtre	os	ímpares	de	uma	lista	recebida	via	parâmetro.
-}

pares :: [Int] -> [Int]
pares ints = filter (\x -> even x) ints

impares :: [Int] -> [Int]
impares ints = filter (\x -> not . even $ x) ints

{-
4.4)	 Filtre	 os	 números	 primos	 de	 uma	 lista	 recebida	 por
parâmetro.
-}
isPrimo :: Int -> Bool
isPrimo n = filter (\x -> mod n x == 0) [1 .. n - 1] == [1]

filtraPrimos :: [Int] -> [Int]
filtraPrimos ints = filter isPrimo ints

{-
4.5)	Implemente	uma	função	que	receba	uma	lista	de	inteiros	e
retorne	o	dobro	de	todos,	eliminando	os	múltiplos	de	4.
-}

dobroMenosQuatro :: [Int] -> [Int]
dobroMenosQuatro n = n
  |> fmap (*2)
  |> filter (\x -> mod x 4 /= 0)


{-
4.6)	Faça	uma	função	 	func	 	que	receba	uma	função	 	f	 	de	tipo
	(String	->	String)	 ,	e	uma	 	String	s	 	que	retorna	o	reverso de	 	s
  concatenado	com	aplicação	da	função	 	f	 	em	 	s	 .
-}

func :: (String -> String) -> String -> String
func f s = f s ++ reverse s

{-
4.7)	Crie	um	tipo	 	Dia	 	contendo	os	dias	da	semana.	Faça	uma
função	que	receba	uma	lista	de	 	Dias	 	e	filtre	as	 	Terças	 .
-}

data Dia = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo deriving (Show, Eq)

fDias :: [Dia] -> [Dia]
fDias dias = filter (\dia -> dia == Terca) dias

{-
4.8)	 Implemente	 o	 tipo	 	Dinheiro	 	 que	 contenha	 os	 campos
	valor	 	 e	 	correncia	 	 ( 	Real	 	 ou	 	Dolar	 ),	 e	 uma	 função	 que
converta	todos	os	"dinheiros"	de	uma	lista	para	dólar	(e	outra	para
real).	Com	isso,	implemente	funções	para:
Filtrar	todos	os	 	Dolares	 	de	uma	lista	de	 	Dinheiro	 .
Somar	todos	os	 	Dolares	 	de	uma	lista.
Contar	a	quantidade	de	 	Dolares	 	de	uma	lista.
-}

data Dinheiro = Dinheiro {valor :: Double,
                          correncia :: Correncia
                          } deriving (Show, Eq)

data Correncia = Dollar | Real deriving (Show, Eq)

toDollar :: Dinheiro -> Dinheiro
toDollar (Dinheiro v Real) = Dinheiro (v / 3.12) Dollar
toDollar x = x

toReal :: Dinheiro -> Dinheiro
toReal (Dinheiro v Dollar) = Dinheiro (v * 3.12) Real
toReal x = x

filtraDollar :: [Dinheiro] -> [Dinheiro]
filtraDollar din = filter (\d -> correncia d == Dollar) din

somaDollar :: [Dinheiro] -> Dinheiro
somaDollar din = foldl (\x y -> Dinheiro ((valor x) + (valor y)) Dollar) (Dinheiro 0 Dollar) $ filtraDollar din

qntdDollar :: [Dinheiro] -> Int
qntdDollar din = length $ filtraDollar din

{-
4.9)	Usando	a	função	 	foldl	 ,	crie	lambdas	para:
Contar	números	negativos	de	uma	lista	de	 	Int	 .
Contar	letras	'P'	de	uma	 	String	 .
Para	 contar	 	 Sabados	 	 em	 uma	 lista	 de	 um
	[DiaSemana]	 .
Para,	a	partir	de	uma	lista	de	 	[DiaSemana]	 ,	retornar
a	 soma	 dos	 dias.	 Exemplo:	 	 [Segunda,	 Segunda,
Quarta]	 	deve	retornar	 	5	 .	 Use	 uma	 função	 auxiliar
para	converter	 	DiaSemana	 	para	 	Int	 .
-}

--como eu faria:
-- negativos ints = length . filter (\x -> x < 0) ints

--usando foldl...
negativos :: [Int] -> Int
negativos ints = foldl (+) 0 $ fmap (\i -> if (i < 0) then 1 else 0) ints

quantosPs :: String -> Int
quantosPs s = foldl (+) 0 $ fmap (\x -> if (x == 'P') then 1 else 0) s

dayToInt :: Dia -> Int
dayToInt Domingo = 1
dayToInt Segunda = 2
dayToInt Terca = 3
dayToInt Quarta = 4
dayToInt Quinta = 5
dayToInt Sexta = 6
dayToInt Sabado = 7

contaDias :: [Dia] -> Int
contaDias dias = foldl (+) 0 $ fmap dayToInt dias

{-
4.10)	Reescreva	os	exercícios	anteriores	usando:	 	.	 ,	 	$	 	e	 	|>	 .
-}
