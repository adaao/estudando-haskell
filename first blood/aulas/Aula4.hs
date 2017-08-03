
data Naipe = Copas | Ouros | Espadas | Paus deriving (Eq, Show)

--data Valor = A | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | J | Q | K deriving Show

data Valor = A |Dois|Tres|Quatro|Cinco|Seis|Sete|Oito|Nove|Dez| J | Q | K deriving (Eq, Show)

data Carta = Carta {valor' :: Valor, naipe :: Naipe} deriving (Eq, Show)

fazCarta :: Valor -> Naipe -> Carta
fazCarta v n = Carta v n

retornaValor :: Carta -> Valor
retornaValor (Carta v n) = v

retornaNaipe :: Carta -> Naipe
retornaNaipe (Carta v n) = n

c = [Carta K Ouros, Carta Dois Paus, Carta Dez Copas, Carta Tres Espadas]


filtraVermelhos :: [Carta] -> [Carta]
filtraVermelhos y = [x | x <- y, elem (y _ n) [Ouros, Copas]]

converterReal :: Moeda -> Moeda
converterReal (Moeda valor Euro) = Moeda (3.24*valor) Real
converterReal (Moeda valor Libra) = Moeda (3.84*valor) Real
converterReal x = x

data Moeda = Moeda {valor :: Double, correncia :: Correncia} deriving Show

data Correncia = Real | Euro | Libra deriving Show

--data Moeda = Moeda Double Correncia deriving Show

somarMoedaReal :: Moeda -> Moeda -> Moeda
somarMoedaReal (Moeda v Real) moeda = Moeda (v + valor (converterReal moeda)) Real

--exercicio: fazer a funcao render, que recebe um fator (porcentagem) e uma moeda. A funcao
--retorna a moeda corrigida

render :: Double -> Moeda -> Moeda
render x moeda = Moeda ((x / 100 + 1) * valor moeda) (correncia moeda)

--render : fator
--x: variavel

--exemplo de curing

--somar x y = x+y

--let f = somar 4
--f 3
 --Lâmbida
 -- expressao: 
 --let s = \x y -> x+Y
