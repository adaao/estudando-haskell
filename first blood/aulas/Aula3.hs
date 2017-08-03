module Aula3 where

data Dia = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado deriving Show

agenda :: Dia -> String
agenda Domingo = "Descanso..."
agenda Sabado = "Dia de maldade..."
agenda Quinta = "Dia de haskell..."
agenda x = "trabalho..."

--Faça 
-- to dia
-- to Int

toInt :: Dia -> Int
toInt Segunda = 2
toInt Domingo = 1
toInt Terca = 3
toInt Quarta = 4
toInt Quinta = 5
toInt Sexta = 6
toInt Sabado = 7

toDia :: Int -> Dia
toDia 1 = Domingo
toDia 2 = Segunda
toDia 3 = Terca
toDia 4 = Quarta
toDia 5 = Quinta
toDia 6 = Sexta
toDia _ = Sabado --underline expressa um valor que não está sendo usado

-- Crie o tipo day que possua os values constructors do tipo Dia em ingles. 
-- faça as funções :
-- a) traduzir enpt 


data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving Show

traduzirEnpt :: Day -> Dia
traduzirEnpt Sunday = Domingo
traduzirEnpt Monday = Segunda
traduzirEnpt Tuesday = Terca
traduzirEnpt Wednesday = Quarta
traduzirEnpt Thursday = Quinta
traduzirEnpt Friday = Sexta
traduzirEnpt Saturday = Sabado

traduzirPten :: Dia -> Day
traduzirPten Domingo = Sunday
traduzirPten Segunda = Monday
traduzirPten Terca = Tuesday
traduzirPten Quarta = Wednesday
traduzirPten Quinta = Thursday
traduzirPten Sexta = Friday
traduzirPten Sabado = Saturday



data Correncia = Real | Euro | Libra deriving Show

data Moeda = Moeda Double Correncia deriving Show

converterReal :: Moeda -> Moeda
converterReal (Moeda valor Euro) = Moeda (3.24*valor) Real
converterReal (Moeda valor Libra) = Moeda (3.84*valor) Real
converterReal x = x

fatorial :: Int -> Int
fatorial 1 = 1
fatorial 0 = 1
fatorial x = calculo x x

calculo :: Int -> Int -> Int
calculo x 1 = x
calculo x y = calculo (x*(y-1)) (y-1)
--calculo _ _ = 0