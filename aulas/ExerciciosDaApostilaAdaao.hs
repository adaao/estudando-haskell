module ExerciciosDaApostilaAdaao where
--1.4 Exercícios
--Exercício 1.1 Crie o tipo Pergunta (Resposta?) com os value constructors Sim ou Nao. Faça
--as funções abaixo determinando seus tipos explicitamente
data Pergunta = Sim | Nao deriving Show

--1. pergNum: recebe via parâmetro uma Pergunta e retorne 0 para Nao e 1
--para Sim;

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

--2. listPergs: recebe via parâmetro uma lista de Perguntas e retorna 0’s e 1’s
--correspondentes aos constructores contidos na lista;

listPergs :: [Pergunta] -> [Int]
listPergs y = [pergNum x | x <- y] 

listPergs' :: [Pergunta] -> [Int]
listPergs' y = map (pergNum) y


--3. and’: recebe duas Perguntas como parâmetro e retorna a tabela verdade do
--and lógico usando Sim como verdadeiro e Nao como falso.

bool :: Pergunta -> Bool
bool Sim = True
bool Nao = False

and' :: Pergunta -> Pergunta -> Bool
and' x y =  and [bool x, bool y]


--4. or’: Idem acima, porém, deve ser usado o ou lógico.

or' :: Pergunta -> Pergunta -> Bool
or' x y = or [bool x, bool y]

--5. not’: Idem aos anteriores, porém, usando o not lógico.

not' :: Pergunta -> Bool
not' x = not (bool x)

--Exercício 1.2 
-- 1 Gere as listas [1,11,121,1331,14641,161051,1771561] e
-- 2 [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,33,34,35,37,38,39] usando
--   list comprehension.

-- 1
gerListaOnze :: [Int]
gerListaOnze = [11^x | x <- [0 .. 6]] --verifcar

--2

gerLista :: [Int]
gerLista = [x | x <- [1, 2 .. 39],  mod x 4 /= 0]

--Exercício 1.3 Faça o tipo Temperatura que pode ter valores Celsius, Farenheit
--ou Kelvin. Implemente as funções:
data Temperatura = Celsius | Fahrenheit | Kelvin deriving Show

--Celsius p/ Fahrenheit °F = °C × 1,8 + 32 

--Fahrenheit p/ Celsius °C = (°F − 32) / 1,8 

--Celsius p/ Kelvin K = °C + 273,15 

--Kelvin p/ Celsius °C = K − 273,15 

--1. converterCelsius: recebe um valor double e uma temperatura e faz a conversão para Celsius.

data Febre = Febre {graus :: Double, temp :: Temperatura} deriving Show

converterCelsius :: Temperatura -> Double -> (Double, Temperatura)
converterCelsius Celsius v = (v, Celsius)
converterCelsius Kelvin v = ((v - 273.15), Celsius)
converterCelsius Fahrenheit v = (((v-32)/1.8), Celsius)

--2. converterKelvin: recebe um valor double e uma temperatura e faz a conversão para Kelvin.

--converterKelvin :: Double -> Temperatura -> (Double, Temperatura)
converterkelvin x Celsius = ((x+273.15), Kelvin) 
converterkelvin x Fahrenheit = ((((x-32)/1.8)+273.15), Kelvin)
converterkelvin x y = (x, y)  


--3. converterFarenheit: recebe um valor double e uma temperatura e faz a conversão para Farenheit.

--converteFahrenheit :: Double -> Temperatura -> (Doueble, Temperatura)
--converteFahrenheit x 

--Exercício 1.4 Faça uma função que simule o vencedor de uma partida de pedra,
--papel e tesoura usando tipos criados (você não poderá usar qualquer outro tipo
--que não seja criado usando o data). Casos de empate devem ser considerados em
--seu tipo.

data Jogada = Pedra | Papel | Tesoura deriving Show
data Resultado = Vence | Empate deriving Show

jokenpo :: Jogada -> Jogada -> (Jogada, Resultado) 
jokenpo Pedra Papel = (Papel, Vence)
jokenpo Papel Pedra = (Papel, Vence)
jokenpo Tesoura Papel = (Tesoura, Vence)
jokenpo Papel Tesoura = (Tesoura, Vence)
jokenpo Tesoura Pedra = (Pedra, Vence)
jokenpo Pedra Tesoura = (Pedra, Vence)  
jokenpo x y = (x, Empate)

   

--Exercício 1.5 Faça uma função que retorne uma string com todas as vogais
--maiúsculas e minúsculas eliminadas de uma string passada por parâmetro usando
--list compreenshion.
--Dica: procure informações sobre a função elem.

retornaVogais :: String -> String
retornaVogais [] = []
retornaVogais (x:xs)
    | notElem x "AEIOUaeiou" = retornaVogais xs
    | otherwise = x : retornaVogais xs

--Exercício 1.6 Gere as listas
--1. ["AaBB", "AbBB", "AcBB", "AdBB", "AeBB", "AfBB", "AgBB"]
--2. [5,8,11,17,20,26,29,32,38,41]
--3. [1.0,0.5,0.25,0.125,0.0625,0.03125]
--usando list compreenshion.

listaUm :: [String]
listaUm = [ "A" ++ [x] ++ "BB" | x <- ['a' .. 'g'] ]

listaDois :: [Int]
listaDois = [ x | x <- [5, 8..41], elem x [14, 23, 35] == False]

listaTres :: [Double]
listaTres = [ (1/2^n) | n <- [0..5] ]

--Exercício 1.7 Sabe-se que as unidades imperiais de comprimento podem ser
--Inch, Yard ou Foot (há outras ignoradas aqui). Sabe-se que 1in = 0.0254m,
--1yd = 0.9144m, 1ft = 0.3048. Faça a função converterMetros que recebe a 
--unidade imperial e o valor correspondente nesta unidade e retorna o valor em metros.
--Faça a função converterImperial que recebe um valor em metros e a unidade de
--conversão e retorna o valor convertido para a unidade desejada.

--http://learnyouahaskell.com/ -> acessar

--https://c9.io/romefeller

--https://c9.io/isagomes/haskell1


inch :: Double
inch = 0.0254

yard :: Double
yard = 0.9144

foot :: Double
foot = 0.3048

data MedidaImperial = Inch | Yard | Foot deriving Show

converterEmMetros :: Double -> MedidaImperial -> Double
converterEmMetros x Inch = x*inch
converterEmMetros x Yard = x*yard
converterEmMetros x Foot = x*foot

converterEmImperial :: Double -> MedidaImperial -> Double
converterEmImperial x Inch = x/inch
converterEmImperial x Yard = x/yard
converterEmImperial x Foot = x/foot

{-
        |x Inch = x * inch
        |x Yard = x * yard
        |x Foot = x * foot

-}
--    where inch = 0.0254

--Exercício 1.8 Faça um novo tipo chamado Mes que possui como valores todos
--os meses do ano. Implemente1.4 Exercícios 7
--1. a função checaFim que retorna o número de dias que cada mês possui (considere Fevereiro tendo 28 dias).
--2. a função prox que recebe um mês atual e retorna o próximo mês.
--3. a função estacao que retorna a estacao do ano de acordo com o mes e com
--o hemisfério. Use apenas tipos criados pela palavra data aqui.





--Exercício 1.9 Faça uma função que receba uma String e retorne True se esta for
--um palíndromo, False caso contrário.

reverccao :: String -> String
reverccao [] = []
reverccao (x:xs) = reverccao xs ++ [x]

palindromo' :: String -> Bool
palindromo' s = s == reverccao s

palindromo :: String -> Bool
palindromo s = s == reverse s

--Exercício 1.10 Faça uma função que elimine todos os números pares, todos os
--ímpares múltiplos de 7 e negativos de uma lista de inteiros passado via parâmetro.
--Você deve retorna esta lista em ordem reversa em comparação a do parâmetro.

filtraLista :: [Int] -> [Int]
filtraLista [] = []
--filtraLista x = reverse [x , (mod x 7 == 0 && mod x 2 /= 0) == True, mod x 2 == 0]
filtraLista (x:xs)
    |(mod x 7 == 0 && mod x 2 /= 0) == True || (x < 0) == True = filtraLista xs
    |otherwise = filtraLista xs ++ [x]

--Exercício 1.11 Faça uma função que recebe três strings x, y e z como parâmetro.
--A função retorna uma tupla com três coordenadas contendo a ordem reversa em
--cada. A primeira coordenada deve conter string reversa do primeiro parâmetro e
--assim por diante.

juliana :: String -> String -> String -> (String, String, String)
juliana x y z = ((reverse x), (reverse y), (reverse z))

--Exercício 1.12 Gere as listas abaixo usando list compreenshions
-- [1,10,19,28,37,46,55,64]
-- [2,4,8,10,12,16,18,22,24,28,30]
-- [’@’,’A’,’C’,’D’,’E’,’G’,’J’,’L’]

contaEmNove :: [Int]
contaEmNove = [1, 10..64]

--Exercício 1.13 Faça uma função, chamada revNum, que receba uma String s e
--um Int n. Esta deverá retornar as n primeiras letras em ordem reversa e o restante
--em sua ordem normal.
--Exemplo:
--revNum 4 ”F AT EC” = ”ET AF C” (1.1)



--Exercício 1.14 Crie o tipo de dado Binario que pode ser Zero ou Um. 
--Crie também o tipo de dado Funcao que pode ser Soma2, Maior, Menor e Mult2. 
--Faça a função aplicar que recebe uma Funcao e dois Binarios seu retorno consiste em
--executar a operação desejada.
--Exemplo:
--aplicar Soma2 Um Um = Zero



--Exercício 1.15 Faça uma função, chamada binList, usando list compreeshion
--que recebe uma lista de Binarios (ver exercício acima) e retorna outra lista com
--elemento somado Um e convertido para Int.
--binList [Um, Zero, Zero, U m, Zero] = [0, 1, 1, 0, 1] (1.3)



{-
2.3 Exercícios
Exercício 2.1 Faça um novo tipo chamado Metros, que possui um value constructor
de mesmo nome cujos parâmetros são um Int que representa a dimensão e
um Double que representa o valor da medida e outro chamado MetragemInvalida.
Implemente as funções
1. areaQuadrado :: Metros ! Metros: Calcula a área de um quadrado
2. areaRet :: Metros ! Metros! Metros: Calcula a área de um retangulo
3. areaCubo :: Metros ! Metros: Calcula a área de um cubo. Exemplo,
areaQuadrado(Metros 1 2.0) = Metros 2 4.0. (2.1)
Use o pattern matching para ignorar as metragens erradas (cáclular a área de
um quadrado com um lado de dimensão 4 não é válido).
-}


{-
Exercício 2.2 (Validação de nomes) Faça o novo tipo Valido que possui dois
value constructor Sim e Nao. O value constructor Sim possui um parâmetro
(campo) String. Implemente uma função isNomeValido que recebe um nome
e retorna Nao caso a String seja vazia e Sim caso contrário.
Exercício 2.3 Refaça o exercício 3 do capítulo anterior usando record syntax e
tipos com parâmetro (siga o exemplo da conversão de medidas SI para imperial).
Exercício 2.4 Faça o tipo Numero, que possui um value constructor Ok com
um campo double e outro value constructor Erro com um campo String. Faça a
função dividir que divida dois números e caso o segundo número seja 0 emita um
erro (use o pattern matching). Exemplo,
dividir(Numero 6) (Numero 5) = Numero 1.2. (2.2)
-}
data Valido = Simm String | Naoo deriving Show

isNomeValido :: String -> Valido
isNomeValido "" = Naoo
isNomeValido x = Simm x 

{-
Exercício 2.5 Faça o tipo Cripto que possua dois value constructors Mensagem
e Cifrado ambos com um campo String e um value constructor Erro. Faça as
funções encriptar e decriptar seguindo cada exemplo a seguir
encriptar(Mensagem ”FATEC”) = Cifrado ”GBUFD” (2.3)
decriptar(Cifrado ”DBTB”) = Mensagem ”CASA”. (2.4)
12 2 Aula 3
OBS: a encriptação deve empurrar cada letra a frente e a decriptação, faz o inverso,
empurrando uma letra para trás. Use as funções succ e pred e também list
compreeshions. Não é possível encriptar mensagens cifradas e decriptar mensagens.
Exercício 2.6 Faça uma função encriptarTodos que encripta (ou dá erro) todos
os elementos de um vetor de Cripto.
Exercício 2.7 Tendo como base o exercício de conversão de medidas dado em
aula, crie uma função que faça conversão de câmbio. Você deve criar o tipo Cambio
contendo os value constructors Euro, Real e Dollar. Crie também o tipo Moeda
que possui os campos (val :: Double) e (cur :: Cambio). Use record
syntax e as taxas de conversão do dia ao qual você fez o exercício (especifique
o dia por comentário).
Exercício 2.8 Crie a função converterTodosReal que recebe uma lista de Moedas
e retorna outra lista de Moedas com todos os seus elementos convertidos para Real.
Use list compreenshion.
Exercício 2.9 Crie a função maxMoeda que recebe uma lista de Moedas e retorna
o valor máximo absoluto(sem conversão alguma) dentre os campos val
desta lista. Exemplo,
maxMoeda [Moeda 3 Real , Moeda 7 Dollar , Moeda 2 Euro] = 7. (2.5)
OBS: Use a função maximum.
-}