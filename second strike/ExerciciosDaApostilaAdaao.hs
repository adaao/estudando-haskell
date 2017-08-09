module ExerciciosDaApostilaAdaao where
--1.4 Exercícios
--Exercício 1.1 Crie o tipo Pergunta (Resposta?) com os value constructors Sim ou Nao. Faça
--as funções abaixo determinando seus tipos explicitamente

data Answer = Yes | No deriving Show

--1. pergNum: recebe via parâmetro uma Pergunta e retorne 0 para Nao e 1
--para Sim;

pergNum :: Answer -> Int
pergNum Yes = 1
pergNum No = 0

--2. listPergs: recebe via parâmetro uma lista de Perguntas e retorna 0’s e 1’s
--correspondentes aos constructores contidos na lista;

lPerg :: [Answer] -> [Int]
lPerg answers = fmap pergNum answers

--3. and’: recebe duas Perguntas como parâmetro e retorna a tabela verdade do
--and lógico usando Sim como verdadeiro e Nao como falso.




--4. or’: Idem acima, porém, deve ser usado o ou lógico.


--5. not’: Idem aos anteriores, porém, usando o not lógico.

--Exercício 1.2 
-- 1 Gere as listas [1,11,121,1331,14641,161051,1771561] e
-- 2 [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,33,34,35,37,38,39] usando
--   list comprehension.

-- 1


--2


--Exercício 1.3 Faça o tipo Temperatura que pode ter valores Celsius, Farenheit
--ou Kelvin. Implemente as funções:


--Celsius p/ Fahrenheit °F = °C × 1,8 + 32 

--Fahrenheit p/ Celsius °C = (°F − 32) / 1,8 

--Celsius p/ Kelvin K = °C + 273,15 

--Kelvin p/ Celsius °C = K − 273,15 

--1. converterCelsius: recebe um valor double e uma temperatura e faz a conversão para Celsius.



--2. converterKelvin: recebe um valor double e uma temperatura e faz a conversão para Kelvin.



--3. converterFarenheit: recebe um valor double e uma temperatura e faz a conversão para Farenheit.

--converteFahrenheit :: Double -> Temperatura -> (Doueble, Temperatura)
--converteFahrenheit x 

--Exercício 1.4 Faça uma função que simule o vencedor de uma partida de pedra,
--papel e tesoura usando tipos criados (você não poderá usar qualquer outro tipo
--que não seja criado usando o data). Casos de empate devem ser considerados em
--seu tipo.



   

--Exercício 1.5 Faça uma função que retorne uma string com todas as vogais
--maiúsculas e minúsculas eliminadas de uma string passada por parâmetro usando
--list compreenshion.
--Dica: procure informações sobre a função elem.


--Exercício 1.6 Gere as listas
--1. ["AaBB", "AbBB", "AcBB", "AdBB", "AeBB", "AfBB", "AgBB"]
--2. [5,8,11,17,20,26,29,32,38,41]
--3. [1.0,0.5,0.25,0.125,0.0625,0.03125]
--usando list compreenshion.



--Exercício 1.7 Sabe-se que as unidades imperiais de comprimento podem ser
--Inch, Yard ou Foot (há outras ignoradas aqui). Sabe-se que 1in = 0.0254m,
--1yd = 0.9144m, 1ft = 0.3048. Faça a função converterMetros que recebe a 
--unidade imperial e o valor correspondente nesta unidade e retorna o valor em metros.
--Faça a função converterImperial que recebe um valor em metros e a unidade de
--conversão e retorna o valor convertido para a unidade desejada.

--http://learnyouahaskell.com/ -> acessar

--https://c9.io/romefeller

--https://c9.io/isagomes/haskell1




--Exercício 1.8 Faça um novo tipo chamado Mes que possui como valores todos
--os meses do ano. Implemente1.4 Exercícios 7
--1. a função checaFim que retorna o número de dias que cada mês possui (considere Fevereiro tendo 28 dias).
--2. a função prox que recebe um mês atual e retorna o próximo mês.
--3. a função estacao que retorna a estacao do ano de acordo com o mes e com
--o hemisfério. Use apenas tipos criados pela palavra data aqui.





--Exercício 1.9 Faça uma função que receba uma String e retorne True se esta for
--um palíndromo, False caso contrário.



--Exercício 1.10 Faça uma função que elimine todos os números pares, todos os
--ímpares múltiplos de 7 e negativos de uma lista de inteiros passado via parâmetro.
--Você deve retorna esta lista em ordem reversa em comparação a do parâmetro.


--filtraLista x = reverse [x , (mod x 7 == 0 && mod x 2 /= 0) == True, mod x 2 == 0]

--Exercício 1.11 Faça uma função que recebe três strings x, y e z como parâmetro.
--A função retorna uma tupla com três coordenadas contendo a ordem reversa em
--cada. A primeira coordenada deve conter string reversa do primeiro parâmetro e
--assim por diante.



--Exercício 1.12 Gere as listas abaixo usando list compreenshions
-- [1,10,19,28,37,46,55,64]
-- [2,4,8,10,12,16,18,22,24,28,30]
-- [’@’,’A’,’C’,’D’,’E’,’G’,’J’,’L’]



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


{-
Exercício 3.1 Faça uma função que retorne a média de um [Double] usando
foldl.


Exercício 3.2 Faça uma função que retorne o desvio padrão
r de um [Double]
usando foldl. O desvio padrão de um vetor é dado por σ = (ver formula na apostila)
é a média deste vetor.

P n (x i −x) 2, n−1 i=1

onde x é a media desse valor.

Exercício 3.3 Refaça o exercício 1.15 usando map.

Exercício 3.4 Faça uma função que receba um [String] e retorne todos ele-
mentos palíndromos. Ver exercício 1.9.

Exercício 3.5 Refaça o exercício 2.8 usando map.

Exercício 3.6 Usando o exercício 2.7 como base, faça uma função que retorna
todas as moedas com o campo val valendo Euro.

Exercício 4.1 Usando a estrtura de árvore, monte uma função mapA, que jogue
uma função passada por parâmetro para todos os elementos de uma árvore. Deixe
explícito o tipo desta função.
Exercício 4.2 Usando o exercício acima, some 5 a cada elemento de uma árvore
de inteiros.
Exercício 4.3 Uma lista ordenada é uma lista cujos elementos são inseridos de
forma ordenada (crescente). Usando o tipo
ListaOrd a = a :?: (ListaOrd a) | Nulo deriving Show
crie as funções
• inserir :: (Ord a) ⇒ a → ListaOrd a → ListaOrd a
• remover :: (Eq a) ⇒ a → ListaOrd a → ListaOrd a
• tamanho :: ListaOrd a → Int
A função remover deve buscar um elemento, se não achar a lista deve se manter
intacta.

Exercício 4.4 Usando a estrutura de árvore dada em aula, faça uma função que
some todos os elementos de uma árvore de números.
Exercício 4.5 Implemente os percursos pós-ordem e pré-ordem. Via comentário,
faça os ”testes de mesa” para os dois percursos usando as árvores:
• Da Figura 4.1.
• Raiz 15 (Raiz 11 (Folha 6) (Raiz 12 (Folha 10) Nula)) (Raiz 20 Nula (Raiz 22
(Folha 21) Nula))
Exercício 4.6 Faça uma função para inserir um elemento em uma árvore de
busca binária (use a mesma estrutura dada em aula).
Exercício 4.7 Faça uma função que a partir de uma lista de elementos de tipo
a insira, usando o exercício anterior, todos os elementos desta lista na árvore e
retorne-a.
ver imagem da arvore na apostila

Exercício 5.2 Crie uma função totalGeral que recebe uma Lista de Produtos e
Retorna o preço total deles usando a monóide acima.
Exercício 5.3 A função min no Haskell retorna o menor entre dois números,
por exemplo, min 4 5 = 4.
1. Crie um tipo Min com um campo inteiro que seja instância de Ord, Eq e
Show (deriving).
2. Crie uma instância de Monoid para Min (maxBound representa o maior
inteiro existente no Haskell).
3. Quanto vale a expressão Min (-32) <> Min (-34) <> Min (-33)?
4. Explique sua escolha para o mempty.
Exercício 5.4 Crie uma função minAll que recebe um [Min] e retorna um
Min contendo o menor valor.
Exercício 5.5 Crie o tipo Paridade com os value constructors Par e Impar. Crie
o typeclass ParImpar que contém a função
decide :: a → Paridade
e possua as instâncias
• Para Int: noção de Par/Impar de Int.
• Para [a]: Uma lista de elementos qualquer é Par se o número de elementos
o for.
• Para Bool: False como Par, True como Impar.
Exercício 5.6 A função max no Haskell retorna o maior entre dois números, por
exemplo, max 4 5 = 5.
1. Crie um tipo Max com um campo inteiro que seja instância de Ord, Eq e
Show (deriving).
2. Crie uma instância de Monoid para Max (minBound representa o menor
inteiro existente no Haskell).
3. Quanto vale a expressão Max 10 <> Max 13 <> Max 5?
4. Explique sua escolha para o mempty.
5. Crie uma função maxAll que recebe um [Max] e retorna um Max con-
tendo o maior valor.
Exercício 6.1 Faça um programa que receba um número inteiro e mostre na tela
se este é par ou ímpar usando a Monad IO.
Exercício 6.2 Faça uma instância de Functor para o tipo Coisa definido no exer-
cício ??. A função deve ser aplicada em todas as coordenadas de Coisa.
Exercício 6.3 Aproveitando o exercício anterior, faça uma instância de Applica-
tive Functor para o tipo Coisa definido no exercício ??. Para definir um Applicative
Functor é necessário definir seu elemento neutro pure(TresCoisas) e seu opera-
dor <*>. A regra para <*> deve ser a distribuição das funções dos campos para
o campo do argumento de forma ordenada. Por exemplo,
DuasCoisas (+4) (+5) <*> DuasCoisas 2 1 =
ghci> DuasCoisas 6 6
(6.1)
Cada value constructor deve se combinar com o mesmo, caso contrário o valor
retornado é sempre Nada.
Referência: http://learnyouahaskell.com/functors-applicative-functors-and-monoids
Exercício 6.4 Crie uma instância de Monad para o tipo Coisa definido no exercí-
cio ??. Seu return deve ser o value constructor UmaCoisa. Referência: http://learnyouahaskell.com/a-
fistful-of-monads
Exercício 6.5 Crie uma função
mult234 :: Double → Coisa Double
que multiplica por 2 a primeira coordenada, por 3 a segunda e por 4 a terceira o
parâmetro x recebido
Exercício 6.6 Determine o valor das expressões (caso seja possível) abaixo:
1. TresCoisas 1 2 3 »= mult234
2. (*2) <$> DuasCoisas 2 4
3. :kind Coisa
4. DuasCoisas (*2) (*3) <*> DuasCoisas 3 4
5. (*2) <$> DuasCoisas 7 9
6. DuasCoisas (*2) (*3) <*> UmaCoisa 5
7. pure (*2) <*> DuasCoisas 4 5
8. pure (+) <*> DuasCoisas 1 2 <*> DuasCoisas 2 1
9. (*) <$> TresCoisas 1 2 3 <*> TresCoisas 1 2 3
10. (*) <$> (TresCoisas 1 2 3 »= mult234) <*> (TresCoisas 1 2 3 »= mult234)
Exercício 6.7 Faça um exemplo, usando a notação do, de um trecho qualquer
de código usando sua Monad Coisa.
Exercício 6.8 Escreva a função do exercício 6.5 em termos dos operadores Ap-
plicative.
Exercício 6.9 Escreva uma instância para Functor e Applicative Functor para
o tipo (Arvore a) visto na Aula 4, quadro 4.2. A regra para estas instâncias são
análogas (a menos de recursão).
Exercício 6.10 Crie 5 expressões usando o Applicative Functor de seu tipo Tree.
-}