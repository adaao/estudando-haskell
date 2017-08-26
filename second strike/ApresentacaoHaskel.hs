module Exercicios where

--3.1) 
-- Crie o tipo Pergunta com os values constructors Sim ou Nao.
-- Faça as funções seguintes, determinando seus tipos explicitamente.

--a)pergNum : recebe via parâmetro uma Pergunta .
--  Retorna 0 para Nao e 1 para Sim .

data Pergunta = Sim | Nao deriving (Show)

pergNum::Pergunta -> Int
pergNum  Sim = 1
pergNum  Nao = 0

--b)listPergs : recebe via parâmetro uma lista de
--Perguntas , e retorna 0s e 1s correspondentes aos
--constructores contidos na lista.
listPergs:: [Pergunta] -> [Int]
listPergs xs = [pergNum x | x <- xs ]


--3. and’: recebe duas Perguntas como parâmetro e retorna a tabela verdade do
--and lógico usando Sim como verdadeiro e Nao como falso.
perAnd:: Pergunta -> Pergunta -> Bool
perAnd Sim Sim = True  && True
perAnd Sim Nao = True  && False
perAnd Nao Nao = False && False
perAnd Nao Sim = False && True 

--4. or’: Idem acima, porém, deve ser usado o ou lógico.
perOr:: Pergunta -> Pergunta -> Bool
perOr Sim Sim = True  || True
perOr Sim Nao = True  || False
perOr Nao Nao = False || False
perOr Nao Sim = False || True

--5. not’: Idem aos anteriores, porém, usando o not lógico.
perNot:: Pergunta -> Pergunta -> Bool
perNot Sim Sim = not (True  && True)
perNot Sim Nao = not (True  && False)
perNot Nao Nao = not (False && False)
perNot Nao Sim = not (False && True)
--------------------------------------------------------------------------------
{-
 3.6) Faça um novo tipo chamado Mes , que possui como
 valores todos os meses do ano. Implemente:
 a) A função checaFim , que retorna o número de dias
    que cada mês possui (considere fevereiro tendo 28
    dias).
 b) A função prox , que recebe um mês atual e retorna o
    próximo mês.
 c) A função estacao , que retorna a estação do ano de
    acordo com o mês e com o hemisfério.
-}

--a)
data Mes = Jan | Fev | Mar | Abr | Mai | Jun |
           Jul | Ago | Set | Out | Nov | Dec deriving Show

checaFim::Mes -> Int
checaFim Jan = 31
checaFim Fev = 28
checaFim Mar = 31
checaFim Abr = 30
checaFim Mai = 31
checaFim Jun = 30
checaFim Jul = 31
checaFim Ago = 31
checaFim Set = 30
checaFim Out = 31
checaFim Nov = 30
checaFim Dec = 31		

--b)
prox::Mes->Mes
prox Jan = Fev
prox Fev = Mar
prox Mar = Abr
prox Abr = Mar
prox Mai = Jun
prox Jun = Jul
prox Jul = Ago
prox Ago = Set
prox Set = Out
prox Out = Nov
prox Nov = Dec
prox Dec = Jan

--c)
data Estac = Verao | Primavera | Outono | Inverno deriving Show
data Emisf = Norte | Sul     deriving Show
data Busca = Busca Mes Emisf deriving Show

estacao::Busca->Estac
estacao (Busca Jan Sul) = Verao
estacao (Busca Fev Sul) = Verao
estacao (Busca Mar Sul) = Verao
estacao (Busca Abr Sul) = Outono
estacao (Busca Mai Sul) = Outono
estacao (Busca Jun Sul) = Outono
estacao (Busca Jul Sul) = Inverno
estacao (Busca Ago Sul) = Inverno
estacao (Busca Set Sul) = Inverno
estacao (Busca Out Sul) = Primavera
estacao (Busca Nov Sul) = Primavera
estacao (Busca Dec Sul) = Primavera
estacao (Busca Jan Norte) = Inverno
estacao (Busca Fev Norte) = Inverno
estacao (Busca Mar Norte) = Inverno
estacao (Busca Abr Norte) = Primavera
estacao (Busca Mai Norte) = Primavera
estacao (Busca Jun Norte) = Primavera
estacao (Busca Jul Norte) = Verao
estacao (Busca Ago Norte) = Verao
estacao (Busca Set Norte) = Verao
estacao (Busca Out Norte) = Outono
estacao (Busca Nov Norte) = Outono
estacao (Busca Dec Norte) = Outono
--------------------------------------------------------------------------------
{-
3.13)
  Faça  um  novo  tipo  chamado 
 Metros ,  que  possui  um  \textit{value  constructor} 
   de   mesmo   nome,   cujos parâmetros  são:  um 
 Int que  representa  a  dimensão,  e  um Double 
  que  representa  o  valor  da  medida  e  outro  chamado
 MetragemInvalida .
-}

data Metros = Metros { dimensao :: Int,
                       medida :: Double
                     } | MetragemInvalida deriving Show

-- Implemente as funções:
{-
 areaQuadrado  ::  Metros  ->  Metros :  
 calcula  a área de um quadrado.
-}
  
areaQuadrado :: Metros -> Metros
areaQuadrado (Metros d m)
    |m <= 0 = MetragemInvalida
    |d > 3 || d < 0 = MetragemInvalida
    |otherwise = Metros d (m*m)

{-
areaQuadrado::Metros->Metros
areaQuadrado (Metros d m) 
  | ((d > 0 && d < 3) && (m > 0)) = Metros d (m*m)
  | otherwise = MetragemInvalida
-}

{-
  areaRet  ::  Metros  ->  Metros  ->  Metros :
calcula a área de um retângulo.
-}

areaRet :: Metros -> Metros -> Metros
areaRet metros1 metros2
    |(dimensao metros1) == (dimensao metros2) = MetragemInvalida
    |(medida metros1 <= 0) || (medida metros2 <=0) = MetragemInvalida
    |(dimensao metros1 < 0 || dimensao metros1 > 3) = MetragemInvalida
    |(dimensao metros2 < 0 || dimensao metros2 > 3) = MetragemInvalida
    |otherwise = (Metros 1 (medida metros1 * medida metros2))
    
{-
 areaCubo :: Metros -> Metros : 
 calcula a área de um cubo.
-}

areaCubo :: Metros -> Metros
areaCubo (Metros d m)
    |m <= 0 = MetragemInvalida
    |d > 3 || d < 0 = MetragemInvalida
    |otherwise = Metros d (m*m*m)

{-    
areaCubo :: Metros -> Metros
areaCubo (Metros d m)
    |((d > 0 && d <=3) && (m > 0)) = Metros d (m*m*m)
    |otherwise = MetragemInvalida
-}
{-
Exemplo:
Prelude> areaQuadrado (Metros 1 2.0) 
Metros 2 4.0
Use  o pattern  matching para  ignorar  as  metragens  erradas
 (calcular a área de um quadrado com um lado de dimensão 4 não é válido).
-}