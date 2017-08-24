module Exercicio313 where

{-
3.13)
  Faça  um  novo  tipo  chamado 
 Metros ,  que  possui  um  \textit{value  constructor} 
   de   mesmo   nome,   cujos parâmetros  são:  um 
 Int que  representa  a  dimensão,  e  um Double 
  que  representa  o  valor  da  medida  e  outro  chamado
 MetragemInvalida . Implemente as funções:
-}

data Metros = Metros { dimensao :: Int,
                       medida :: Double
                     } | MetragemInvalida deriving Show

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
Exemplo:
Prelude> areaQuadrado (Metros 1 2.0) 
Metros 2 4.0
Use  o pattern  matching para  ignorar  as  metragens  erradas
 (calcular a área de um quadrado com um lado de dimensão 4 não é válido).
-}

