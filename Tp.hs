module Tp where

-- ALUNOS: ALEXANDRE, ALINE E ERIK
-- DATA: 23/03/2017

-- EX1: Crie o tipo chamado "Lista" que possui um type parameter "a". Este tipo deve possuir 3 value constructors:
-- Nada
-- Um Elemento tendo um campo de tipo "a"
-- Dois Elementos tendo dois campos de tipo "a"

data Lista a = Nada | UmElemento a | DoisElementos a a deriving Show


-- a) Cria a funcao lmap que receba uma funcao tipo (a -> b) e uma (Lista a) como Parâmetro. Esta função deverá
-- retornar uma (Lista b) Quando o argumento de tipo (Lista a) for Nada, lmap deverá retornar Nada. Quando o argumento
-- for UmElemento, a função deverá ser calculada no valor de tipo "a"; Quando for Dois Elementos, a função deve ser calculada
-- nos dois valores (use pattern matching).

--dobrar x = x * 2
  
lmap :: (a -> b) -> (Lista a)           ->       (Lista b)
lmap   _            Nada                =         Nada
lmap   funcao       (UmElemento x)      =         UmElemento (funcao x)
lmap   funcao       (DoisElementos x y) =         DoisElementos ( funcao x) (funcao y) 

dobrarValor :: Int -> Int
dobrarValor x = 2 * x

-- b) Use a função acima para dobrar todos os campos de uma Lista de tipo Int