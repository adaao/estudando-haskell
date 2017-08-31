module Aula3 where

data Correncia = Euro | Dollar | Real | Bitcoin deriving Show

-- Record Syntax: EH O ATO DE SE NOMEAR OS CAMPOS
-- DE UMA VALUE CONSTRUCTOR. OS NOMES DOS CAMPOS TB SE 
-- COMPORTAM COMO "GETS DA POO". ESSES SAO NOMES TB SAO 
-- CHAMADOS DE FUNCOES DE PROJECAO.
data Dinheiro = Dinheiro { valor :: Double,
                           curr  :: Correncia 
                         } deriving Show

                        
converterReal :: Dinheiro -> Dinheiro
converterReal (Dinheiro x Dollar) = Dinheiro (3.14*x) Real
converterReal (Dinheiro x Euro) = Dinheiro (3.71*x) Real
converterReal (Dinheiro x Bitcoin) = Dinheiro (13000*x) Real
converterReal x = x

dobrarDinheiro :: Dinheiro -> Dinheiro
dobrarDinheiro (Dinheiro v c) = Dinheiro (v*2) c

--record syntax
somarDinheiro :: Dinheiro -> Dinheiro -> Dinheiro
somarDinheiroReal din0 din1 = Dinheiro 
     (valor (converterReal din0) + valor (converterReal din1)) Real
