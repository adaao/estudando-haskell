module Aula7 where

-- TYPE CLASS: IMPOEM RESTRICOES A TIPOS GENERICOS.
-- VOCE SOH CONSEGUIRA TRABALHAR COM AS FUNCOES DEFINIDAS
-- NO TYPECLASS E MAIS NADA.

-- TYPE PARAMETER: ESTE PARAMETRO, QUE EH UM TIPO, DEFINIRA
-- O COMPORTAMENTO DE CARTEIRA.
-- Ex: Carteira String, Carteira Int, Carteira Bool
data Carteira a = Nada
                | UmItem a
                | DoisItens a a deriving Show

data Moeda = Euro | Real | Dollar

-- SE QUISEREMOS FAZER UMA IMPLEMENTACAO DE Show
-- DIFERENTE DA PROPOSTA PELA LINGUAGEM, DEVEMOS:
-- a) Tirar o deriving
-- b) Prover uma instancia
instance Show Moeda where
    show Euro = "Que chique..."
    show Dollar = "Que legal..."
    show Real = "Que ..."

instance Eq Moeda where
    Dollar == Euro   = True
    Euro   == Dollar = True
    Real   == Real   = True
    Dollar == Dollar = True
    Euro == Euro     = True
    _ == _           = False

instance Eq a => Eq (Carteira a) where
    Nada == Nada = True
    (UmItem x) == (UmItem y) = x == y
    (DoisItens x1 x2) == (DoisItens y1 y2) =  (x1 == y1 && x2 == y2)
                                           || (x2 == y1 && x1 == y2)
    _ == _ = False
