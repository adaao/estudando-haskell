module Contas where
v::Double
v = 202

juros::[Double]
juros = [0.1847, 0.0249, 0.1026, 0.0743, 0.002460, 0.002460, 0.002460, 0.002460]

calculoJuros::[Double]->Double->[Double]
calculoJuros j val = fmap (*val) j

calc::Double
calc = foldl (+) v (calculoJuros juros v)

jurosDia::[Double]->Double
jurosDia j = (foldl (+) 0 j) / 30

