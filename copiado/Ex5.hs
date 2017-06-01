{-
Podem ser feitos, via ghci (lambdas e/ou let), ou via programa 
a) Filtre os numeros pares de uma lista
b) Filtre os ímpares de uma lista
c*) Filtre os números primos de uma lista (Fazer uma funcao de checar primos)
-}
isPrimo :: Int -> Bool
isPrimo 1 = True
isPrimo 2 = True
    |
{-
d) Faça uma função que receba uma lista de inteiros e retorne
o dobro de todos eliminando os múltplos de 4
e) Faça uma função que receba uma função (String -> String)
e uma String e retorne o reverso da String que consiste
na aplicação da função recebida no outro parâmetro
f) Monte um currying da função
hip :: Double -> Double -> Double
hip x y = sqrt (x*x + y*y)
g) Dê um jeito de aplicar a função acima em uma lista de Doubles
h) Elimine todas as hipotenusas que não dê 1 de uma lista
(Dê seu jeito!)
i) Faça um tipo Dia contendo os dias da semana. Faca uma
função que receba uma lista de Dias e filtre as Terças.
j) Faça o tipo Dinheiro que contenha os campos valor
e correncia (Real ou Dolar) e uma função que converta
todos os dinheiros de uma lista para dolar (e outra
para real.)
k) Filtre todos os Dolares de uma lista de Dinheiro.

l) Faça um fold para somar todos os Dolares da lista.
m) Faça um fold para contar o número de Dolares da lista.
n) Faça um fold para contar negativos
o) Faça um fold para contar letras 'P'
p) Faça um fold para contar Sabados em uma lista
de [DiaSemana].
q) Faça um fold para contar Trues de uma lista de Bool
r) Faça um fold para concatenar o reverso de 
palavras de uma lista de Strings
s) Faça um fold que a partir de uma lista de
[DiaSemana] retorne a soma dos dias. Exemplo
[Segunda, Segunda, Quarta] deve retornar 5.
-}

printar :: String
printar = "tes \n te"


