module ExerciciosDoLivro where
-- aqui foram desenvolvidos somente os primeiros exercicios do livro,
-- para uma melhor organizacao os proximos seram divididos de acordo
-- com os capitulos!!!!
  a21 = [11^x | x <- [0..6]]

  b21 = [x | x <- [1..40], mod x 4 /= 0]
  
c21 = [['A'] ++ [x] ++ ['B','B'] | x <- ['a'..'g']]
 -- c21 = ["A" ++ x ++ "BB" | x <- ["a".."g"]]
 --resposta acima esta correta :p

  d21 = [5, 8..41]

--  e21 =

  f21 = [1, 10..64]

  g21 = [x | x <- [2, 4..30], notElem x [6, 14, 20, 26]]
