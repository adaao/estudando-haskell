module ExerciciosDoLivro where
  a21 = [11^x | x <- [0..6]]

  b21 = [x | x <- [1..40], mod x 8 /= 0]

--  c21 = ["A" ++ x ++ "BB" | x <- ['a'..'g']]

  d21 = [5, 8..41]

--  e21 =

  f21 = [1, 10..64]

  g21 = [x | x <- [2, 4..30], notElem x [6, 14, 20, 26]]
