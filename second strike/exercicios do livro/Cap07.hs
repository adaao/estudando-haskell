module Cap07 where

--module	Cap07 ( Contravariant(..))	where	

data Predicado a =  Predicado	{runPred ::	a -> Bool} 


ehMenor4	::	Predicado	Int
ehMenor4	=	Predicado	(\x	->	x	<	4)

tamanhoOito	::	Predicado	String
tamanhoOito	=	Predicado	(\x	->	length	x	==	8)


class Contravariant f where
    contramap :: (a -> b) -> f b -> f a
    
instance Contravariant Predicado where
    contramap g (Predicado p) = Predicado (p . g)

{-
class	Contravariant	f	where
contramap	::	(a	->	b)	->	f	b	->	f	a
--(>$) :: b -> f b -> f a
--(>$) = contramap . const


class Contravariant Predicado where 
  contramap	::	(a	->	b)	->	Predicado	b	->	Predicado	a
  (>$) :: b -> Predicate b -> Predicate a 


instance Contravariant Predicado where    
contramap  g (Predicado	p) = Predicado (p.g)
-}