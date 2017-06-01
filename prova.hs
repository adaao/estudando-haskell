{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances, GADTs, TypeOperators, UndecidableInstances #-}
module Prova where

-- PEGADINHA!!!!

data Zero
data Succ a

infixl 6 :+
infixl 6 :*


type family nat1 :+ nat2 :: *
type family nat1 :* nat2 :: *

type instance Zero      :+ nat2 = nat2
type instance Succ nat1 :+ nat2 = Succ (nat1 :+ nat2)

type instance Zero :* nat2 = Zero
type instance Succ nat1 :* nat2 = (nat1 :* nat2) :+ nat2


data Nat nat where
    Zero ::            Nat Zero
    Succ :: Nat nat -> Nat (Succ nat)

infix 4 :==
data val1 :== val2 where
    Refl :: val1 :== val2

toNatural :: Nat nat -> Int
toNatural Zero       = 0
toNatural (Succ nat) = succ (toNatural nat)

instance Show (Nat nat) where
    show = show . toNatural

instance Show (val1 :== val2) where
    show Refl = "Refl"

rightUnit :: Nat nat -> nat :+ Zero :== nat
rightUnit Zero       = Refl
rightUnit (Succ nat) = case rightUnit nat of
                           Refl -> Refl

oner :: Nat nat -> nat :+ (Succ Zero) :== Succ nat
oner Zero       = Refl
oner (Succ nat) = case rightUnit nat of
                           Refl -> Refl

dobro :: Nat nat -> (Succ (Succ Zero)) :* nat :== nat :+ nat
dobro Zero       = Refl
dobro (Succ nat) = case rightUnit nat of
                           Refl -> Refl

