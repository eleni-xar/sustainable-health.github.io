module Arithmetic where

data Nat 
  = Zero 
  | Next Nat
  deriving Show

isZero :: Nat -> Bool
isZero Zero = True
isZero _ = False

prev :: Nat -> Nat
prev Zero = Zero
prev (Next n) = n

add :: Nat -> Nat -> Nat
add m n =
  if isZero n
    then m
    else add (Next m) (prev n)

add' :: Nat -> Nat -> Nat
add' m Zero = m
add' m (Next n) = add (Next m) (prev n)

isZero' n = 
  if isZero' n 
    then True
    else False
