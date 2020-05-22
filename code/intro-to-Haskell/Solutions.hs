module Solutions where

import Arithmetic (Nat, add)


-- Exercise 1

times2 :: Nat -> Nat -- multiplies n with 2
times2 n = add n n

times4 :: Nat -> Nat -- multiplies n with 4
times4 n = add (times2 n) (times2 n) 

times8 :: Nat -> Nat -- multiplies n with 8
times8 n = add (times4 n) (times4 n) 

times16 :: Nat -> Nat -- multiplies n with 16
times16 n = add (times8 n) (times8 n)

-- If we continue like this we can define functions that multiply n with powers of 2.

times3 :: Nat -> Nat -- multiplies n with 3
times3 n = add n (times2 n)

times5 :: Nat -> Nat -- multiplies n with 5
times5 n = add (times3 n) (times2 n)

times6 :: Nat -> Nat -- multiplies n with 6
times6 n = add (times3 n) (times3 n)

times7 :: Nat -> Nat -- multiplies n with 7
times7 n = add (times3 n) (times4 n)

{-- There are multiple definitions for the above functions. For example, we could have written:

times7 :: Nat -> Nat -- multiplies n with 7
times7 n = add (times5 n) (times2 n)

or 

times7 :: Nat -> Nat -- multiplies n with 7
times7 n = add (times3 n) (add (times2 n) (times2 n))
Any way of breaking down 7 as a sum of two or more smaller numbers would have worked.
--}



-- Exercise 2

data Parity 
  = Odd
  | Even
  deriving Show

parity :: Nat -> Parity
parity Zero = Even
parity (Next Zero) = Odd
parity (Next (Next n)) = parity n



-- Exercise 3

{-- There are four possible functions of type Parity -> Parity. That is
because the type Parity has 2 values: Even and Odd. A function can  either 
1.leave the values unchanged;
2. turn them all to Even
3. turn them all to Odd
4. reverse them, turning Odd to Even and vice versa.
--}


parityId :: Parity -> Parity -- the identity function leaves its argument unchanged
parityId Odd = Odd
parityId Even = Even

allOdd :: Parity -> Parity -- turns everything to odd
allOdd _ = Odd

allEven :: Parity -> Parity -- turns everything to odd
allEven _ = Even

reverseParity :: Parity -> Parity -- turns Even to Odd and vice versa
reverseParity Odd = Even
reverseParity Even = Odd


{-- Similarly, we have 4 functions of type Bool -> Bool 

            Possible results
Arguments |        |
False     | False  |  True
True      |  False |  True

The first row states that there are two possible values that we can assign to
the argument False: either False or True. Another way to say this is: That
there are two possible values, True and False, to which we can map False.The
second row makes a similar statement about True.

So, if we choose to map False to Flase then we have two choices for the result
of True. Likewise, if we map False to True, we again have two choices for the
result of True. Thus, in total, there four different options. --}

boolId :: Bool -> Bool -- the identity function leaves its argument unchanged
boolId False = False
boolId True = True

allFalse :: Bool -> Bool -- turns everything to False
allFalse _ = False

allTrue :: Bool -> Bool -- turns everything to odd
allTrue _ = True

reverseBool :: Bool -> Bool -- turns Even to Odd and vice versa
reverseBool False = True
reverseBool True = False

{-- Haskell already has an in-built version of the last function called not.
--}



-- Execrise 4

parity' :: Nat -> Parity
parity' Zero = Even
parity' (Next n) = reverseParity (parity' n)




-- Exercise 5

isEven :: Nat -> Bool
isEven Zero = True
isEven (Next Zero) = False
isEven (Next (Next n)) = isEven n

isEven' :: Nat -> Bool
isEven' Zero = True
isEven' (Next n) = reverseBool (isEven' n)

-- You can use not instead of reverseBool



-- Exercise 6

isOdd :: Nat -> Bool
isOdd n = not (isEven n)



-- Exercise 7

nextOdd :: Nat -> Nat
nextOdd n =
  if isEven n 
    then Next n
    else Next (Next n)

nextOdd' :: Nat -> Nat
nextOdd' n =
  if isOdd n 
    then Next (Next n)
    else Next n

{--We only need isEven or isOdd because else takes care of the other case.--}

nextEven :: Nat -> Nat
nextEven n =
  if isOdd n 
    then Next n
    else Next (Next n)

nextEven' :: Nat -> Nat
nextEven' n =
  if isEven n 
    then Next (Next n)
    else Next n

nextOfSameParity :: Nat -> Nat
nextOfSameParity n = Next (Next n)

-- In this case the result is the same independently of whether the argument
-- is even or odd. So we don't have to use the conditional. If we had then
-- both options, the one after then and the one after else, would have been
-- the same.
