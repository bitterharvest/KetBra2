{-#LANGUAGE GADTs #-}

module Qtm.KetBra.Bra (Bra (Zero', BraZero, (:-:)), (^+), (^-), prBra, invB, invK) where

import Qtm.KetBra.Ket

infixl 5 :-:

data Bra a where
  Zero' :: Bra a  -- Don't use it when defining a Bra. It is only used through operatons.
  BraZero :: Bra a
  (:-:) :: Bra a -> a -> Bra a

invB :: (Eq a, Ord a) => Bra a -> Ket a
invB Zero' = Zero
invB BraZero = KetZero
invB (b :-: a)
  | b == BraZero = a :+: KetZero
  | otherwise    = a :+: (invB b) 

invK :: (Eq a, Ord a) => Ket a -> Bra a
invK Zero = Zero'
invK KetZero = BraZero
invK (a :+: b)
  | b == KetZero = BraZero :-: a
  | otherwise    = (invK b) :-: a

instance (Eq a, Ord a) => Eq (Bra a)  where
    Zero' == Zero' = True
    Zero' == _    = False
    _     == Zero' = False
    BraZero == BraZero = True
    BraZero == _       = False
    _       == BraZero = False
    (a :-: b) == (c :-: d) = (a == c) && (b == d)

instance (Show a, Read a, Eq a, Ord a) => Show (Bra a)  where
    showsPrec _ Zero'   = showString "0"
    showsPrec _ BraZero   = showString "< ...00(0)00... |"
    showsPrec n (a :-: b)   = showsPrec n a . showString "-a" . showsPrec n b . showString "," 
prBra  :: (Eq a, Ord a, Num a, Show a) => Bra a -> String
prBra Zero' = "0"
prBra b = "<" ++ c ++ "|"
  where 
    c =  init $ tail $ prKet (invB b)  

infixl 5 ^-
(^-) :: (Eq a, Ord a) => Bra a -> a -> Bra a
(^-) Zero' _ = Zero'
(^-) b a = invK $ a :+: (invB b)

infixl 5 ^+
(^+) :: (Eq a, Ord a) => Bra a -> a -> Bra a
(^+) Zero' _ = Zero'
(^+) b a 
  | b' == c = Zero'
  | otherwise = (invK c) 
    where
      b' = invB b
      c = delete a b'
      delete :: (Eq a) => a -> Ket a -> Ket a
      delete a KetZero = KetZero
      delete a (x :+: xs) 
        | a == x = xs 
        | otherwise = x :+: delete a xs 

