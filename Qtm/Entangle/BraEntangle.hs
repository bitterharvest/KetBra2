{-#LANGUAGE GADTs #-}

module Qtm.Entangle.BraEntangle (CoEntangle (EnZero', (:|:)), esort', (-|), prCoEntangle, normalize')  where

import Qtm.KetBra.Bra
import Data.Complex

infixl 5 :|:

data CoEntangle a b 
  where
    EnZero'  :: CoEntangle a b
    (:|:) :: CoEntangle a b -> (a, b) -> CoEntangle a b

esort' :: (Eq a, Ord a) => CoEntangle a b -> CoEntangle a b
esort' EnZero' = EnZero'
esort' (xs :|: (xa,xb)) = esort' (larger xs) ||| (EnZero' :|: (xa,xb)) ||| esort' (smaller xs)
    where 
      smaller xs = smaller' xs EnZero'
      smaller' EnZero' s    = s 
      smaller' (ys :|: (ya, yb)) s = if ya < xa then smaller' ys (s :|: (ya, yb)) else smaller' ys s 
      larger xs  = larger' xs EnZero'
      larger'  EnZero' s    = s
      larger'  (ys :|: (ya, yb)) s = if ya >= xa then larger' ys (s :|: (ya, yb)) else larger' ys s

infixl 5 |||
(|||) :: CoEntangle a b -> CoEntangle a b -> CoEntangle a b
(|||) a b = connect a b
    where 
      connect a EnZero' = a
      connect a (xs :|: x) = connect a xs :|: x

infixl 5 -|
(-|) :: (Eq a, Ord a, Num b) => CoEntangle a b -> CoEntangle a b -> CoEntangle a b
(-|) x y = esort' $ (-||) (esort' x) (esort' y) EnZero'
    where
      infixl 5 -||
      (-||) :: (Eq a, Ord a, Num b) => CoEntangle a b -> CoEntangle a b -> CoEntangle a b -> CoEntangle a b
      (-||) x EnZero' s = x ||| s
      (-||) EnZero' y s = y ||| s
      (-||) (xs :|: (xa,xb)) (ys :|: (ya,yb)) s
        | xa < ya = (-||) xs (ys :|: (ya,yb)) (s :|: (xa,xb))
        | xa > ya = (-||) (xs :|: (xa,xb)) ys (s :|: (ya,yb))
        | otherwise = (-||) xs ys (s :|: (xa, xb + yb))

instance (Eq a, Eq b, Ord b) => Eq (CoEntangle a b)  where
    EnZero' == EnZero'                  = True
    EnZero' == _                        = False
    _ == EnZero'                        = False
    a :|: (ba, bb) == c :|: (da, db) :|: d = (a == c) && (ba == da) && (bb == db)


instance (Show a, Read a, Eq a, Ord a, Show b, Read b, Eq b) => Show (CoEntangle a b)  where
    showsPrec _ EnZero' = showString ""
    showsPrec n (EnZero' :|: (xa, xb)) = showString "a" . showsPrec n xa . showString "_" . showsPrec n xb
    showsPrec n (x :|: (ya, yb)) = showsPrec n x . showString " + " . showString "a" . showsPrec n ya . showString "_" . showsPrec n yb


prCoEntangle :: (Show a, Num a, Eq a, Ord a, Show b, Num b) => CoEntangle a b -> String
prCoEntangle EnZero' = ""
prCoEntangle (EnZero' :|: (xa, xb)) = prBra (liftBra xa) ++ show xb
prCoEntangle (y :|: (xa, xb)) =  prCoEntangle y ++ " + " ++ prBra (liftBra xa) ++ show xb

liftBra :: a -> Bra a
liftBra a = BraZero :-: a

normalize'  :: (RealFloat b) => CoEntangle a (Complex b) -> CoEntangle a (Complex b)
normalize' EnZero' = EnZero'
normalize' x = normalize'' x
    where
      total = sqrt $ sum x 0
      sum ::  (RealFloat b) => CoEntangle a (Complex b) -> b -> b
      sum EnZero' s = s
      sum (x :|: (ya, yb)) s = sum x $ (magnitude yb) * (magnitude yb)  + s
      normalize'' EnZero' = EnZero'
      normalize'' (x :|: (ya, yb)) = normalize'' x :|: (ya, (realPart yb) / total :+ (imagPart yb) / total)

