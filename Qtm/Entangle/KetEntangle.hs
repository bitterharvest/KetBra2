{-#LANGUAGE GADTs #-}

module Qtm.Entangle.KetEntangle (Entangle (EnZero, (:&:)), esort, (+&), prEntangle, normalize) where

import Qtm.KetBra.Ket
import Data.Complex

infixr 5 :&:

data Entangle a b 
  where
    EnZero  :: Entangle a b
    (:&:) :: (a, b) -> Entangle a b -> Entangle a b

esort :: (Eq b, Ord b) => Entangle a b -> Entangle a b
esort EnZero = EnZero
esort ((xa,xb) :&: xs) = esort (smaller xs) &&& ((xa,xb) :&: EnZero) &&& esort (larger xs)
    where 
      smaller xs = smaller' xs EnZero
      smaller' EnZero s    = s 
      smaller' ((ya, yb) :&: ys) s = if yb < xb then smaller' ys ((ya, yb) :&: s) else smaller' ys s 
      larger xs  = larger' xs EnZero
      larger'  EnZero s    = s
      larger'  ((ya, yb) :&: ys) s = if yb >= xb then larger' ys ((ya, yb) :&: s) else larger' ys s
infixr 5 &&&
(&&&) :: Entangle a b -> Entangle a b -> Entangle a b
(&&&) a b = connect a b
    where 
      connect EnZero b = b
      connect (x :&: xs) b = x :&: connect xs b

infixr 5 +&
(+&) :: (Num a, Eq b, Ord b) => Entangle a b -> Entangle a b -> Entangle a b
(+&) x y = esort $ (+&&) (esort x) (esort y) EnZero
    where
      infixr 5 +&&
      (+&&) :: (Num a, Eq b, Ord b) => Entangle a b -> Entangle a b -> Entangle a b -> Entangle a b
      (+&&) x EnZero s = x &&& s
      (+&&) EnZero y s = y &&& s
      (+&&) ((xa,xb) :&: xs) ((ya,yb) :&: ys) s
        | xb < yb = (+&&) xs ((ya,yb) :&: ys) ((xa,xb) :&: s)
        | xb > yb = (+&&) ((xa,xb) :&: xs) ys ((ya,yb) :&: s)
        | otherwise = (+&&) xs ys ((xa + ya,xb) :&: s)

instance (Eq a, Eq b, Ord b) => Eq (Entangle a b)  where
    EnZero == EnZero                   = True
    EnZero == _                        = False
    _ == EnZero                        = False
    (aa, ab) :&: b == (ca, cb) :&: d = (aa == ca) && (ab == cb) && (b == d)


instance (Show a, Read a, Eq a, Show b, Read b, Eq b, Ord b) => Show (Entangle a b)  where
    showsPrec _ EnZero   = showString ""
    showsPrec n ((xa, xb) :&: EnZero)   = showsPrec n xa . showString "_a" . showsPrec n xb . showString "^"
    showsPrec n ((xa, xb) :&: y)   = showsPrec n xa . showString "_a" . showsPrec n xb . showString "^ + " . showsPrec n y

prEntangle :: (Show a, Num a, Show b, Num b, Eq b, Ord b) => Entangle a b -> String
prEntangle EnZero = ""
prEntangle ((xa, xb) :&: EnZero) = "(" ++ show xa ++ ")" ++ prKet (liftKet xb)
prEntangle ((xa, xb) :&: y) = "(" ++ show xa ++ ")" ++ prKet (liftKet xb) ++ " + " ++ prEntangle y

liftKet :: a -> Ket a
liftKet a = a :+: KetZero

normalize  ::  (RealFloat a) => Entangle (Complex a) b -> Entangle (Complex a) b
normalize EnZero = EnZero
normalize x = normalize'' x
    where
      total = sqrt $ sum x 0
      sum ::  (RealFloat a) => Entangle (Complex a) b -> a -> a
      sum EnZero s = s
      sum ((xa, xb) :&: y) s = sum y $ (magnitude xa) * (magnitude xa)  + s
      normalize'' EnZero = EnZero
      normalize'' ((xa, xb) :&: y) = ((realPart xa) / total :+ (imagPart xa) / total, xb) :&: normalize'' y
