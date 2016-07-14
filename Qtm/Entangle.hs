{-#LANGUAGE GADTs #-}

module Qtm.Entangle (Entangle (EnZero, (:&:)), esort, (+&), prEntangle, normalize, CoEntangle (EnZero', (:|:)), esort', (-|), prCoEntangle, normalize', (^**^))where

import Data.Complex
import Qtm.Entangle.KetEntangle
import Qtm.Entangle.BraEntangle

-- Inner Product of a Bra Entaglement and a Ket Entaglement
infix 4 ^**^
(^**^) :: (Num a, Eq a, Ord a, Show a, RealFloat b, Show b) => CoEntangle a (Complex b) -> Entangle (Complex b) a -> Complex b
(^**^) b k = product b k (0 :+ 0) where
  product EnZero' _ s = s
  product _  EnZero s = s
  product b@(bs :|: (ba, bb)) k@((ka, kb) :&: ks) s
    |ba < kb   = product bs k s 
    |ba > kb   = product b ks s
    |otherwise = product bs ks s + bb * ka
