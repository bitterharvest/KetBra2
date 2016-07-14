{-#LANGUAGE GADTs #-}

module Qtm.KetBra (Ket (KetZero, (:+:)), (+^), (-^), prKet, Bra (BraZero, (:-:)), (^+), (^-), prBra, (^*^), invK, invB)  where

import Qtm.KetBra.Ket
import Qtm.KetBra.Bra

infix 4 ^*^
(^*^) :: (Eq a, Ord a, Num a, Show a) => Bra a -> Ket a -> a
(^*^) b k
  | b == Zero' = 0
  | k == Zero = 0
  | prKet (invB b) == prKet k = 1
  | otherwise = 0
