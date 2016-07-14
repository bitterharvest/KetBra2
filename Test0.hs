{-#LANGUAGE GADTs #-}

module Test0 where

import Qtm.KetBra

a = KetZero
b = 2 :+: a
c = 3 :+: b
d = 0 :+: c

e = 4 -^ 3 -^ 2 +^ 4 +^ 0 +^ 3 +^ KetZero

a' = BraZero
b' = a' :-: 2
c' = b' :-: 3
d' = c' :-: 0

e' = BraZero ^- 4 ^- 3 ^- 2 ^+ 4 ^- 0 ^+ 3

f = e' ^*^ e

g = e' ^*^ a