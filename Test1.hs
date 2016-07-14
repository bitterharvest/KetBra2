{-#LANGUAGE GADTs #-}

module Test1 where

import Data.Complex
import Qtm.KetBra
import Qtm.Entangle


a = EnZero
b = (5.3 :+ 4.2, 1) :&: a
c = (3.3 :+ 3.2, 3) :&: b
d = (1.3 :+ 1.2, 2) :&: c


a' = EnZero'
b' = a' :|: (1, conjugate(5.3 :+ 4.2))
c' = b' :|: (3, conjugate(3.3 :+ 3.2))
d' = c' :|: (2, conjugate(1.3 :+ 1.2))


g = (conjugate(3.3 :+ 3.2), 1) :&: (conjugate(3.1 :+ 0.2), 4) :&: (conjugate(2.3 :+ 1.2), (-1)) :&: ((4.3 :+ 3.3), 2) :&: ((3.3 :+ 0.2), 3) :&: EnZero
h = (conjugate(2.3 :+ 0.2), 3) :&: (conjugate(3.4 :+ 2.2), (-2)) :&: (conjugate(5.3 :+ 1.5), (-1)) :&: ((1.3 :+ 0.3), 1) :&: ((3.2 :+ 2.2), 2) :&: EnZero

p = esort g
q = esort h

r = g +& h
t = normalize r

g' = EnZero' :|: (1, (3.3 :+ 3.2)) :|: (4, (3.1 :+ 0.2)) :|: (-1, (2.3 :+ 1.2)) :|: (2, conjugate(4.3 :+ 3.3)) :|: (3, conjugate(3.3 :+ 0.2))
h' = EnZero' :|: (3, (2.3 :+ 0.2)) :|: (-2, (3.4 :+ 2.2)) :|: (-1, (5.3 :+ 1.5)) :|: (1, conjugate(1.3 :+ 0.3)) :|: (2, conjugate(3.2 :+ 2.2))

p' = esort' g'
q' = esort' h'

r' = g' -| h'
t' = normalize' r'

u = g' ^**^ g
v = h' ^**^ h
w = t' ^**^ t
