{-#LANGUAGE GADTs #-}

module Qtm.KetBra.Ket (Ket (Zero, KetZero, (:+:)), (+^), (-^), prKet) where

infixr 5 :+:
data Ket a where
  Zero :: Ket a  -- Don't use it when defining a Bra. It is only used when operating Ket data.
  KetZero :: Ket a
  (:+:) :: a -> Ket a -> Ket a

infixr 5 +^
(+^) :: (Eq a, Ord a) =>  a -> Ket a -> Ket a
(+^) _ Zero = Zero
(+^) a b = a :+: b

infixr 5 -^
(-^) :: (Eq a, Ord a) => a -> Ket a -> Ket a
(-^) _ Zero = Zero
(-^) a b
  | b == c = Zero
  | otherwise = c 
    where
      c = delete a b
      delete :: (Eq a) => a -> Ket a -> Ket a
      delete a KetZero = KetZero
      delete a (x :+: xs) 
        | a == x = xs 
        | otherwise = x :+: delete a xs 

instance (Eq a, Ord a) => Eq (Ket a)  where
    Zero == Zero = True
    Zero == _    = False
    _    == Zero = False
    KetZero == KetZero = True
    KetZero == _       = False
    _       == KetZero = False
    (a :+: b) == (c :+: d) = (a == c) && (b == d)

instance (Show a, Read a, Eq a, Ord a) => Show (Ket a)  where
    showsPrec _ Zero   = showString "0"
    showsPrec _ KetZero   = showString "| ...00(0)00... >"
    showsPrec n (a :+: b)   = showString ",a" . showsPrec n a . showString "+" . showsPrec n b

prKet :: (Eq a, Ord a, Num a, Show a) => Ket a -> String
prKet Zero = "0"
prKet KetZero = "|...000(0)000...>"
prKet a = "|..." ++ prKet' (ksort' $ ksort a) [] "right" 0 0 ++ "...>"
    where
      prKet' :: (Eq a, Ord a, Num a, Show a) => Ket a -> String -> String -> a -> a -> String
      prKet' KetZero s "left" _ num  = "000" ++ show num ++ s ++ "000" 
      prKet' KetZero s "right" _ num = "000" ++ s ++ show num ++ "000"
      prKet' (a :+: b) s dir index num
        | dir == "right" && index == a    = prKet' b s dir index (num + 1)
        | dir == "right" && index == 0    = prKet' (a :+: b) (s ++ "(" ++ show num ++ ")") "left" (index + 1) 0
        | dir == "right"                  = prKet' (a :+: b) (s ++ show num) "left" (index + 1) 0 
        | dir == "left"  && index == (-a) = prKet' b s dir index (num + 1)
        | otherwise                       = prKet' (a :+: b) (show num ++ s) "right" index 0 

ksort :: Ord a => Ket a -> Ket a
ksort KetZero = KetZero
ksort (x :+: xs) = ksort (smaller xs) +++ (x :+: KetZero) +++ ksort (larger xs)
    where 
      smaller xs = smaller' xs KetZero
      smaller' KetZero s    = reverse1 s 
      smaller' (y :+: ys) s = if y < x then smaller' ys (y :+: s) else smaller' ys s 
      larger xs  = larger' xs KetZero
      larger'  KetZero s    = reverse1 s
      larger'  (y :+: ys) s = if y >= x then larger' ys (y :+: s) else larger' ys s

ksort' :: (Num a, Ord a) => Ket a -> Ket a
ksort' KetZero = KetZero
ksort' (x :+: xs) = ksort' (smaller xs) +++ (x :+: KetZero) +++ ksort' (larger xs)
    where 
      smaller xs = smaller' xs KetZero
      smaller' KetZero s    = reverse1 s 
      smaller' (y :+: ys) s = if abs y < abs x then smaller' ys (y :+: s) else smaller' ys s 
      larger xs  = larger' xs KetZero
      larger'  KetZero s    = reverse1 s
      larger'  (y :+: ys) s = if abs y >= abs x then larger' ys (y :+: s) else larger' ys s

infixr 5 +++
(+++) :: Ket a -> Ket a -> Ket a
(+++) a b = connect a b
    where 
      connect KetZero b = b
      connect (x :+: xs) b = x :+: connect xs b 

reverse1 :: Ket a -> Ket a
reverse1 x = reverse' x KetZero
    where
      reverse' KetZero s = s
      reverse' (y :+: ys) s = reverse' ys (y :+: s)
