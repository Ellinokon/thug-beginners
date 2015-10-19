module A1Nat where

data Nat = Z
         | S Nat
         deriving Show

instance Eq Nat where
  Z     == Z     = True
  (S x) == (S y) = x == y
  _     == _     = False

isZ :: Nat -> Bool
isZ Z = True
isZ _ = False

toNat :: Int -> Nat
toNat n | n > 0     = S (toNat (n - 1))
        | otherwise = Z

fromNat :: Nat -> Int
fromNat Z     = 0
fromNat (S n) = 1 + fromNat n

predNat :: Nat -> Nat
predNat Z     = Z
predNat (S n) = n

succNat :: Nat -> Nat
succNat = S

plus :: Nat -> Nat -> Nat
Z     `plus` y = y
(S x) `plus` y = S (x `plus` y)

times :: Nat -> Nat -> Nat
Z     `times` y  = Z
(S x) `times` y = y `plus` (x `times` y)

powerOf :: Nat -> Nat -> Nat
_ `powerOf` Z     = S Z
b `powerOf` (S e) = b `times` (b `powerOf` e)

minus :: Nat -> Nat -> Nat
Z     `minus` _     = Z
x     `minus` Z     = x
(S x) `minus` (S y) = x `minus` y

lteNat :: Nat -> Nat -> Bool
Z     `lteNat` y     = True
x     `lteNat` Z     = False
(S x) `lteNat` (S y) = lteNat x y

ltNat :: Nat -> Nat -> Bool
ltNat = lteNat . S

gteNat :: Nat -> Nat -> Bool
gteNat = flip lteNat

gtNat :: Nat -> Nat -> Bool
gtNat = flip ltNat

minNat :: Nat -> Nat -> Nat
Z     `minNat` _     = Z
(S x) `minNat` Z     = Z
(S x) `minNat` (S y) = S (minNat x y)

maxNat :: Nat -> Nat -> Nat
Z     `maxNat` y     = y
(S x) `maxNat` Z     = S x
(S x) `maxNat` (S y) = S (maxNat x y)

instance Ord Nat where
  compare Z Z         = EQ
  compare Z (S y)     = LT
  compare (S x) Z     = GT
  compare (S x) (S y) = compare x y

fact :: Nat -> Nat
fact Z     = S Z
fact (S n) = S n `times` fact n

fib :: Nat -> Nat
fib Z         = Z
fib (S Z)     = S Z
fib (S (S n)) = fib (S n) `plus` fib n
