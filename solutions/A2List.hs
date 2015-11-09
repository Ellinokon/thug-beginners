{-# LANGUAGE DeriveFoldable #-}
module A2List where

import A1Nat

data List a = Nil
            | a `Cons` List a
            deriving (Show, Eq, Ord, Foldable)

infixr 5 `Cons`

append :: List a -> List a -> List a
Nil           `append` ys = ys
(x `Cons` xs) `append` ys = x `Cons` (xs `append` ys)

data Perhaps a = Nope
               | Have a
               deriving (Show, Eq, Ord, Foldable)

headList :: List a -> Perhaps a
headList Nil           = Nope
headList (x `Cons` xs) = Have x

tailList :: List a -> Perhaps (List a)
tailList Nil           = Nope
tailList (x `Cons` xs) = Have xs

lastList :: List a -> Perhaps a
lastList Nil            = Nope
lastList (x `Cons` Nil) = Have x
lastList (x `Cons` xs)  = lastList xs

initList :: List a -> Perhaps (List a)
initList Nil            = Nope
initList (x `Cons` Nil) = Nope
initList (x `Cons` xs)  = Have (go x xs)
  where go _ Nil           = Nil
        go y (z `Cons` zs) = y `Cons` go z zs

unconsList :: List a -> Perhaps (a, List a)
unconsList Nil           = Nope
unconsList (x `Cons` xs) = Have (x, xs)

nullList :: List a -> Bool
nullList Nil = True
nullList _   = False

lengthList :: List a -> Nat
lengthList Nil           = Z
lengthList (x `Cons` xs) = S Z `plus` lengthList xs

sumList :: List Nat -> Nat
sumList Nil           = Z
sumList (x `Cons` xs) = x `plus` sumList xs

productList :: List Nat -> Nat
productList Nil = S Z
productList (x `Cons` xs) = x `times` productList xs

maximumNatList :: List Nat -> Perhaps Nat
maximumNatList Nil = Nope
maximumNatList (x `Cons` xs) = Have (go x xs)
  where go y Nil             = y
        go y (z `Cons` zs)   = go (maxNat y z) zs

minimumNatList :: List Nat -> Perhaps Nat
minimumNatList Nil = Nope
minimumNatList (x `Cons` xs) = Have (go x xs)
  where go y Nil             = y
        go y (z `Cons` zs)   = go (minNat y z) zs


takeList :: Int -> List a -> List a
takeList _ Nil          = Nil
takeList n (x `Cons` xs)| n <= 0    = Nil
                        | otherwise = x `Cons` takeList (n - 1) xs

dropList :: Int -> List a -> List a
dropList _ Nil               = Nil
dropList n xss@(_ `Cons` xs) | n <= 0    = xss
                             | otherwise = dropList (n - 1) xs

splitList :: Int -> List a -> (List a, List a)
splitList n xs = (takeList n xs, dropList n xs)

reverseList :: List a -> List a
reverseList Nil           = Nil
reverseList (x `Cons` xs) = reverseList xs `append` (x `Cons` Nil)

intersperseList :: a -> List a -> List a
intersperseList _ Nil           = Nil
intersperseList x (y `Cons` ys) = y `Cons` x `Cons` intersperseList x ys

concatList :: List (List a) -> List a
concatList Nil             = Nil
concatList (xs `Cons` Nil) = xs
concatList (xs `Cons` ys)  = xs `append` concatList ys

intercalateList :: List a -> List (List a) -> List a
intercalateList xs ys = concatList (intersperseList xs ys)

zipList :: List a -> List b -> List (a, b)
zipList Nil _                       = Nil
zipList _ Nil                       = Nil
zipList (x `Cons` xs) (y `Cons` ys) = (x, y) `Cons` zipList xs ys

elemList :: (Eq a) => a -> List a -> Bool
elemList _ Nil           = False
elemList x (y `Cons` ys) | x == y    = True
                         | otherwise = elemList x ys
