{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- This file is the source code for the tests. You're not supposed to be here!

module Main where

import Data.Foldable
  (
  toList,
  )
import Data.List
  (
  intersperse,
  )

import Test.Hspec
  (
  Spec,
  describe,
  hspec,
  it,
  shouldSatisfy,
  )
import Test.QuickCheck
  (
  Arbitrary,
  Property,
  (==>),
  arbitrary,
  arbitrarySizedNatural,
  choose,
  listOf,
  property,
  )

import qualified A1Nat         as N
import qualified A2List        as L
import qualified A3BreadButter as B

isInverseOf :: (Arbitrary a, Eq a, Show a) => (b -> a) -> (a -> b) -> Property
isInverseOf f g = property $ \a -> f (g a) == a

itIsAssociative :: (Arbitrary a, Eq a, Show a) => (a -> a -> a) -> Spec
itIsAssociative f = it "is associative" $ property $ \a b c -> f (f a b) c == f a (f b c)

itIsCommutative :: (Arbitrary a, Show a, Eq b) => (a -> a -> b) -> Spec
itIsCommutative f = it "is commutative" $ property $ \a b -> f a b == f b a

itIsInvolutory :: (Arbitrary a, Show a, Eq a) => (a -> a) -> Spec
itIsInvolutory f = it "is involutory" $ f `isInverseOf` f

itIsIrreflective :: (Arbitrary a, Show a) => (a -> a -> Bool) -> Spec
itIsIrreflective f = it "is irreflective" $ property $ \a -> not $ f a a

itIsReflective :: (Arbitrary a, Show a) => (a -> a -> Bool) -> Spec
itIsReflective f = it "is reflective" $ property $ \a -> f a a

itIsTransitive :: (Arbitrary a, Show a) => (a -> a -> Bool) -> Spec
itIsTransitive f = it "is transitive" $ property $ \a b c -> f a b && f b c ==> f a c

main :: IO ()
main = hspec $ do
  describe "Nat.==" $ do
    it "tests *arbitrary* Nats for equality" $
      property $ \x y -> (N.S x == N.S y) == (x == y)
    itIsCommutative ((==) :: N.Nat -> N.Nat -> Bool)
    itIsReflective ((==) :: N.Nat -> N.Nat -> Bool)
  describe "Nat.isZ" $
    it "checks if *arbitrary* Nats are zero" $
      property $ \n -> N.isZ n == (n == N.Z)
  describe "Nat.toNat" $ do
    it "turns *arbitrary* Ints into Nats" $
      property $ \n -> N.toNat n == toNat n
    it "is the inverse of Nat.fromNat" $
      N.toNat `isInverseOf` N.fromNat
  describe "Nat.fromNat" $
    it "turns *arbitrary* Nats into Ints" $
      property $ \n -> N.fromNat (N.S n) == 1 + N.fromNat n
  describe "Nat.predNat" $ do
    it "preceed *arbitrary* Nats" $
      property $ \n -> N.predNat (N.S n) == n
    it "is the inverse of Nat.succNat" $
      N.predNat `isInverseOf` N.succNat
  describe "Nat.succNat" $
    it "succeed *arbitrary* Nats" $
      property $ \n -> N.succNat n == N.S n
  describe "Nat.plus" $ do
    it "adds *arbitrary* Nats" $
      property $ \x y -> N.plus (N.S x) y == N.S (N.plus x y)
    itIsAssociative N.plus
    itIsCommutative N.plus
  describe "Nat.times" $ do
    it "multiplies *arbitrary* Nats" $
      property $ \x y -> N.times (N.S x) y == N.plus y (N.times x y)
    itIsAssociative (\(Small x) (Small y) -> Small (N.times x y))
    itIsCommutative N.times
  describe "Nat.powerOf" $
    it "raises *arbitrary* Nats to the power of *arbitrary* Nats" $
      property $ \(Small b) (Tiny e) -> N.powerOf b (N.S e) == N.times b (N.powerOf b e)
  describe "Nat.minus" $ do
    it "subtracts *arbitrary* Nats" $
      property $ \x y -> N.minus (N.S x) (N.S y) == N.minus x y
    it "is the inverse of Nat.plus" $
      property $ \a -> (`N.minus` a) `isInverseOf` (`N.plus` a)
  describe "Nat.lteNat" $ do
    it "finds out if an *arbitrary* Nat is smaller than or equal to an *arbitrary* Nat" $
      property $ \x y -> N.lteNat (N.S x) (N.S y) == N.lteNat x y
    itIsReflective N.lteNat
    itIsTransitive N.lteNat
  describe "Nat.ltNat" $ do
    it "finds out if an *arbitrary* Nat is smaller than an *arbitrary* Nat" $
      property $ \x y -> N.ltNat x y == N.lteNat (N.S x) y
    itIsIrreflective N.ltNat
    itIsTransitive N.ltNat
  describe "Nat.gteNat" $ do
    it "finds out if an *arbitrary* Nat is larger than or equal to an *arbitrary* Nat" $
      property $ \x y -> N.gteNat x y == N.lteNat y x
    itIsReflective N.gteNat
    itIsTransitive N.gteNat
  describe "Nat.gtNat" $ do
    it "finds out if an *arbitrary* Nat is greater than an *arbitrary* Nat" $
      property $ \x y -> N.gtNat x y == N.ltNat y x
    itIsIrreflective N.gtNat
    itIsTransitive N.gtNat
  describe "Nat.minNat" $ do
    it "finds out which out of two *arbitrary* Nats is the smaller" $
      property $ \x y -> N.minNat x (N.plus x y) == x
    itIsAssociative N.minNat
    itIsCommutative N.minNat
  describe "Nat.maxNat" $ do
    it "finds out which out of two *arbitrary* Nats is the bigger" $
      property $ \x y -> N.maxNat x (N.plus x y) == N.plus x y
    itIsAssociative N.maxNat
    itIsCommutative N.maxNat
  describe "Nat.compare" $
    it "compares *arbitrary* Nats" $
      property $ \x y -> compare (N.S x) (N.S y) == compare x y
  describe "Nat.fact" $
    it "calculates an *arbitrary* factorial" $
      property $ \(Tiny n) -> N.fact (N.S n) == N.times (N.S n) (N.fact n)
  describe "Nat.fib" $
    it "calculates an *arbitrary* fibonacci sequence number" $
      property $ \(Small n) -> N.fib (N.S (N.S n)) == N.plus (N.fib (N.S n)) (N.fib n)
  describe "List.append" $ do
    it "appends *arbitrary* Lists" $
      property $ \(IntList xs) (IntList ys) -> L.append xs ys ==
        case xs of
          L.Nil       -> ys
          L.Cons z zs -> L.append (L.Cons z zs) ys
    itIsAssociative (L.append :: L.List Int -> L.List Int -> L.List Int)
  describe "List.headList" $
    it "gets the head of an *arbitrary* List" $
      property $ \(IntList xs) -> L.headList xs ==
        case xs of
          L.Nil        -> L.Nope
          (L.Cons y _) -> L.Have y
  describe "List.tailList" $
    it "gets the tail of an *arbitrary* List" $
      property $ \(IntList xs) -> L.tailList xs ==
        case xs of
          L.Nil            -> L.Nope
          (L.Cons _ ys)    -> L.Have ys
  describe "List.initList" $
    it "gets the init of an *arbitrary* List" $
      property $ \(IntList xs) -> L.initList xs ==
        case xs of
          L.Nil          -> L.Nope
          L.Cons y L.Nil -> L.Nope
          L.Cons y ys    -> L.Have (go y ys)
            where go _ L.Nil          = L.Nil
                  go z (L.Cons zz zs) = L.Cons z (go zz zs)
  describe "List.unconsList" $ do
    it "unconses an *arbitrary* List" $
      property $ \(IntList xs) -> L.unconsList xs ==
        case xs of
          L.Nil          -> L.Nope
          L.Cons y ys    -> L.Have (y, ys)
    it "is the inverse of List.Cons" $
      ((\(L.Have x) -> x) . L.unconsList) `isInverseOf` (\(y, ys) -> L.Cons (y :: Int) ys)
  describe "List.nullList" $
    it "checks if an *arbitrary* List is null" $
      property $ \(IntList xs) -> L.nullList xs == (xs == L.Nil)
  describe "List.sumList" $
    it "sums an *arbitrary* List of Nats" $
      property $ \xs -> L.sumList xs ==
        case xs of
          L.Nil       -> N.Z
          L.Cons y ys -> N.plus y (L.sumList ys)
  describe "List.productList" $
    it "calculates the product of an *arbitrary* List of Nats" $
      property $ \(SmallList xs) -> L.productList xs ==
        case xs of
          L.Nil       -> N.S N.Z
          L.Cons y ys -> N.times y (L.productList ys)
  describe "List.maximumList" $
    it "gets the greatest element of an *arbitrary* List of Nats" $
      property $ \xs -> L.maximumNatList xs ==
        case xs of
          L.Nil -> L.Nope
          _     -> L.Have . maximum  . toList $ xs
  describe "List.minimumList" $
    it "gets the smallest element of an *arbitrary* List of Nats" $
      property $ \xs -> L.minimumNatList xs ==
        case xs of
          L.Nil -> L.Nope
          _     -> L.Have . minimum  . toList $ xs
  describe "List.takeList" $
    it "takes the given number of elements from the head of an *arbitrary* List" $
      property $ \n (IntList xs) -> toList (L.takeList n xs) == take n (toList xs)
  describe "List.dropList" $
    it "drops the given number of elements from the head of an *arbitrary* List" $
      property $ \n (IntList xs) -> toList (L.dropList n xs) == drop n (toList xs)
  describe "List.splitList" $
    it "splits an *arbitrary* List on an *arbitrary* index" $
      property $ \n (IntList xs) -> L.splitList n xs == (L.takeList n xs, L.dropList n xs)
  describe "List.reverseList" $ do
    it "reverses an *arbitrary* List" $
      property $ \(IntList xs) -> L.reverseList xs ==
        case xs of
          L.Nil       -> L.Nil
          L.Cons y ys -> L.append (L.reverseList ys) (L.Cons y L.Nil)
    itIsInvolutory (L.reverseList :: L.List Int -> L.List Int)
  describe "List.intersperseList" $
    it "intersperses an *arbitrary* element to an *arbitrary* List" $
      property $ \x (IntList xs) -> L.intersperseList x xs ==
        case xs of
          L.Nil       -> L.Nil
          L.Cons y ys -> L.Cons y (L.Cons x (L.intersperseList x ys))
  describe "List.concatList" $
    it "concatenates an *arbitrary* List" $
      property $ \(IntListList xs) -> L.concatList xs ==
        case xs of
          L.Nil           -> L.Nil
          L.Cons ys L.Nil -> ys
          L.Cons ys zs    -> L.append ys (L.concatList zs)
  describe "List.intercalateList" $
    it "intercalates an *arbitrary* List with an *arbitrary* List" $
      property $ \(IntList xs) (IntListList ys) -> L.intercalateList xs ys == L.concatList (L.intersperseList xs ys)
  describe "List.zipList" $
    it "zips two *arbitrary* Lists" $
      property $ \(IntList xs) (IntList ys) -> L.zipList xs ys ==
        case xs of
          L.Nil       -> L.Nil
          L.Cons z zs -> case ys of
                           L.Nil         -> L.Nil
                           L.Cons zz zzs -> L.Cons (z, zz) (L.zipList zs zzs)
  describe "List.elemList" $
    it "checks if an *arbitrary* element is in an *arbitrary* List" $
      property $ \x (IntListList xs) -> L.elemList x xs == elem x (toList xs)
  describe "BreadButter.mapList" $
    it "maps a function on an *arbitrary* list" $
      property $ \(xs :: [Int]) -> B.mapList (odd . (* 6)) xs == map (odd . (* 6)) xs
  describe "BreadButter.filterList" $
    it "filters an *arbitrary* list on a predicate" $
      property $ \(xs :: [Int]) -> B.filterList (> 5) xs == filter (> 5) xs
  describe "BreadButter.add42List" $
    it "maps (+ 42) to an *arbitrary* list of Ints" $
      property $ \xs -> B.add42List xs == map (+ 42) xs
  describe "BreadButter.invertBoolList" $ do
    it "inverts an arbitrary list of Booleans" $
      property $ \xs -> B.invertBoolList xs == map not xs
    itIsInvolutory B.invertBoolList
  describe "BreadButter.geq42List" $
    it "filters an *arbitrary* list of Ints for any Int >42" $
      property $ \xs -> B.geq42List xs == filter (> 42) xs
  describe "BreadButter.onlyTrueList" $
    it "filters out any False values from an *arbitrary* list of Bools" $
      property $ \xs -> B.onlyTrueList xs == filter (== True) xs
  describe "BreadButter.times2" $
    it "multiplies an *arbitrary* Int with 2" $
      property $ \n -> B.times2 n == n * 2
  describe "BreadButter.take4" $
    it "takes 4 elements from the head of an *arbitrary* list" $
      property $ \(xs :: [Int]) -> B.take4 xs == take 4 xs
  describe "BreadButter.intersperse0" $
    it "intersperses 0 to an *arbitrary* list of Ints" $
      property $ \xs -> B.intersperse0 xs == intersperse 0 xs
  describe "BreadButter.take4intersperse0" $
    it "takes 4 elements from the head of an *arbitrary* list of Ints, then intersperses 0 to the resulting list" $
      property $ \xs -> B.take4intersperse0 xs == (take 4 . intersperse 0 $ xs)
  describe "BreadButter.t4i0t2" $
    it "takes 4 elements from the head of an *arbitrary* list of Ints, then intersperses 0 to the resulting list, and finally multiplies every element with 2" $
      property $ \xs -> B.t4i0t2 xs == (take 4 . intersperse 0 . map (* 2) $ xs)
  describe "BreadButter.letterT" $
    it "filters out any value that is not 't' or 'T' from an *arbitrary* String" $
      property $ \xs -> B.letterT xs == (filter (== 't') . filter (== 'T') $ xs)
  describe "BreadButter.notElemList" $
    it "checks if an *arbitrary* element is not in an *arbitrary* list" $
      property $ \x (xs :: [Int]) -> B.notElemList x xs == notElem x xs
  describe "BreadButter.lengthFold" $
    it "calculates the length of an *arbitrary* list" $
      property $ \(xs :: [Int]) -> B.lengthFold xs == length xs
  describe "BreadButter.reverseFold" $
    it "reverses an *arbitrary* list" $
      property $ \(xs :: [Int]) -> B.reverseFold xs == reverse xs
  describe "BreadButter.appendFold" $
    it "appends *arbitrary* lists" $
      property $ \(xs :: [Int]) (ys :: [Int]) -> B.appendFold xs ys == xs ++ ys
  describe "BreadButter.mapFold" $
    it "maps a function on an *arbitrary* list" $
      property $ \(xs :: [Int]) -> B.mapFold (odd . (* 6)) xs == map (odd . (* 6)) xs
  describe "BreadButter.filterFold" $
    it "filters an *arbitrary* list on a predicate" $
      property $ \(xs :: [Int]) -> B.filterFold (> 5) xs == filter (> 5) xs

toNat :: Int -> N.Nat
toNat n | n > 0     = N.S (toNat (n - 1))
        | otherwise = N.Z

newtype TinyNats   = Tiny N.Nat                        deriving (Eq, Show)
newtype SmallNats  = Small N.Nat                       deriving (Eq, Show)
newtype IntList    = IntList     (L.List Int)          deriving (Eq, Show)
newtype IntIntList = IntListList (L.List (L.List Int)) deriving (Eq, Show)
newtype TinyList   = TinyList    (L.List N.Nat)        deriving (Eq, Show)
newtype SmallList  = SmallList   (L.List N.Nat)        deriving (Eq, Show)

instance Arbitrary N.Nat where
  arbitrary = toNat <$> arbitrarySizedNatural

instance Arbitrary (TinyNats) where
  arbitrary = Tiny . toNat <$> choose (0, 3)

instance Arbitrary (SmallNats) where
  arbitrary = Small . toNat <$> choose (0, 9)

instance (Arbitrary a) => Arbitrary (L.List a) where
  arbitrary = (foldr L.Cons L.Nil :: [a] -> L.List a) <$> arbitrary

instance Arbitrary IntList where
  arbitrary = IntList <$> arbitrary

instance Arbitrary IntIntList where
  arbitrary = IntListList <$> arbitrary

instance Arbitrary TinyList where
  arbitrary = TinyList . foldr L.Cons L.Nil <$> listOf (toNat <$> choose (0, 3))

instance Arbitrary SmallList where
  arbitrary = SmallList . foldr L.Cons L.Nil <$> listOf (toNat <$> choose (0, 3))
