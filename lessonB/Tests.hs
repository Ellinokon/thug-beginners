{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
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
import Data.Monoid
  (
  Sum (Sum),
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

import qualified B1Monoid         as M
import qualified B2Functor        as F
import qualified B3Monad          as N
import qualified B4FoldTraverse   as T

isInverseOf :: (Arbitrary a, Eq a, Show a) => (b -> a) -> (a -> b) -> Property
isInverseOf f g = property $ \a -> f (g a) == a

itIsAssociative :: (Arbitrary a, Eq a, Show a) => (a -> a -> a) -> Spec
itIsAssociative f = it "is associative" $ property $ \a b c -> f (f a b) c == f a (f b c)

itIsCommutative :: (Arbitrary a, Show a, Eq b) => (a -> a -> b) -> Spec
itIsCommutative f = it "is commutative" $ property $ \a b -> f a b == f b a

itIsInvolutory :: (Arbitrary a, Show a, Eq a) => (a -> a) -> Spec
itIsInvolutory f = it "is involutory" $ f `isInverseOf` f

itIsIrreflexive :: (Arbitrary a, Show a) => (a -> a -> Bool) -> Spec
itIsIrreflexive f = it "is irreflexive" $ property $ \a -> not $ f a a

itIsReflexive :: (Arbitrary a, Show a) => (a -> a -> Bool) -> Spec
itIsReflexive f = it "is reflexive" $ property $ \a -> f a a

itIsTransitive :: (Arbitrary a, Show a) => (a -> a -> Bool) -> Spec
itIsTransitive f = it "is transitive" $ property $ \a b c -> f a b && f b c ==> f a c

main :: IO ()
main = hspec $ do
  describe "Monoid.appendPerhapsList" $ do
    it "appends *arbitrary* Perhaps (List a)s" $
      property $ \(PerhapsIntList xs) (PerhapsIntList ys) -> M.appendPerhapsList xs ys ==
        case (xs, ys) of
          (a, M.Nope)          -> a
          (M.Nope, b)          -> b
          (M.Have a, M.Have b) -> M.Have (a `M.append` b)
    itIsAssociative (M.appendPerhapsList :: M.Perhaps (M.List Int) -> M.Perhaps (M.List Int) -> M.Perhaps (M.List Int))
  describe "Monoid.appendPerhaps" $ do
    it "appends *arbitrary* Perhaps [a]s" $
      property $ \(xs :: M.Perhaps [Int]) (ys :: M.Perhaps [Int]) -> M.appendPerhaps xs ys ==
        case (xs, ys) of
          (a, M.Nope)          -> a
          (M.Nope, b)          -> b
          (M.Have a, M.Have b) -> M.Have (a ++ b)
    itIsAssociative (M.appendPerhaps :: M.Perhaps [Int] -> M.Perhaps [Int] -> M.Perhaps [Int])
  describe "Monoid.appendMaybeList" $ do
    it "appends *arbitrary* Maybe (List a)s" $
      property $ \(MaybeIntList xs) (MaybeIntList ys) -> M.appendMaybeList xs ys ==
        case (xs, ys) of
          (a, Nothing)     -> a
          (Nothing, b)     -> b
          (Just a, Just b) -> Just (a `M.append` b)
    itIsAssociative (M.appendMaybeList :: Maybe (M.List Int) -> Maybe (M.List Int) -> Maybe (M.List Int))
  describe "Monoid.appendMaybe" $ do
    it "appends *arbitrary* Maybe [a]s" $
      property $ \(xs :: Maybe [Int]) (ys :: Maybe [Int]) -> M.appendMaybe xs ys ==
        case (xs, ys) of
          (a, Nothing)     -> a
          (Nothing, b)     -> b
          (Just a, Just b) -> Just (a ++ b)
    itIsAssociative (M.appendMaybe :: Maybe [Int] -> Maybe [Int] -> Maybe [Int])
  describe "Monoid.(<>) (Perhaps)" $ do
    it "combines *arbitrary* Perhaps as" $
      property $ \(p :: M.Perhaps [Int]) (q :: M.Perhaps [Int]) -> (M.<>) p q ==
        case (p, q) of
          (a, M.Nope)          -> a
          (M.Nope, b)          -> b
          (M.Have a, M.Have b) -> M.Have ((M.<>) a b)
    itIsAssociative ((M.<>) :: M.Perhaps [Int] -> M.Perhaps [Int] -> M.Perhaps [Int])
  describe "Monoid.(<>) (Maybe)" $ do
    it "combines *arbitrary* Maybe as" $
      property $ \(m :: Maybe [Int]) (n :: Maybe [Int]) -> (M.<>) m n == mappend m n
    itIsAssociative ((M.<>) :: Maybe [Int] -> Maybe [Int] -> Maybe [Int])
  describe "Functor.mapTree" $
    it "maps a function on an *arbitrary* Tree" $
      property $ \(IntTree a) -> correctTree (F.mapTree mapF a) == (mapF <$> correctTree a)
  describe "Functor.fmap (Tree)" $
    it "fmaps a function on an *arbitrary* Tree" $
      property $ \(IntTree a) -> correctTree (mapF <$> a) == (mapF <$> correctTree a)
  describe "Functor.fmap (Pair)" $
    it "fmaps a function on an *arbitrary* Pair" $
      property $ \(IntPair a) -> correctPair (mapF <$> a) == (mapF <$> correctPair a)
  describe "Functor.fmap (Perhaps)" $
    it "fmaps a function on an *arbitrary* Perhaps" $
      property $ \(PerhapsInt a) -> correctPerhaps (mapF <$> a) == (mapF <$> correctPerhaps a)
  describe "Functor.fmap (Composition)" $
    it "fmaps a function on an *arbitrary* Composition" $
      property $ \(ComposeMaybeIntList a) -> correctComposition (mapF <$> a) == (mapF <$> correctComposition a)
  describe "Functor.fmap (ZipList)" $
    it "fmaps a function on an *arbitrary* ZipList" $
      property $ \(IntZipList a) -> correctZipList (mapF <$> a) == (mapF <$> correctZipList a)
  describe "Functor.fmap (NonDetermTree)" $
    it "fmaps a function on an *arbitrary* NonDetermTree" $
      property $ \(IntNonDetermTree a) -> correctNonDetermTree (mapF <$> a) == (mapF <$> correctNonDetermTree a)
  describe "Functor.pure & Functor.(<*>) (ZipList)" $
    it "aps a function on an *arbitrary* pure ZipList" $
      property $ \(IntZipList a) -> correctZipList (pure mapF <*> a) == (pure mapF <*> correctZipList a)
  describe "Functor.pure & Functor.(<*>) (NonDetermTree)" $
    it "aps a function on an *arbitrary* pure NonDetermTree" $
      property $ \(IntNonDetermTree a) -> correctNonDetermTree (pure mapF <*> a) == (pure mapF <*> correctNonDetermTree a)
  describe "Monad.fmap (Ctx)" $
    it "fmaps a function on an *arbitrary* Ctx" $
      property $ \(IntCtx a) -> correctCtx (mapF <$> a) == (mapF <$> correctCtx a)
  describe "Monad.pure & Monad.(<*>) (Ctx)" $
    it "aps a function on an *arbitrary* pure Ctx" $
      property $ \(IntCtx a) -> correctCtx (pure mapF <*> a) == (pure mapF <*> correctCtx a)
  describe "Monad.incC" $
    it "increments an *arbitrary* Int and puts it in a Ctx" $
      property $ \a -> N.incC a == N.C (a + 1)
  describe "Monad.(>>=) (Ctx)" $
    it "binds an arbitrary Ctx a to a function" $
      property $ \(IntCtx a) -> (a >>= N.incC) == (a >>= N.incC)
  describe "Monad.(>>=) (Perhaps)" $
    it "binds an arbitrary Perhaps a to a function" $
      property $ \(PerhapsInt a) -> correctPerhaps (a >>= F.Have . (1 +)) == (correctPerhaps a >>= Have . (1 +))
  describe "Monad.(>>=) (NonDetermTree)" $
    it "binds an arbitrary NonDetermTree a to a function" $
      property $ \(IntNonDetermTree a) -> correctNonDetermTree (a >>= F.Leaf . (1 +)) == (correctNonDetermTree a >>= Leaf . (1 +))
  describe "FoldTraverse.foldMap" $
    it "foldMaps an *arbitrary* Tree" $
      property $ \(IntTree a) -> foldMap Sum a == foldMap Sum (correctTree a)
  describe "FoldTraverse.sumFold" $
    it "sums an *arbitrary* Foldable" $
      property $ \(IntTree a) -> T.sumFold a == sum (correctTree a)
  describe "FoldTraverse.lengthFold" $
    it "calculates the length of an *arbitrary* Foldable" $
      property $ \(IntTree a) -> T.lengthFold a == length (correctTree a)
  describe "FoldTraverse.treeToList" $
    it "sums an *arbitrary* Foldable" $
      property $ \(IntTree a) -> T.treeToList a == toList (correctTree a)
  describe "FoldTraverse.foldableToList" $
    it "sums an *arbitrary* Foldable" $
      property $ \(IntTree a) -> T.foldableToList a == toList (correctTree a)
  describe "FoldTraverse.traverse" $
    it "traverses an *arbitrary* Tree" $
      property $ \(IntTree a) -> (correctTree <$> traverse travF a) == traverse travF (correctTree a)

data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Eq, Foldable, Functor, Show, Traversable)

data Pair a = P a a deriving (Eq, Foldable, Functor, Show, Traversable)

data Perhaps a = Nope
               | Have a
               deriving (Eq, Foldable, Functor, Show, Traversable)
instance Applicative Perhaps where
  pure            = Have
  Nope   <*> _    = Nope
  _      <*> Nope = Nope
  Have f <*> x    = f <$> x
instance Monad Perhaps where
  Nope     >>= _ = Nope
  (Have p) >>= f = f p

data Composition f g x = Compose (f (g x))
  deriving (Eq, Foldable, Functor, Show)

data ZipList a = Z [a] deriving (Eq, Foldable, Functor, Show, Traversable)
instance Applicative ZipList where
  pure x            = Z (repeat x)
  (Z fs) <*> (Z xs) = Z (zipWith id fs xs)

data NonDetermTree a = EmptyND
                     | Leaf a
                     | Branch (NonDetermTree a) (NonDetermTree a)
               deriving (Eq, Foldable, Functor, Show, Traversable)
instance Applicative NonDetermTree where
  pure                = Leaf
  EmptyND <*> _       = EmptyND
  _       <*> EmptyND = EmptyND
  Leaf f <*> t        = fmap f t
  (Branch l r) <*> t  = Branch (l <*> t) (r <*> t)
instance Monad NonDetermTree where
  EmptyND      >>= _ = EmptyND
  Leaf x       >>= f = f x
  (Branch l r) >>= f = Branch (l >>= f) (r >>= f)

data Ctx a = C a deriving (Eq, Foldable, Functor, Show)

instance Applicative Ctx where
   pure        = C
   (C f) <*> c = fmap f c

instance Monad Ctx where
  (C c) >>= f = f c

correctTree :: F.Tree a -> Tree a
correctTree F.Empty        = Empty
correctTree (F.Node l x r) = Node (correctTree l) x (correctTree r)

correctPair :: F.Pair a -> Pair a
correctPair (F.P a b) = P a b

correctPerhaps :: F.Perhaps a -> Perhaps a
correctPerhaps F.Nope     = Nope
correctPerhaps (F.Have x) = Have x

correctComposition :: F.Composition f g x -> Composition f g x
correctComposition (F.Compose c) = Compose c

correctZipList :: F.ZipList a -> ZipList a
correctZipList (F.Z z) = Z z

correctNonDetermTree :: F.NonDetermTree a -> NonDetermTree a
correctNonDetermTree F.EmptyND      = EmptyND
correctNonDetermTree (F.Leaf a)     = Leaf a
correctNonDetermTree (F.Branch l r) = Branch (correctNonDetermTree l)
                                             (correctNonDetermTree r)

correctCtx :: N.Ctx a -> Ctx a
correctCtx (N.C c) = C c

newtype IntPair             = IntPair             (F.Pair Int)
  deriving (Eq, Show)
newtype IntList             = IntList             (M.List Int)
  deriving (Eq, Show)
newtype IntTree             = IntTree             (F.Tree Int)
  deriving (Eq, Show)
newtype PerhapsInt          = PerhapsInt          (F.Perhaps Int)
  deriving (Eq, Show)
newtype PerhapsIntList      = PerhapsIntList      (M.Perhaps (M.List Int))
  deriving (Eq, Show)
newtype MaybeIntList        = MaybeIntList        (Maybe (M.List Int))
  deriving (Eq, Show)
newtype ComposeMaybeIntList = ComposeMaybeIntList (F.Composition Maybe [] Int)
  deriving (Eq, Show)
newtype IntZipList          = IntZipList          (F.ZipList Int)
  deriving (Eq, Show)
newtype IntNonDetermTree    = IntNonDetermTree    (F.NonDetermTree Int)
  deriving (Eq, Show)
newtype IntCtx              = IntCtx              (N.Ctx Int)
  deriving (Eq, Show)

instance Arbitrary IntPair where
  arbitrary = IntPair <$> arbitrary

instance Arbitrary IntList where
  arbitrary = IntList <$> arbitrary

instance Arbitrary IntTree where
  arbitrary = IntTree <$> arbitrary

instance Arbitrary PerhapsInt where
  arbitrary = PerhapsInt <$> arbitrary

instance Arbitrary PerhapsIntList where
  arbitrary = PerhapsIntList <$> arbitrary

instance Arbitrary MaybeIntList where
  arbitrary = MaybeIntList <$> arbitrary

instance Arbitrary ComposeMaybeIntList where
  arbitrary = ComposeMaybeIntList <$> arbitrary

instance Arbitrary IntZipList where
  arbitrary = IntZipList <$> arbitrary

instance Arbitrary IntNonDetermTree where
  arbitrary = IntNonDetermTree <$> arbitrary

instance Arbitrary IntCtx where
  arbitrary = IntCtx <$> arbitrary

instance Arbitrary a => Arbitrary (M.Perhaps a) where
  arbitrary = mayHaps <$> arbitrary
    where mayHaps Nothing   = M.Nope
          mayHaps (Just x)  = M.Have x

instance Arbitrary a => Arbitrary (F.Pair a) where
  arbitrary = (<$> arbitrary) . F.P =<< arbitrary

instance Arbitrary a => Arbitrary (M.List a) where
  arbitrary = (foldr M.Cons M.Nil :: [a] -> M.List a) <$> arbitrary

instance Arbitrary a => Arbitrary (F.Tree a) where
  arbitrary = (foldr insertTree F.Empty :: [a] -> F.Tree a) <$> arbitrary
    where insertTree y F.Empty        = F.Node F.Empty y F.Empty
          insertTree x (F.Node l n r)
            | length (correctTree l) <=
              length (correctTree r)  = F.Node (insertTree x l) n r
            | otherwise               = F.Node l n (insertTree x r)

instance Arbitrary a => Arbitrary (F.Perhaps a) where
  arbitrary = mayHaps <$> arbitrary
    where mayHaps Nothing  = F.Nope
          mayHaps (Just x) = F.Have x

instance Arbitrary (F.Composition Maybe [] Int) where
  arbitrary = (F.Compose . Just . pure) <$> arbitrary

instance Arbitrary (F.ZipList Int) where
  arbitrary = F.Z <$> arbitrary

instance Arbitrary (F.NonDetermTree Int) where
  arbitrary = (foldr insertTree F.EmptyND :: [a] -> F.NonDetermTree a) <$> arbitrary
    where insertTree x F.EmptyND  = F.Leaf x
          insertTree x (F.Leaf a) = F.Branch (F.Leaf a) (F.Leaf x)
          insertTree x (F.Branch l r)
            | length (correctNonDetermTree l) <=
              length (correctNonDetermTree r)  = F.Branch (insertTree x l) r
            | otherwise                        = F.Branch l (insertTree x r)

instance Arbitrary (N.Ctx Int) where
  arbitrary = N.C <$> arbitrary

mapF :: Int -> Bool
mapF = odd . (* 6)

travF :: Integral a => a -> Maybe a
travF x | even x    = Just (x * 2)
        | otherwise = Nothing
