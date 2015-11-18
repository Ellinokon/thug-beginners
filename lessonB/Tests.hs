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
main = hspec $ do return ()
