module B2Functor where

import Data.Monoid
  (
  (<>),
  )

-- Recall that the code we wrote in LessonA ended up being polymorphic for
-- kind *. I.e. we wrote code that worked regardless of what type you had at
-- the outmost concrete level. It could be [a], List a, or Perhaps a, and it
-- did not really matter.
--
-- Now we will learn how to be polymorphic in kind * -> * as well. I.e. we
-- will write code that is f a -> f a. We often use f to mean things that need
-- to implement Functor.
--
-- Just in case you are not motivated yet, let's see why Functor (and
-- friends) are necessary in the first place.

-- Remember mapList from A3? In Haskell it's usually just called map. It lets
-- you apply a function (a -> b) to a [a], giving you a [b]. Simple. But now
-- consider this data type:
data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

mapTree :: (a -> b) -> Tree a -> Tree b
-- How do we map stuff here?
mapTree f Empty        = undefined
mapTree f (Node l x r) = undefined

-- OK, so that wasn't so difficult. But once again we have two functions where
-- we would have liked to have a polymorphic one. The same issues we talked
-- about in B1 exist here. And once again we have a solution. Welcome to
-- functors.
--
-- Functors have a function fmap (named so because "map" was introduced first
-- -- arguably it should just be called map), and must fulfil two laws:
--   fmap id      = id
--   fmap (f . h) = fmap f . fmap h
--
-- The implementation is trivial.
--
--   class functor f where
--     fmap :: (a -> b) -> f a -> f b
--
-- Then we instantiate it as you'd expect.
--   instance Functor [] where
--     fmap _ []     = []
--     fmap f (x:xs) = f x : fmap f xs
--
-- For [], you could just say fmap = map.

-- Make Tree a Functor now.
instance Functor Tree where
  fmap f Empty        = undefined
  fmap f (Node l x r) = undefined

-- Note that we instantiate things that are * -> * as Functors, not *s. So
-- fmap now works on a Tree containing elements of any type.
--
-- And vòila, we now have polymorphism one level up. That wasn't so hard, now
-- was it.
--
-- Now let's have some exercises. As a side-note, if you like infix style,
-- (<$>) is infix fmap.

-- Just a pair of *something*.
data Pair a = P a a deriving (Show, Eq)

instance Functor Pair where
  -- Implement an fmap that applies f to both members of the Pair.
  fmap = undefined

-- Good old Perhaps.
data Perhaps a = Nope
               | Have a
               deriving (Show, Eq, Ord)

instance Functor Perhaps where
-- Maybe is a Functor in Haskell proper. So let's make Perhaps a Functor too.
  fmap = undefined

-- The composition of two higher kinded types on a type.
data Composition f g x = Compose (f (g x)) deriving (Show, Eq)

instance (Functor f, Functor g) => Functor (Composition f g) where
  -- The composition of two Functors. E.g. Maybe [a], or [Maybe a]. HINT: Read
  -- the types! Remember that:
  --   fmap :: (a -> b) -> f a -> f b
  -- What is f in this case? The answer lies in the types, and the Functor
  -- laws.
  fmap = undefined

-- Speaking of the laws... they are very important. Exercise: can you imagine
-- a Functor [] implementation that typechecks but does not satisfy the laws?
-- Can you figure out why this is horribly bad?

-- As a result of the Functor laws we can actually automatically derive
-- Functors with the usual deriving syntax. Because there can only be one
-- lawful instance of a Functor. Ever.
--
-- This is because something called the *free theorem*. For the function
-- f :: [a] -> [a] we get the free theorem:
--   map g . f  = f . map g
-- The free theorem for fmap says that given:
--   f . g = p . q
-- We can be sure of:
--   fmap f . fmap g = fmap p . fmap q
--
-- Exercise: Let p = id, and q = f . g. Can you see why the second Functor law
-- is actually redundant?
--
-- You can read more about free theorems in Philip Wadler's excellent paper
-- linked in the README (link 3 under LessonB -- the "wadler.pdf" link).

-- Functors let you apply functions on things that are in a context, returning
-- the result inside the same context. We can thus say that functors only
-- cares what's inside the context, not the context itself.

-- But what if you have a function that's in a context itself? Then you need
-- what's called an applicative functor.
--
-- class Functor f => Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
--
-- Hopefully the types are clear to you. pure simply injects something into an
-- Applicative context, and (<*>) (which we pronounce "apply") is like fmap,
-- but with f (a -> b).
--
-- Have you learned about ($) yet? Maybe it's time. ($) is just function
-- application as an operator (rather than using whitespace).
--   f :: (a -> b) -> a -> b
--   f $ x = f x
--
-- What is it used for? Well, its low precedence means we can use it to
-- eliminate parentheses.
--   f x (g y) == f x $ g y
--
-- Now, do you see that (<$>) is just function application? (<$>) is exactly
-- like ($), but everything is in a computational context.

-- There are some laws for applicative functors.
--
-- The identity law:
--   pure id <*> v = v
-- The homomorphism law:
--   pure f <*> pure x = pure (f x)
-- The interchange law:
--   u <*> pure y = pure ($ y) <*> u
-- The composition law:
--   u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
--
-- We won't dwell on them much now. But they do afford one immediately
-- interesting observation. Namely that fmap g x = pure g <*> x. Consider this
-- then:
--   pure (+) <*> [1, 2, 3] <*> pure 10 == [11, 12, 13]
-- We put (+) in some pure context, apply it to the list [1, 2, 3] in order to
-- get the list of functions [(1+), (2+), (3+)], which we then apply to 10 in
-- some pure context. The result is then of course
-- [(1 + 10), (2 + 10), (3 + 10)], which we can reduce to [11, 12, 13].
--
-- But rather than using pure, we can via the insight afforded by the Functor
-- relation law use fmap!
--   fmap (+) [1, 2, 3] <*> pure 10
-- And with the (<$>) we get the more idiomatic "applicative style syntax":
--   (+) <$> [1, 2, 3] <*> pure 10

-- Now let's do some exercises using Applicative Functors. Note that all of
-- them can be resolved in nearly a plethora of ways with regards to syntax.
-- You should optimally do them in several ways, and try to understand why
-- some of them might be preferable to the others.

instance Applicative Perhaps where
  -- We'll start out with Perhaps. There's basically only one thing that makes
  -- sense here. If we Have something, then apply the function to it, If Nope
  -- then Nope.
  pure            = Have
  Nope   <*> _    = Nope
  _      <*> Nope = Nope
  Have f <*> x    = f <$> x

instance Applicative Tree where
  -- Now this is more interesting... For Trees there are likely several
  -- implementations that satisfy the laws. There sure are for the built-in
  -- list. We'll get back to that. Anyway, here's a perfectly valid
  -- implementation:
  pure x                          = let t = Node t x t in t
  Empty        <*> _              = Empty
  _            <*> Empty          = Empty
  (Node l f r) <*> (Node l' x r') = Node (l <*> l') (f x) (r <*> r')
  -- So what we're doing here? We're just applying the functions of the lhs
  -- Tree to the values in the rhs Tree. An example would be:
  --   let t = Node Empty (*2) Empty
  --   let u = Node Empty 1234 Empty
  -- Then t1 <*> t2 would give us Node Empty 2468 Empty. Play around in GHCi
  -- to see how it works with bigger trees.

-- [] is an Applicative. It could be so in at least two obvious ways. Here's
-- how it is implemented:
--   instance Applicative [] where
--     pure x    = [x]
--     fs <*> xs = [ f x | f <- fs, x <- xs ]
--
-- pure puts x in a List. So pure 42 == [42]. In (<*>) we use the special
-- built-in syntax list comprehensions. We'll explain this in detail in B3,
-- but for now let's focus on what it actually does. It is analogous to set
-- comprehensions from maths. You can read it as "for every fs as f, and every
-- xs as x, do f x". So given a list of functions and a list of values, apply
-- every function to every value, and collect the results in a list.
--   [(+ 2), (* 2)] <*> [1, 2, 3, 4] == [1 + 2, 2 + 2, 3 + 2, 4 + 2
--                                      ,1 * 2, 2 * 2, 3 * 2, 4 * 2]
--                                   == [3, 4, 5, 6, 2, 4, 6, 8]
--
-- It's pretty simple, and conceivably useful. But what's the intuition? Well,
-- lists model non-determinism. So the list [1, 2, 3, 4] models
-- a computational context whereof the result may be 1, 2, 3, or 4.

-- Another way we could make [] an Applicative would be by considering []
-- a collection of things.
--
-- In Haskell, we can't make a type implement a typeclass in more than one
-- way. Thankfully that problem is easily overcome by using a wrapper data
-- type.

data ZipList a = Z [a] deriving (Show, Eq)

instance Functor ZipList where
  -- First we need to make it a Functor. Go ahead and do that. It's just like
  -- the regular [] Functor, except wrapped in a ZipList.
  fmap = undefined

instance Applicative ZipList where
  -- Now the Applicative Functor for ZipList. The intuition we want, as given
  -- away by our type's name, is to zip a list together. An example:
  --   Z [(+ 2), (* 2), (/ 2)] <*> Z [1, 2, 3] == Z [1 + 2, 2 * 2, 3 / 2]
  --                                           == Z [3.0,4.0,1.5]
  -- HINT: Look at the type of the zipWith funtion (it's in Prelude), and play
  -- around with that for a bit.
  pure x            = undefined
  (Z fs) <*> (Z xs) = undefined

-- See how the ZipList behaves similarly to our Tree? That must mean our Tree
-- should be able to behave like []!
--
-- In order to make the tree a bit more useful for this style, we'll put the
-- data at the leaves instead.
data NonDetermTree a = EmptyND
                     | Leaf a
                     | Branch (NonDetermTree a) (NonDetermTree a)
                     deriving (Show, Eq)

instance Functor NonDetermTree where
  -- First, let's make it a Functor.
  fmap = undefined

instance Applicative NonDetermTree where
  -- And now an Applicative Functor that applies every function to every
  -- value, and collects them all in a nice NonDetermTree.
  pure                = undefined
  EmptyND <*> _       = undefined
  _       <*> EmptyND = undefined
  Leaf f <*> t        = undefined
  (Branch l r) <*> t  = undefined
