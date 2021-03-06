module B4FoldTraverse where

import Data.Monoid
  (
  (<>),
  )

import B2Functor

treeToList :: Tree a -> [a]
-- Using recursion, build a [] from a Tree. It should work like this:
--   treeToList $ Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty)
--   == [1, 2, 3]
treeToList = undefined

-- Simple enough. I could throw more boring exercises at you now, like writing
-- the same thing for Maybe or NonDetermTree or whatever. Instead let's not.
-- Let's instead solve it once and for all.
--
-- In LessonA we wrote fold, which reduced a list while applying a binary
-- function to every member. That was useful, wasn't it? And, naturally, we
-- may abstract it from working on lists to working on anything... as long as
-- the stuff inside the anything is a Monoid. Why a Monoid? Because we need
-- the empty basecase (mempty).
--
-- Enter Foldable.
--
--   class Foldable t where
--     fold    :: Monoid m => t m -> m
--     foldMap :: Monoid m => (a -> m) -> t a -> m
--
--     foldr   :: (a -> b -> b) -> b -> t a -> b
--     foldl   :: (a -> b -> a) -> a -> t b -> a
--     foldr1  :: (a -> a -> a) -> t a -> a
--     foldl1  :: (a -> a -> a) -> t a -> a
--
-- Don't worry; it looks more complicated than it is. You only need to
-- implement foldr or foldMap. Then you'll get the rest for free.

instance Foldable Tree where
  -- Using the Tree we defined in B2, which is already a Functor, implement
  -- Foldable, assuming we're working with Monoids. The type of foldMap should
  -- make it obvious what this function is going to do. It takes a function
  -- that converts data to a Monoid, as well as the data (in some
  -- computational context) that it is to convert. So the empty value becomes
  -- mempty, and if there are multiple values they are combined with (<>).
  --
  -- An example function would be the Sum data constructor, which is a -> Sum
  -- a. Sum is a Monoid. You can import it in GHCi with Data.Monoid and play
  -- with it.
  --
  -- Exercise: What does foldMap Sum do? What about
  -- foldMap (\a -> (Sum 1, Sum a, Sum  -- (a*a)))?
  foldMap f Empty        = undefined
  foldMap f (Node l x r) = undefined

-- And you're done. You now got the other functions for free. Exercise: What
-- do the other functions do, do you think? Follow the types!
--
-- Have you spotted the relationship between foldMap and fold? Exercise:
-- express fold using id.
--
-- We could also have made Tree a Foldable via foldr. It's a bit more
-- difficult. Exercise: Write foldr for Foldable Tree.
--
-- It is worth mentioning that you actually get a bunch of functions for free
-- with your Foldable instance. See:
--   https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Foldable.html

-- Now let's do some exercises using Foldable. Verify them in GHCi using your
-- Foldable Tree.

sumFold :: Foldable f => f Int -> Int
sumFold = undefined

lengthFold :: Foldable f => f a -> Int
lengthFold = undefined

foldableToList :: Foldable f => f a -> [a]
-- And here we get a toList for all Foldables!
foldableToList = undefined

-- Finally, we want a way to commute Functors. I.e. we want to go from
-- Maybe (Tree a) to Tree (Maybe a), or from [Maybe a] to Maybe [a].
--
--   class (Functor t, Foldable t) => Traversable t where
--     traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
--     sequenceA :: Applicative f => t (f a) -> f (t a)
--     mapM      ::       Monad m => (a -> m b) -> t a -> m (t b)
--     sequence  ::       Monad m => t (m a) -> m (t a)
--
-- Like with Foldable we can get away with only implementing one of two
-- functions: traverse or sequenceA. The key insight offered by Traversable is
-- being able to go t (f a) -> f (t a). Very useful. The other insight is the
-- appearance of Applicative and Monad. They mean that we get e.g. a map that
-- may be effectful. Unlike plain old (f)map, mapM will let us apply
-- a function to a computational context, and for every application we get an
-- effect in that context. We'll look more closely at what this means in
-- LessonC, where we will be doing IO.

instance Traversable Tree where
  -- Let's turn Tree into a Traversable using traverse. From the type of
  -- traverse we know it is basically fmap. In fact, the only difference from
  -- the Functor implementation we did in B2 will be that you wrap everything
  -- in an Applicative to give us f (t a) rather than f a.
  traverse g Empty        = undefined
  traverse g (Node l x r) = undefined

-- That's it. You can now play with this in GHCi. Try turning a Maybe (Tree a)
-- into a Tree (Maybe a) and vice versa. Play around with similar context
-- commuting.

-- What is the significance of the Applicative nature of traverse, when
-- compared to fmap? Exercise: Given the following function:
--   f x | even x    = Just (x * 2)
--       | otherwise = Nothing
-- What happens if you fmap it to [1, 2, 3, 4]? What about when you traverse
-- the same list with it? What about the list [2, 4, 6, 8]?

-- Do you see what sequenceA is to traverse? It is similar to the relationship
-- between fold to foldMap. Exercise: What is sequenceA in terms of id?
