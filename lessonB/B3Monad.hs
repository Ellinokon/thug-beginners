module B3Monad where

import B2Functor

-- Applicatives solved the problem of not being able to apply a function in
-- a context to values in a context. But the more imaginative of you might
-- have identified yet another problem.

-- Let's play with pretty much the simplest datatype imaginable.
data Ctx a = C a deriving (Show, Eq)

instance Functor Ctx where
  -- First, make it a Functor.
  fmap = undefined

instance Applicative Ctx where
   -- Then an Applicative.
   pure  = undefined
   (<*>) = undefined

-- This works and is nice. You can now in GHCi use fmap on it like you'd
-- expect. Try (+) <$> C 1 <*> C 2. In fact, let's write a function that
-- increments an Int, and then puts it in a Ctx.
incC :: Int -> Ctx Int
-- Take the given Int, add one to it, put it in a Ctx.
incC n = undefined

-- Now... how do increment it twice?
--
-- ...
--
-- Well, you could of course write inc2C and do that. But that's no good if
-- you want to incC once, and then another time later on. You could also just
-- unwrap it and do incC again on the unwrapped Int, but what if you don't
-- want to unwrap it? Maybe unwrapping it doesn't make sense for safety
-- purposes. Consider Ctx being a Maybe -- what do you do with Nothing? A lot
-- of problems arise with these proposed "solutions".
--
-- Monad to the rescue!
--
--   class Applicative m => Monad m where
--     (>>=) :: m a -> (a -> m b) -> m b
--
--     (>>)  :: m a -> m b -> m b
--     m >> k = m >>= \_ -> k
--
-- Monad lets us *compose computational contexts*. There are two functions in
-- there. The main function, the whole point of Monad, is (>>=) (pronounced
-- "bind"). (>>) is just a special case of (>>=) where we discard the value.
--
-- Monad also provides several utility functions, like (=<<), see:
--   https://hackage.haskell.org/package/base-4.8.1.0/docs/Control-Monad.html

instance Monad Ctx where
  -- Make C into an instance of Monad.
  (C c) >>= f = undefined

-- If you got that right, we can now compose incC!
--   pure 1 >>= incC            == C 2
--   incC 1 >>= incC            == C 2
--   (incC >=> incC) 1          == C 3
--   (incC >=> incC >=> incC) 1 == C 4

-- The (>=>) (often called the Kleisli arrow or Kleisli composition, see the
-- Why Do Monads Matter paper in the README) operator, is perhaps more
-- explicit, and might help you gain an intuition for what's going on.
-- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c).
--
-- Exercise: Observe how (>=>) relates to regular function composition, (.).

-- As with the other typeclasses, there are some simple rules to keep us sane:
--
--   return a >>= k                 = k a
--   m        >>= return            = m
--   m        >>= (\x -> k x >>= h) = (m >>= k) >>= h
--
-- Exercise: Reformulate the laws using (>=>).

instance Monad Perhaps where
  -- Let's turn Perhaps into a sensible Monad.
  Nope     >>= _ = undefined
  (Have p) >>= f = undefined

-- OK, that was simple enough. What about NonDetermTree from B2? First, let's
-- look at the [] Monad.
--   instance Monad []  where
--       xs >>= f  = [y | x <- xs, y <- f x]
--       xs >> ys  = [y | _ <- xs, y <- ys]
-- Remember to think about the non-determinism intuition. What's happening in
-- (>>=) is that for every xs as x, we do f x and denote that y. Then we
-- return every y.
--   [1, 2, 3] >>= \x -> [x * 2, x + 2] == [2, 3, 4, 4, 6, 5]
--   [3, 4, 5] >>= \x -> [x, -x]        == [3,-3, 4, -4, 5, -5]

-- Now try to make NonDetermTree a Monad. It is quite like Perhaps, only the
-- last case is a tiny bit more tricky. Follow the types!
-- HINT: If you are having a hard time with the third equation, try solving it
-- for 'Branch (Leaf l) (Leaf r) >>= f' first, and try to think about how you
-- can solve it recursively afterwards.
instance Monad NonDetermTree where
  EmptyND      >>= _ = undefined
  Leaf x       >>= f = undefined
  (Branch l r) >>= f = undefined

-- Now that you have thought long and hard about Monads, let me explain list
-- comprehensions, like I said I would. OK. So you have:
--   [x * 2 | x <- [1, 2, 3, 4]] == [2, 4, 6, 8]
-- Simple enough. For every member of [1, 2, 3, 4] as x, do x * 2.
--
-- What about this though:
--   [f x | x <- [1, 2, 3], f <- [(+ 1), (* 2)]] == [2, 2, 3, 4, 4, 6]
-- What's going on here? It's eerily reminiscent of the [] Applicative:
--    [(+1),(*2)] <*> [1..3] =                   == [2, 3, 4, 2, 4, 6]
-- Yet it's not entirely the same.
--
-- That's because this is really a *monad comprehension*. The reason they work
-- for lists and not other monads, is simply because it's more used for lists,
-- and nobody really whined about it not being enabled for other monads. Yes.
-- Really. That is the reason. Personally, I think it would make sense to have
-- them on by default, but whatever. You can enable them with the
-- MonadComprehensions pragma. To enable pragmas in GHCi, use :set -X[pragma].
-- Exercise: Do that in your GHCi now. :set -XMonadComprehensions. How does it
-- work for other monads, like Maybe?

-- So we know that we're really just using the [] Monad. But what specifically
-- are we doing? Well, it's rather simple. Honestly.
--
--   [(x, y) | x <- xs, y <- ys]
--   xs >>= (\x -> ys >>= \y -> pure (x, y))
-- That's it. That's all. Comprehensions are just syntax sugar that desugar to
-- the regular honest to Church (>>=).
--
-- Exercise: What do the following expressions beta-reduce to?
--    [(x, y) | x <- [1, 2], y <- [3, 4]]
--    [(x, y) | x <- Just 1, y <- Just 2]
--
-- Exercise: What do the aforementioned expressions desugar to?

-- Another convenient type of sugar is called do-notation. It makes monads
-- much more sweet. Let's look at how it works with an expression we've
-- already used.
--   xs >>= (\x -> ys >>= \y -> pure (x, y))
-- In do-notation:
--   do x <- xs
--      y <- ys
--      pure (x, y)
