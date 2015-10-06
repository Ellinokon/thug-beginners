module A3BreadButter where

import Data.List
  (
  intersperse, -- We'll need this later -- don't worry about it.
  )

-- In this module we're going to learn about mapping, filtering, and folding.
-- "map" and "filter" are often called the bread and butter of functional
-- programming. I guess that makes "fold' the brunost.
--
-- In this module we'll be using Haskell's builtin list type. It's like ours,
-- but with some neat syntax sugar. You can imagine it written like this:
--   data [a] = []
--            | a : [a]
-- The special syntax [a] means the same as our List a; you could imagine it
-- being written '[] a". There is the unfortunate confusion here that lists
-- have the same name on value level as type level. I.e., [] can be the empty
-- list (our Nil), and it can also be the type constructor of kind *->* that
-- must be concretised like e.g. [Int] to get kind *. Hopefully this won't be
-- too confusing. Legacy code, you know how it is.

-- So let's start off with mapping. mapList is going to take a function and
-- apply it to every member of the list. If you had a list of [1, 2, 3], and
-- wanted to apply (+ 1) to each member, you could then do:
--   mapList (+1 ) [1, 2, 3]
-- Simple as that! Let's get started.

mapList :: (a -> b) -> [a] -> [b] --  (a -> b) -> List a -> List b
-- Some new syntax here -- parentheses in the type signature. What do they
-- mean? Short version: "(a -> b)" in a type signature means "a function from
-- a to b". In Haskell, functions are first-class values. This means functions
-- can take functions as arguments.
--
-- Anyway, go ahead and implement it now. It's the same basic recurse to
-- basecase stuff you've done before. Just apply the passed in function to
-- every member of the list. Remember that Cons is now (:) and Nil is now [].
mapList = undefined

-- That was mapping. Now filtering. Filtering means applying a predicate
-- on every member of a list, only keeping the elements that pass.
--   filter (>3) [1, 2, 3, 4, 5] == [4, 5]
-- Got it? Now implement it!
filterList :: (a -> Bool) -> [a] -> [a]
filterList = undefined

-- Now let's practice map and filter a bit.

add42List :: [Int] -> [Int]
-- Add 42 to every member of a list. Use the builtin + function -- 1 + 2 == 3.
add42List = undefined

invertBoolList :: [Bool] -> [Bool]
-- Turn every True False, and every False True.
invertBoolList  = undefined

geq42List:: [Int] -> [Int]
-- Only keep members that are 42 or more. You'll want to use an Ord function:
-- https://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#t:Ord
geq42List = undefined

onlyTrueList :: [Bool] -> [Bool]
-- Only keep Trues, throw out any Falses.
onlyTrueList = undefined

-- Now we'll look at some exercises where you'll want to compose or partially
-- apply functions. First I'll explain what this means.
--
-- a neat thing in Haskell is that every function is *curried*. Every function
-- in reality only accepts a single argument, and returns a single value. How
-- can this be? Well, (->) is a type-level operator that is right associated,
-- which lets us omit parentheses. But we can readd them to demonstrate the
-- point:
--   add :: Int -> Int -> Int ==
--   add :: Int -> (Int -> Int)
-- If add is a function that adds two numbers, it takes two numbers and
-- returns the result. But that's only a superficial simplified understanding.
-- What it *really* does is that first it takes a number, then it returns
-- a function that accepts the second number and gives back the result. That's
-- kind of cool -- but what are the implications? Well, we can *partially
-- apply* the function. That means we only give it that first argument, so
-- that we can get a new function!
--   add :: Int -> Int -> Int
--   add x y = x + y
--
--   add42 :: Int -> Int
--   add42 x = add 42 x
--
--  See how we got an add42 function by just reusing add? We could have
--  written this in a eta-reduced style, where the arguments are implicit.
--    add42 :: Int -> Int
--    add42 = add 42
--  This highlights the partial application. Many people prefer this
--  eta-reduced style. It is often called a "point free" style. But use it
--  sparingly. It can result in more obscure code, which is where the joke
--  "point less" style comes from.

-- Before we move on, go ahead and practice partial application a bit.

times2 :: Int -> Int
-- Partially apply the (*) operator to give us a function that multiplies its
-- arguments with two.
times2 = undefined

take4 :: [a] -> [a]
-- Get the first four members of the given list. Use the builtin "take"
-- function. It's just like the takeList function we wrote.
take4 = undefined

intersperse0 :: [Int] -> [Int]
-- Intersperse a list with the element 0. Use the builtin "intersperse"
-- function, which is like our intersperseList.
intersperse0 = undefined

-- That was simple enough. Now composition. We can compose functions together
-- to get a new function. This is one of the most important things in all of
-- functional programming. In Haskell the compose function is:
--   (.) :: (b -> c) -> (a -> b) -> a -> c
--   (f . g) x = f (g x)
-- The concept is very simple. Apply one, then the other. We can partially
-- apply this to get new functions. "f . g" is a new function that takes an
-- argument "x". Let's practice this a bit too. Note that you can (and
-- probably should) combine composing with partial application!

take4intersperse0 :: [Int] -> [Int]
-- Yup, you guessed it. intersperse the list with 0, then take the first four.
take4intersperse0 = undefined

t4i0t2 :: [Int] -> [Int]
-- Do the same, but map times2 on every element as well.
t4i0t2 = undefined

letterT :: String -> String
-- Only keep the letter t (both 'T' and 't'). The String type is a synonym for
-- [Char], i.e. a list of characters. Characters are written with 's, and
-- strings with "s.
--   "Hallo!" == 'H' : 'A' : 'L' : 'L' : 'O' : '!' : []
letterT = undefined

notElemList :: (Eq a) => a -> [a] -> Bool
-- Remember our elemList? Now write notElemList, using the builtin lists and
-- the "elem" and "not" functions. elem is like our elemLists.
--   not :: Bool -> Bool
--   not True  = False
--   not False = True
notElemList = undefined

-- And, now, finally. For the delicious brunost. Folding is reducing a list
-- while applying a binary function to every member. It's pretty simple,
-- really. We give it a binary function, a basecase, and a list to work on.
-- The binary function is applied to the basecase and the first element of the
-- list, to which is produces a new value. Then the function is called again
-- with the new value and the next element. And so on. Let's look at fold.
fold :: (a -> b -> b) -> b -> [a] -> b
fold _ z []     =  z
fold f z (x:xs) =  f x (fold f z xs)

-- That's it. As you may have observed, this is the simple "recurse to
-- basecase" pattern once more! append, lengthList, sumList, reverselist,
-- mapList, and filterList are all following the same principle. This
-- principle is encapsulated in the fold. So for our final exercises this time
-- around, you're to reimplement those using fold.

-- To start you off, I'll, in fine textbook fashion, give you the easiest one
-- for free:
--   sumFold :: [Int] -> Int
--   sumFold = fold f 0
--     where f x y = x + y
-- This is using the "where"s we already know. We could also use a lambda (an
-- anonymous (nameless) function) for the job:
--   sumFold = fold (\x y -> x + y) 0
-- This is a common way to write it. We can eta-reduce it to its *most* common
-- form:
sumFold :: [Int] -> Int
sumFold = fold (+) 0

-- When working through the folds you're left to implement yourself, pick the
-- style that feels the most comfortable to you. If you're really
-- enthusiastic, do all three!

lengthFold :: [a] -> Int
lengthFold = undefined

reverseFold :: [a] -> [a]
reverseFold = undefined

appendFold :: [a] -> [a] -> [a]
appendFold = undefined

mapFold :: (a -> b) -> [a] -> [b]
mapFold = undefined

filterFold :: (a -> Bool) -> [a] -> [a]
filterFold = undefined
