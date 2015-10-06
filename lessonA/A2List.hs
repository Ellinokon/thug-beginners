{-# LANGUAGE DeriveFoldable #-} -- Needed for the tests, don't worry!
module A2List where

import A1Nat

-- Now we're going take it up a notch. Both in terms of difficulty and in
-- terms of the type systems.
--
-- Our previous type Nat had kind '*'. If you don't believe me, go ahead and
-- do
--   :k Nat
-- in GHCi right now.
--
-- ... Ok, back? Believe me now? Then let me very briefly tell you about
-- kinds. A kind is a type but one level up. The "type of types", if you will.
-- Just like the value 3 has a type, its type has a kind. 3 has type Int, and
-- Int has kind *. Every data type in Haskell has kind *. This means that
-- every inhabited type (types that can have values) has kind *, and that * is
-- an inhabited kind (a kind that can have types). Well except #, which is for
-- raw values, but that's a bit complicated and off-topic. You can read about
-- it here: <https://wiki.haskell.org/Unboxed_type>.
--
-- Anyway. In this file you're going to be looking at things with kind * -> *.
-- Which is just like functions of type a -> a. Let me explain with an
-- example.
--
-- Let's start with Good old lists. Here's a simple list type, which we'll be
-- working with:
data List a = Nil                              -- The empty constructor.
            | a `Cons` List a                  -- The constructor.
            deriving (Show, Eq, Ord, Foldable) -- Deriving Show again. We'll
                                               -- just grab Eq and Ord for
                                               -- free this time around.
                                               -- Foldable is needed for the
                                               -- tests. Don't worry about it!

-- So we have List *a*. What does the 'a' mean? It means the same thing it
-- does in functions. "Any value".
--
-- Now if you do
--   :k List
-- in GHCi, what do you see?
--
-- Did you see * -> *? Good. What does this really mean? It means that List by
-- itself can't have a value. It doesn't make any sense, because it's not
-- a concrete type yet. It's just a *type constructor*. How do we make it
-- concrete?
--
-- Well if you have a function f :: a -> a, how do get a value 'a'? You
-- feed the function an 'a'! So how do we make * -> * a *? We feed it a '*'!
--
-- Remember how Int was an example of *? Well, you wouldn't believe what
--   :k List Int
-- Outputs!
--
-- Well, okay, maybe you've figured it out by now. It's *. So we can have
-- values of type List Int, List String, or List Nat, to use our type from A1.
-- But we can't have values of * -> *. Got it? Good. Now let's move on and see
-- how we can use our List type.
--
-- But first, Imma just set the Cons fixity for you real quick. This line
-- simply means that Cons associates from the right, and with a precedence of
-- 5. Don't worry about it for now. It just means we can write Cons like this:
-- 1 `Cons` 2 `Cons` 3 `Cons` Nil rather than having to write it like this:
-- 1 `Cons` (2 `Cons` (3 `Cons` Nil))
infixr 5 `Cons`

-- OK, let's hack!
append :: List a -> List a -> List a
-- First off we want to be able to concatenate two Lists. This function should
-- add the two lists together, the right one at the end of the left one.
Nil           `append` ys = undefined
(x `Cons` xs) `append` ys = undefined

-- You see how we use the pattern of x `Cons` xs a lot? We call x the head,
-- and xs the tail. This operation is not entirely safe. Try doing this in
-- GHCi:
--   let h (x `Cons` xs) = x
--   h (1 `Cons` 2 `Cons` 3 `Cons` Nil)
--   h Nil
-- Do you see how this blows up? h works on any value List a. Nil is a List a.
-- But there's no Cons to match on! Oh, no! What do we do?
--
-- Well, we could have
--   h :: List Int -> Int
--   h (x `Cons` xs) = x
--   h Nil           = 0
-- But doing that for every type would be tedious at best, and nonsensical at
-- times.
--
-- We simply need a type to denote the lack of a value.
--
-- Introducing: Perhaps. Perhaps we Have something, Perhaps, Nope, we don't!
data Perhaps a = Nope   -- Means we don't have anything. Nope   :: Perhaps a
               | Have a -- Means we have something.      Have 5 :: Perhaps Int
               deriving (Show, Eq, Ord)

headList :: List a -> Perhaps a
-- Write a safe version of the h function we saw above, that indicates whether
-- we Have something or Nope.
headList Nil           = undefined
headList (x `Cons` xs) = undefined

tailList :: List a -> Perhaps (List a)
-- Now write the opposite function, give us the tail instead of the head.
tailList Nil           = undefined
tailList (x `Cons` xs) = undefined

lastList :: List a -> Perhaps a
-- Let's turn the concepts around. That's a bit more tricky. Instead of the
-- first and rest, we want the last and rest. Let's start with the last member
-- of the List. I'll give you that one for free.
--
-- First we take care of the Nil List. That one is simple. There is no last
-- element.
lastList Nil            = Nope
-- Then we get to the interesting bit. If there is just one member, then
-- that's the last member, obviously. But if there are more, then we use
-- recursion. We try to figure out, what's the last member of the tail of that
-- list?
lastList (x `Cons` Nil) = Have x
lastList (x `Cons` xs)  = lastList xs
-- Note that we could have written:
--   lastList xs = lastList (tailList xs)
-- To reuse our tailList function.
--
-- Finally, note that *order matters*! What do you think would happen if you
-- put the two equations in the opposite order? If you can't figure it out as
-- a mental exercise, go ahead and try to rearrange them.

initList :: List a -> Perhaps (List a)
-- Now for the rest. I'll give you most of it, but not the main bit. That's
-- for you to figure out!
initList Nil            = Nope
initList (x `Cons` Nil) = Nope
initList (x `Cons` xs)  = Have (go x xs)
  -- Here we're doing something we haven't done before. The "where" keyword
  -- lets us define some local values or functions that are only in scope
  -- inside of the scope it is defined -- i.e. it is not a top-level
  -- declaration like our other functions.
  --
  -- If there is only a single helper-function like this, it is often called
  -- "go". The reason we use new variables instead of reusing x and xs is to
  -- avoid "shadowing", where you have different variables inside of a scope,
  -- that have the same name. This gets very confusing, so best avoid it.
  where go _ Nil           = Nil
        go y (z `Cons` zs) = undefined

-- For the rest of this module I'll be far more hands off, only giving you
-- pointers when there's something novel. I'm not even going to give you the
-- necessary equations for free this time.
--
-- Good luck!

unconsList :: List a -> Perhaps (a, List a)
-- Unconstruct the list into a tuple of its head and tail, or Nope if empty.
-- () is for tuples. (a, b, c, d, ...). Note that it is rarely used for more
-- complicated structures than pairs.
unconsList = undefined

nullList :: List a -> Bool
-- Check if the List is empty.
nullList = undefined


-- Now for the "recurse down to the basecase, doing something to each element
-- of the List along the way"-section. I will give you one example of what
-- I mean. All other functions in this section will look quite like this
-- example.
lengthList :: List a -> Nat
-- Find the length of the List -- i.e. the number of elements.
lengthList Nil           = Z
lengthList (x `Cons` xs) = S Z `plus` lengthList xs
-- The length of the Nil List is obviously zero The length of a populated list
-- is one + the length of its tail. You got that?
--   length (one `Cons` two `Cons` three `Cons` Nil) ==
--     one + length (two `Cons` three `Cons` Nil)    ==
--     two + length (three `Cons` Nil)               ==
--     three + length Nil                            ==
--     three `plus` Z                                == three
-- (I will use "one", "two" and so on as shorthands for "S Z", "S (S Z)" and
-- so on. You can define that yourself if you want.)
--
-- This "recurse to the base condition" idea is the pattern you'll use for
-- this section. Figure out what you need to do for every element, and what
-- you need to do at the end. You might have to use the "go"-idiom as well.

sumList :: List Nat -> Nat
-- Sum a list of Nats.
--   sumList (three `Cons` two `Cons` one `Cons` Nil) = six
sumList = undefined

productList :: List Nat -> Nat
-- The product of a list of numbers.
--   productList (three `Cons` four `Cons` five `Cons` Nil) = sixty
-- The product of the Nil List is one.
productList = undefined

maximumNatList :: List Nat -> Perhaps Nat
-- Given a list of Nats, which is the biggest? Remember to handle the Nil
-- List!
maximumNatList = undefined

minimumNatList :: List Nat -> Perhaps Nat
-- And now the smallest.
minimumNatList = undefined

takeList :: Int -> List a -> List a
-- This is kind of like headList, but it takes an Int (we're ditching our Nats
-- for now and just using regular Ints) denoting how many to take. So e.g.
-- "takeList 3" should give the first three members of the list. take-ing
-- negative numbers should not be permitted.
takeList = undefined

dropList :: Int -> List a -> List a
-- The dual of takeList. "dropList 3" gives the list except for the first
-- three members.
dropList = undefined

splitList :: Int -> List a -> (List a, List a)
-- Combining takeList and dropList, we can get a tuple of e.g. both the first
-- three, and the list without the first three.
splitList = undefined

reverseList :: List a -> List a
-- Reverse a list.
reverseList = undefined

intersperseList :: a -> List a -> List a
-- Put the element given in between every element of the list.
--   intersperseList 8 (1 `Cons` 2 `Cons 3 `Cons` Nil) ==
--   1 `Cons` 8 `Cons` 2 `Cons` 8 `Cons` 3 `Cons` Nil
intersperseList = undefined

concatList :: List (List a) -> List a
-- Concatenate a list of lists -- i.e. flatten the List of Lists by using
-- append on every element. A List can be an element of another List. 'a'
-- really means any type, as long as it has kind *. It can be an Int, or
-- a List (List (Preferable Int)). Anything that's kind *
concatList = undefined

intercalateList :: List a -> List (List a) -> List a
-- Intersperse a List in a List of Lists, concatenating the result.
intercalateList = undefined

zipList :: List a -> List b -> List (a, b)
-- Take two lists, zip them together as pairs, discarding excess elements.
--   zipList (1 `Cons` 2 `Cons` 3 `Cons` Nil) (2 `Cons` 4 `Cons` Nil) ==
--   (1, 2) `Cons` (4, 2) `Cons` Nil
zipList = undefined

elemList :: (Eq a) => a -> List a -> Bool
-- Check if the passed in value is in the list. The "=>" syntax means that rhs
-- variables must satisfy the qualifications from lhs. Here we have Eq a,
-- which means that for all a, they must implement the Eq typeclass (which
-- gives us equality operators like (==). Note that Nil is not a part of the
-- Nil List! But it *is* a part of a List of Lists where one of the Lists are
-- the Nil List.
elemList = undefined
