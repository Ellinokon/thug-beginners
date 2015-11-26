module A1Nat where

-- This is our simple Natural numbers data type.
--
-- S is a successor, Z is zero.
-- 1 is S Z (the successor of zero).
-- 2 is S (S Z).
-- And so on.
data Nat = Z           -- Zero
         | S Nat       -- Successor
         deriving Show -- This is a special syntax for automatically giving us
                       -- the implemnetation of a typeclass. Here we derive
                       -- the Show typeclass, which lets us convert values of
                       -- our Nat type into Strings to print e.g. in GHCi.

-- Some terminology real quick: "data" is the keyword to make algebraic data
-- types. Data types in Haskell are algebraic because they can be sums
-- (alternations) or products (combinations).
--   data D     = A | B
--   data D e f = G H I
-- Both of these are perfectly acceptable. In here D is the type, and above
-- Nat is our type. A, B, and G are data constructors. In our type, Z and
-- S are data constructors.  This means that they are *values* not *types*.
-- 3 is a *value* of *type* Int. This is an important distinction. H and I are
-- the types of the arguments to G, like our S takes a Nat. That means you
-- can't write S 2 or S "Hallo!", because 2 and "Hallo!" are not Nats. You can
-- however write S Z, denoting the sucessor of zero, i.e. the natural number
-- 1.

-- Now let's get hacking! First of we want a notion of equality for our number
-- system.
instance Eq Nat where
  -- This means that we are making our Nat type an instance of the Eq type
  -- class, which defines equality (==) and inequality (=/).
  --
  -- I've given you the patterns you need to match. But you'll have to figure
  -- out the code yourself. (==) has the type a -> a -> Bool, where 'a' is an
  -- instance of Eq. Here we are going to define how Nat is an instance of Eq.
  -- So here it effectively has the type Nat -> Nat -> Bool.
  Z     == Z     = True      -- Is Z (zero) equal to Z?
  (S x) == (S y) = x == y    -- Is one number equal to a different number?
                             -- Hint: Use recursion!
  _     == _     = False     -- This is what we call the base case, or the
                             -- catch all. '_' means we don't bind the value
                             -- to a variable at all.

-- Note that we did not have to implement inequality (=/) at all. Why is
-- this? Because Haskell's implementation of Eq is quite clever. It is simply
-- the mutually recursive definition:
--   a == b = not (a =/ b)
--   a =/ b = not (a == b)
-- This is the default definition that you get. They don't make much sense by
-- themselves, as they will never terminate. But what they mean in practice is
-- that if you implement one, you get the other for free, since it is
-- automatically "the opposite of the other one".
--
-- The syntax is simple enough here. "value == value". (==) here is an
-- operator. Operators are just functions. We could write it prefix:
--   (==) Z Z = ...
-- Any function may be written infix or prefix. It's a matter of taste and
-- convenience.

-- Now let's get going and write some functions!
--
-- We'll introduce syntax as we go. But let's explain type signatures straight
-- away. If we have a function 'f' which takes an Int and returns an Int, we
-- say that f has type Int -> Int. "Has type" is written "::".
--   f :: Int -> Int
-- This means we must feed f an Int to get us an Int.
--
-- Types always start with an uppercase letter. If you use lowercase letters,
-- you are using type variables instead. This can be used for polymorphism:
--   f :: a_type -> another_type -> a_type -> another_type
--   f _ y _ = y
-- Here we need to give f three values. Two of them have to be the same type
-- (a_type), but that type can be any type, like Int or String. the middle one
-- can be any type, including the same as the a_type arguments.
--
-- You'll observe that we bind it to 'y'. y here is just a variable (not
-- a *type variable*, but a variable on the value level). It matches anything
-- of value another_type. Consider this:
--   f :: Nat -> String
--   f Z     = "Zero"
--   f (S Z) = "One"
--   f a     = "Something else:" ++ show a
-- Here we match Z specifically, then we match (S Z) specifically, then we
-- match anything of value Nat as a. It's just like the basecase above where
-- we matched anything as _, but here we also bind it to a named variable that
-- we can use in the function body.
--
-- We could call it almost what we like, but we prefer very short variable
-- names because of the short scope of their existence.
--
-- We usually write short type variables too, like "f :: a -> b -> c -> b".

isZ :: Nat -> Bool
-- We'll start off with a very simple function -- isZ. Is the argument given
-- in Zero?
isZ Z = True      -- As you can see, I'm really spoonfeeding you here.
                  -- Is Z equal to Z, do you think?
isZ _ = False     -- Another base case, because we only care about Z.

toNat :: Int -> Nat
-- Now let's make add some Int interop.
--
-- In this function we are using a new syntax you have not seen yet.
--
-- f | p         = x
--   | p2        = y
--   | otheriwse = z
--
-- The parts between the '|'s and '='s are prepositions. So simple boolean
-- expressions. If p, then x. If p2, then y. Otherwise (base case) z.
--
-- This function will take an Int and give you a Nat.
--   toNat 0 = Z
--   toNat 1 = S Z
--   toNat 2 = S (S Z)
-- And so on.
--
-- Numbers below 0 should result in Z.
toNat n | n > 0     = S (toNat (n - 1))
        | otherwise = Z

fromNat :: Nat -> Int
-- Now let's go the other way around. Here we don't have to worry about n < Z,
-- because that can't happen.
fromNat Z     = 0
fromNat (S n) = 1 + (fromNat n)

predNat :: Nat -> Nat
-- predNat takes a Nat and gives us its predecessor.
--
--   predNat S (S (S N)) = S (S N)
-- Like that. Remember that our number system doesn't go any lower than Z.
predNat Z     = Z
predNat (S n) = n

succNat :: Nat -> Nat
-- And here's the successor function for symmetry. Hint: if you think it's too
-- simple to be true, you probably have the answer to this one.
succNat = S

plus :: Nat -> Nat -> Nat
-- And now some classic arithmetic. Let's start out with addition.
-- Hint: Recursion! Of course recursion. Always recursion.
Z     `plus` y = y
(S x) `plus` y = S (x `plus` y)

times :: Nat -> Nat -> Nat
-- Next up is multiplication.
Z     `times` _ = Z
(S x) `times` y = y `plus` (x `times` y) -- Hint: You probably want to use plus here.

powerOf :: Nat -> Nat -> Nat
-- Power of. The first argument is the base case, the second is the exponent.
_ `powerOf` Z     = S Z
b `powerOf` (S e) = b `times` (b `powerOf` e)

minus :: Nat -> Nat -> Nat
-- Subtraction.
Z     `minus` _     = Z
x     `minus` Z     = x
(S x) `minus` (S y) = x `minus` y

lteNat :: Nat -> Nat -> Bool
-- Moving onto ordering. This is the less-than-or-equal-to function for Nats.
-- Is the left-hand side argument (lhs) smaller than or equal to the
-- right-hand side (rhs) argument?
Z     `lteNat` y     = True
x     `lteNat` Z     = False
(S x) `lteNat` (S y) = x `lteNat` y

ltNat :: Nat -> Nat -> Bool
-- The less-than function. Hint: You probably want to use lteNat and succNat.
ltNat x y = (succNat x) `lteNat` y

gteNat :: Nat -> Nat -> Bool
-- The greater-than-or-equal-to function. Hint: Reuse!
gteNat x y = y `lteNat` x

gtNat :: Nat -> Nat -> Bool
-- The greater-than function.
gtNat x y = y `ltNat` x

minNat :: Nat -> Nat -> Nat
-- The minimum function. Given two Nats, which is the smallest one?
minNat Z     _     = Z
minNat _     Z     = Z
minNat (S x) (S y) = S (minNat x y)

maxNat :: Nat -> Nat -> Nat
-- And now the maximum.
maxNat Z     y     = y
maxNat x     Z     = x
maxNat (S x) (S y) = S (maxNat x y)

instance Ord Nat where
  -- Of course all those order functions could be given to us a lot more
  -- cheaply. We could just make our data type an instance of the Ord type
  -- class. This gives us all of those functions, and more, for free! All we
  -- need to do is to implement the compare function.
  --
  -- Compare takes two Nats and returns one of three values: EQ, LT or GT. EQ
  -- means that the values are equal, LT that lhs is smaller than rhs, and GT
  -- the opposite of LT.
  compare Z Z         = EQ
  compare Z (S y)     = LT
  compare (S x) Z     = GT
  compare (S x) (S y) = compare x y
  -- Of course, while we're being honest here, we don't *really* have to do
  -- *any* of this. We can just derive Ord for completely free, like we did
  -- with Show. But it was a fun exercise, right? The functions you get from
  -- Ord are listed here in the documentation:
  -- https://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#t:Ord

fact :: Nat -> Nat
-- Let's do a couple more. First the factorial. The factorial is the product
-- of all the numbers less than the argument, and the number itself.
--
--   fact (S (S (S Z))) = S (S (S (S (S (S Z)))))
-- Or, if we used Ints it would look like this.
--   fact 3 = 6
-- Because 1 * 2 * 3 = 6. Get it? Good. Now implement it!
fact Z     = S Z
fact (S n) = (S n) `times` (fact n)

fib :: Nat -> Nat
-- No tutorial is complete without "Hallo, world!". But in functional
-- programming, we tend to implement the fibonacci sequence instead! The nice
-- thing about the fibonacci sequence is that its mathematical definition maps
-- pretty perfectly to Haskell.
--
-- The definition is as follows:
--   fib n = fib (n - 1) + fib (n - 2)
-- With two seeds:
--   fib 0 = 0
--   fib 1 = 1
--
-- Now you give it a go, using our Nat type.
fib Z         = Z
fib (S Z)     = S Z
fib (S (S n)) = (fib (S n)) `plus` (fib n)
