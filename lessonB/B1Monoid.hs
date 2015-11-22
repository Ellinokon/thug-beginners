module B1Monoid where

data List a = Nil
            | a `Cons` List a
            deriving (Show, Eq, Ord)

data Perhaps a = Nope
               | Have a
               deriving (Show, Eq, Ord)

append :: List a -> List a -> List a
-- Remember our append function?
Nil           `append` ys = ys
(x `Cons` xs) `append` ys = x `Cons` (xs `append` ys)

-- It's simple enough. [1, 2] `append` [3, 4] gives us [1, 2, 3, 4]. But what
-- if we have Perhaps [1, 2], and Perhaps [3, 4]?

appendPerhapsList :: Perhaps (List a) -> Perhaps (List a) -> Perhaps (List a)
-- Look "inside" the Haves or Nopes, and append the lists that might be in
-- there.
p      `appendPerhapsList` Nope   = undefined
Nope   `appendPerhapsList` p      = undefined
Have p `appendPerhapsList` Have q = undefined

appendPerhaps :: Perhaps [a] -> Perhaps [a] -> Perhaps [a]
-- OK, now do it for Perhaps [a], the built-in list. The append function for
-- built-in lists is (++) :: [a] -> [a] -> [a]
p      `appendPerhaps` Nope   = undefined
Nope   `appendPerhaps` p      = undefined
Have p `appendPerhaps` Have q = undefined

-- Notice how these two functions were pretty much exactly the same thing? Now
-- let's try using Maybe. Maybe is the built-in version of Perhaps.
--   data Maybe a = Nothing | Just a

appendMaybeList :: Maybe (List a) -> Maybe (List a) -> Maybe (List a)
p       `appendMaybeList` Nothing = undefined
Nothing `appendMaybeList` p       = undefined
Just a  `appendMaybeList` Just b  = undefined

appendMaybe :: Maybe [a] -> Maybe [a] -> Maybe [a]
p       `appendMaybe` Nothing = undefined
Nothing `appendMaybe` p       = undefined
Just a  `appendMaybe` Just b  = undefined

-- Are you bored yet? Good. Then you should be sufficiently motivated for our
-- next trick.
--
-- There are several problems above.
--
-- One is that we need to invent and remember all these different identifiers.
-- Imagine needing to use addFloat, addDouble, addInteger, and so on, instead
-- of just (+). The Num typeclass saves us this trouble. And, thankfully,
-- there are typeclasses to the rescue in this case too.
--
-- Another issue, that is a lot worse, is that when we get to kind * -> *, we
-- have to implement one function per type. So we had one for Maybe (List a),
-- and one for Maybe [a]. Now imagine having to do this for 100 types.
--
-- Luckily... you *don't*. You only have to implement it for the computational
-- context *once*. Welcome to semigroups and monoids.
--
-- A semigroup is a Set, S, and a binary operation, (<>), for combining
-- elements from S. Said operator must be associative.
--   (a <> b) <> c == a <> (b <> c)
--
-- Some trivial examples of semigroups include integers under addition or
-- multiplication. Another nice semigroup is... lists under appending!
--
-- Now let's look at the class.
class SemiGroup a where
  (<>) :: a -> a -> a

-- What could be easier? Now let's make List and [] into SemiGroups. (N.B. we
-- will use capitalised letters like SemiGroup, Monoid, and Functor, when
-- speaking about Haskell typeclasses. When speaking about the mathematical
-- concept, we will not capitalise them outside of regular prose convention.)
instance SemiGroup (List a) where
  (<>) = append

instance SemiGroup [a] where
  (<>) = (++)

instance SemiGroup a => SemiGroup (Perhaps a) where
  -- Now *you* make Perhaps a SemiGroup. This will look essentially like the
  -- append functions you wrote for Perhaps earlier, except now you have the
  -- polymorphic (<>) function at your disposal!
  p      <> Nope   = undefined
  Nope   <> p      = undefined
  Have p <> Have q = undefined

instance SemiGroup a => SemiGroup (Maybe a) where
  -- Do the same for Maybe.
  p       <> Nothing = undefined
  Nothing <> p       = undefined
  Just p  <> Just q  = undefined

-- Congratulations! You now have one polymorphic function for every use case.
--
-- In Haskell base, the SemiGroup typeclass unfortunately doesn't exist (yet).
-- There are some alternative Preludes, and various packages on Hackage, most
-- notably semigroups, <https://hackage.haskell.org/package/semigroups>.
--
-- We do have monoids though. And in Haskell, the (<>) function from SemiGroup
-- is simply bundled with the Monoid typeclass instead.
--
-- A monoid is a semigroup with an element e so that:
--   e <> x = x <> e = x
-- I.e. an element that when combined with any other element, using the (<>)
-- operator, is effectively the identity function.
--   e <> x == id x
--
-- Thus, monoids are sometimes simply called "semigroups with identity".
--
-- A proper monoid must satisfy three laws:
--   e <> x        = x
--   x <> e        = x
--   (x <> y) <> z = x <> (y <> z)
--
-- The implementation in Haskell looks like this:
--   class Monoid a where
--     mempty  :: a
--     mappend :: a -> a -> a
--
-- Where mempty is the 'e' we've been talking about, and mappend is (<>). The
-- mappend name is very unfortunate and misleading, since a Monoid may have
-- nothing to do with "appending". Is 5 + 5 appending 5 to 5? Not really.
-- Fortunately we at least have (<>) as an alias for mappend, so I recommend
-- using that instead.
--
-- Exercise: can you figure out what e is for integers under addition? What
-- about under multiplication? What is e for List, [], Perhaps, and Maybe?
