{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Monoid where
import Prelude hiding (Monoid(..))

type Compose a =
  a -> a -> a -- uncurried form of (a, a) -> a

class Semigroup a where
  (<>) :: Compose a -- x <> (y <> z) = (x <> y) <> z

instance Semigroup Int where
  i <> j = i + j -- could also use *, max etc

instance Semigroup Bool where
  b <> b' = b && b'  -- could also use ||, xor

instance Semigroup [a] where
  xs <> ys = xs ++ ys -- concatenation

instance Semigroup b => Semigroup (a -> b) where
  --    /-- f -- \
  -- --x          <> --
  --    \-- g -- /
  f <> g = \x -> f x <> g x

type Predicate a = a -> Bool

anagram str = str == reverse str
evenLength = even . length

-- compose predicates
evenAnagram = anagram <> evenLength

----------------------------------------------------------------------

class Semigroup a => Monoid a where
  empty :: a   -- x <> empty = x = empty <> x

instance Monoid Int where
  empty = 0

instance Monoid Bool where
  empty = True

instance Monoid [a] where
  empty = []

instance Monoid b => Monoid (a -> b) where
  empty = const empty

-- For predicates: Why is empty <> f = f?

fold [] = empty
fold (x:xs) = x <> fold xs

-- Add a wrapper to avoid conflics with instance for (a -> b)
newtype Transformation a = T (a -> a)

instance Semigroup (Transformation a) where
  T f <> T g = T (f . g)

instance Monoid (Transformation a) where
  empty = T id  -- the identity function is the empty element for
                -- functions composition

-- Pairs of monoids form monoids

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a,x) <> (b,y) = (a <> b, x <> y)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  empty = (empty, empty)

-- Example: monoids for measuring performance

type Time = Float

instance Semigroup Time where
  (<>) = (+)

instance Monoid Time where
  empty = 0

-- fastest time

newtype FastestTime = Fastest Time
  deriving Show

instance Semigroup FastestTime where
  Fastest time1 <> Fastest time2 =
    Fastest (min time1 time2)

instance Monoid FastestTime where
  empty = Fastest (1/0) -- negative Infinity

-- slowest time

newtype SlowestTime = Slowest Time
  deriving Show

instance Semigroup SlowestTime where
  Slowest time1 <> Slowest time2 =
    Slowest (max time1 time2)

instance Monoid SlowestTime where
  empty = Slowest (-1/0) -- negative Infinity

-- average time

data AverageTime = Average (Int, Time)

instance Show AverageTime where
  show  (Average (i,total)) = "Average "  ++ show (total / fromIntegral i)

instance Semigroup AverageTime where
  Average a <> Average b = Average (a <> b)

instance Monoid AverageTime where
  empty = Average empty

statistics =
  fold . map (\t -> (Slowest t, (Average (1, t), Fastest t)))

test = statistics [2,8,9,34,10]
-- >> (Slowest 34.0,(Average 12.6,Fastest 2.0))
