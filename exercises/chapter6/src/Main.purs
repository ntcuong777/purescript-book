module Main where

import Prelude

import Data.Array (nub, nubEq)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Generic.Rep (class Generic)
import Data.Hashable (hash, hashEqual)
import Data.Newtype (class Newtype, over2, wrap)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Class.Console (logShow)

main :: Effect Unit
main = do
  logShow $ hash 123
  logShow (hash true)
  logShow (hash [1, 2, 3])
  logShow (hash "testing")
  logShow (hash 'a')
  logShow ("foo" `hashEqual` "foo")
  logShow ("foo" `hashEqual` "bar")

newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance Show Point where
  show (Point p) = "(" <> show p.x <> ", " <> show p.y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

derive newtype instance eqComplex :: Eq Complex

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) = show real <> showIma <> "i"
    where
      showIma = (if imaginary >= 0.0 then "+" else "") <> show imaginary

derive instance newtypeComplex :: Newtype Complex _

instance complexSemiring :: Semiring Complex where
  zero = wrap { real: 0.0, imaginary: 0.0 }
  one = wrap { real: 1.0, imaginary: 0.0 }
  add = over2 Complex (+)
  mul = over2 Complex
    \ { real: a, imaginary: b}
      { real: c, imaginary: d} -> { real: a * c - b * d
                                  , imaginary: a * d + b * c
                                  }

derive newtype instance ringComplex :: Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

data NonEmpty a = NonEmpty a (Array a)

derive instance Generic (NonEmpty a) _

instance Show a => Show (NonEmpty a) where
  show = genericShow

instance Eq a => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys

instance Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [ y ] <> ys)

instance Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

instance Monoid a => Monoid (NonEmpty a) where
  mempty = NonEmpty mempty []

instance Foldable Array => Foldable NonEmpty where
  foldr f acc (NonEmpty x xs) = f x $ foldr f acc xs
  foldl f acc (NonEmpty x xs) = foldl f (f acc x) xs
  foldMap f (NonEmpty x xs) = f x <> foldMap f xs

{- Solution does this:
derive instance Functor NonEmpty
-}

data Extended a = Infinite | Finite a

instance Eq a => Eq (Extended a) where
  eq Infinite Infinite = true
  eq (Finite x) (Finite y) = x == y
  eq _ _ = false

instance (Eq a, Ord a) => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite x) (Finite y) = compare x y

data OneMore f a = OneMore a (f a)

instance Foldable f => Foldable (OneMore f) where
  foldr f acc (OneMore x xs) = f x $ foldr f acc xs
  foldl f acc (OneMore x xs) = foldl f (f acc x) xs
  foldMap f (OneMore x xs) = f x <> foldMap f xs

derive newtype instance Eq Point
derive instance Eq Shape
derive newtype instance Ord Point
derive instance Ord Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub
