module Main
  ( Complex(..)
  , Point(..)
  , main
  )
  where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Data.Hashable (hash, hashEqual)

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

f x = x * f (x - 1)
