module Main where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Control.Monad.Reader (Reader)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Number (pi)
import Data.Person (Person)
import Data.Picture (Picture, Point, Shape(..), bounds, getCenter, origin, shapeBounds)
import Effect (Effect)
import Effect.Console (log)


main :: Effect Unit
main = do
  log "🍝"

factorial :: Int -> Int
factorial n | n <= 1 = 1
            | otherwise = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial n k =
  fact_n / (fact_k * fact_nk)
  where
    fact_n = factorial n
    fact_k = factorial k
    fact_nk = factorial (n - k)

pascal :: Int -> Int -> Int
pascal n k =
  b1 + b2
  where
    b1 = binomial (n - 1) k
    b2 = binomial (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } } = c1 == c2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton default [] = default
fromSingleton _ [x] = x
fromSingleton default _ = default

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

translateShape :: Point -> Shape -> Shape
translateShape d (Circle c r) = Circle (c + d) r
translateShape d (Rectangle c w h) = Rectangle (c + d) w h
translateShape d (Line p1 p2) = Line (p1 + d) (p2 + d)
translateShape d (Text c t) = Text (c + d) t
translateShape d (Clipped pic c w h) = Clipped translatedPics (c + d) w h
  where
    translatedPics = map (translateShape d) pic

translatePic :: Point -> Picture -> Picture
translatePic d = map (translateShape d)

centerShape :: Shape -> Shape
centerShape (Circle _ r) = Circle origin r
centerShape (Rectangle _ w h) = Rectangle origin w h
centerShape l@(Line p1 p2) = Line (p1 - delta) (p2 - delta)
  where
    delta = getCenter l
centerShape (Text _ text) = Text origin text
centerShape (Clipped pic c w h) = Clipped (translatePic (origin - c) pic) origin w h

scaleShape :: Number -> Shape -> Shape
scaleShape s (Circle p r) = Circle p (r * s)
scaleShape s (Rectangle p w h) = Rectangle p (w * s) (h * s)
scaleShape s (Line p1 p2) = Line (p1 * ss) (p2 * ss)
  where
    ss :: Point
    ss = {x: s, y: s}
scaleShape _ t@(Text _ _) = t
scaleShape s (Clipped pic c w h) = Clipped scaledPics c (w * s) (h * s)
  where
    scaledPics = map (scaleShape s) pic

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = centerShape >>> scaleShape 2.0

shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text
shapeText _ = Nothing


newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a * v)

area :: Shape -> Number
area (Circle _ r) = pi * r * r
area (Rectangle _ w h) = w * h
area (Line _ _) = 0.0
area (Text _ _) = 0.0
area cl@(Clipped _ _ _ _) = width * height
  where
    cBound = shapeBounds cl
    width = cBound.right - cBound.left
    height = cBound.top - cBound.bottom
