module Main where

import Data.Maybe
import Data.Path
import Prelude

import Control.Alternative (guard)
import Control.Monad.Trampoline (done)
import Data.Array (concatMap, filter, head, length, range, tail, (..), (:))
import Data.Foldable (foldl)
import Data.Int (floor, toNumber)
import Data.Number (sqrt)
import Effect (Effect)
import Effect.Console (log)
import Test.Examples (allFiles, factorsV3)

main :: Effect Unit
main = do
  log "🍝"

isEven :: Int -> Boolean
isEven n = (mod n 2) == 0

countEven :: Array Int -> Int
countEven [] = 0
countEven lst = v + countEven tailOfLst
  where
  maybeEven :: Maybe Int -> Boolean
  maybeEven Nothing = false
  maybeEven (Just x) = isEven x

  v = if maybeEven (head lst) then 1 else 0

  parseTail :: forall a. Maybe (Array a) -> Array a
  parseTail Nothing = []
  parseTail (Just a) = a

  tailOfLst = (parseTail <<< tail) lst

squared :: Array Number -> Array Number
squared lst = (\x -> x * x) <$> lst

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (_ >= 0.0)

infix 4 filter as <$?>

keepNonNegativeRewrite ∷ Array Number → Array Number
keepNonNegativeRewrite lst = (_ >= 0.0) <$?> lst

isPrime ∷ Int → Boolean
isPrime 1 = false
isPrime x = (length cf) == 1
  where
  cf = factorsV3 x

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct arr1 arr2 = do
  i <- arr1
  j <- arr2
  pure [ i, j ]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [ a, b, c ]

divideFactor ∷ Int → Int → { factors ∷ Array Int, remain ∷ Int }
divideFactor n v = doDivide n v []
  where
  doDivide :: Int -> Int -> Array Int -> { factors :: Array Int, remain :: Int }
  doDivide n v res =
    if mod n v /= 0 then { factors: res, remain: n }
    else doDivide (n / v) v (res <> [ v ])

primeFactors ∷ Int → Array Int
primeFactors 0 = []
primeFactors 1 = []
primeFactors n = findPrimeFactors n 2 []
  where
  findPrimeFactors n v res
    | n == 1 = res
    | v > floor (sqrt (toNumber n)) = if n > 1 then res <> [ n ] else res
    | mod n v /= 0 = findPrimeFactors n (v + 1) res
    | otherwise = findPrimeFactors d.remain (v + 1) (res <> d.factors)
        where
        d = divideFactor n v

allTrue ∷ Array Boolean -> Boolean
allTrue = foldl (\acc x -> acc && x) true

{-
(Medium - No Test) Characterize those arrays xs for which the function foldl (==) false xs returns true.
In other words, complete the sentence: "The function returns true when xs contains ..."
-> The function returns true when xs contains an odd number of `false` values.
-}

fibTailRec ∷ Int → Int
fibTailRec 0 = 0
fibTailRec 1 = 1
fibTailRec n = innerTailRec 0 1 (n - 1)
  where
  innerTailRec f1 f2 cnt =
    if cnt == 0 then f2
    else innerTailRec f2 (f1 + f2) (cnt - 1)

reverse :: forall a. Array a -> Array a
reverse = foldl (\acc x -> [ x ] <> acc) []

-- allFiles :: Path -> Array Path
-- allFiles file = file : concatMap allFiles (ls file)

onlyFiles :: Path -> Array Path
onlyFiles file
  | isDirectory file = concatMap onlyFiles (ls file)
  | otherwise = file : concatMap onlyFiles (ls file)

whereIs :: Path -> String -> Maybe Path
whereIs path fname = head $ do
  dir <- allFiles path
  child <- ls dir
  guard $ (filename child) == (filename dir) <> fname
  pure dir

largestSmallest :: Path -> Array Path
largestSmallest path = foldl loop [] (onlyFiles path)
  where
    loop r@[f1, f2] f
      | size f1 < size f = [f, f2]
      | size f < size f2 = [f1, f]
      | otherwise = r
    loop [f1] f = if size f1 < size f then [f, f1] else [f1, f]
    loop r f = f : r
