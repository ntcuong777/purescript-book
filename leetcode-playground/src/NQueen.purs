module NQueens where

import Prelude

import Data.Array (elem)
import Data.Int64 (Int64, fromInt)

totalNQueens :: Int -> Int64
totalNQueens n = tryRow (n - 1) [] [] []
  where
    isColSafe :: Int -> Array Int -> Boolean
    isColSafe col occupiedCols =
      not (elem col occupiedCols)

    isDiagSafe :: Int -> Int -> Array Int -> Boolean
    isDiagSafe row col occupiedDiags =
      not (elem (row - col) occupiedDiags)

    isAntiDiagSafe :: Int -> Int -> Array Int -> Boolean
    isAntiDiagSafe row col occupiedAntiDiags =
      not (elem (row + col) occupiedAntiDiags)

    putIsValid :: Int -> Int -> Array Int -> Array Int -> Array Int -> Boolean
    putIsValid row col occupiedCols occupiedDiags occupiedAntiDiags =
      isColSafe col occupiedCols &&
      isDiagSafe row col occupiedDiags &&
      isAntiDiagSafe row col occupiedAntiDiags

    tryCol :: Int -> Int -> Array Int -> Array Int -> Array Int -> Int64
    tryCol col row occupiedCols occupiedDiags occupiedAntiDiags
      | col >= n = fromInt 0
      | putIsValid row col occupiedCols occupiedDiags occupiedAntiDiags =
          -- is either sum of ways to:
          -- 1. Put remaining queens beginning from the next row, starting from first column, or
          -- 2. Put the current queen in one of the next columns of the current row
          (tryRow (row - 1) (occupiedCols <> [col]) (occupiedDiags <> [row - col]) (occupiedAntiDiags <> [row + col])) 
          + (tryCol (col + 1) row occupiedCols occupiedDiags occupiedAntiDiags)
      | otherwise = tryCol (col + 1) row occupiedCols occupiedDiags occupiedAntiDiags

    tryRow :: Int -> Array Int -> Array Int -> Array Int -> Int64
    tryRow row occupiedCols occupiedDiags occupiedAntiDiags
      | row <= -1 = fromInt 1
      | otherwise = tryCol 0 row occupiedCols occupiedDiags occupiedAntiDiags

-- main :: Effect Unit
-- main = do
--   log "N-Queens problem solver"
--   let n = 8
--   log $ "Total number of solutions: " <> show (totalNQueens n)
--   -- let solutions = solveNQueens n
--   -- log $ "Number of solutions for " <> show n <> " queens: " <> show (length solutions)
--   -- log $ "Solutions: " <> show solutions
--   -- log $ "Total number of solutions: " <> show (totalNQueens n)
