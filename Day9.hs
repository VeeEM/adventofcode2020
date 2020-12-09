module Day9 where

import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as V

twoPointer :: V.Vector Int -> Int -> Int -> Int -> Maybe Int
twoPointer xs target p1 p2
  | p1 == p2 = Nothing
  | sumVals == target =  Just $ firstVal * secondVal
  | sumVals < target  =  twoPointer  xs target (p1 + 1) p2
  | sumVals > target  =  twoPointer  xs target p1 (p2 - 1)
  where
    firstVal = xs V.! p1
    secondVal = xs V.! p2
    sumVals = firstVal + secondVal

contiguousSum :: V.Vector Int -> Int -> Int
contiguousSum xs target = contiguousSum' 0 0 0
  where
    contiguousSum' start current sum
      | current >= V.length xs || currentSum > target = contiguousSum' (start + 1) (start + 1) 0
      | currentSum == target = V.minimum currentSlice + V.maximum currentSlice
      | otherwise = contiguousSum' start (current + 1) currentSum
      where
        currentSum = sum + (xs V.! current)
        currentSlice = V.slice start (current - start + 1) xs

firstNonMatching :: V.Vector Int -> Int -> Int -> Int
firstNonMatching xs preamble current
  | isJust twoSum = firstNonMatching xs preamble (current + 1)
  | isNothing twoSum = xs V.! current
  where
    sortedPreambles = V.modify V.sort $ V.slice (current - preamble) preamble xs
    twoSum = twoPointer sortedPreambles (xs V.! current) 0 (preamble - 1)

main :: IO ()
main = do
  str <- readFile "input9.txt"
  let values = V.fromList $ map read $ lines str :: V.Vector Int
  let preamble = 25
  let firstNon = firstNonMatching values preamble preamble
  print firstNon
  print $ contiguousSum values firstNon
