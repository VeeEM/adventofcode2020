module Day1 where

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as V
import Data.Maybe
import System.IO

main :: IO ()
main = do
  fileContent <- readFile "input1.txt"
  let numbers = V.modify V.sort $ V.fromList $ map read $ lines fileContent :: V.Vector Int
  putStrLn $ show $ findTwoSum numbers
  putStrLn $ show $ findThreeSum numbers

findTwoSum :: V.Vector Int -> Int
findTwoSum xs = fromJust $ twoPointer xs 2020 0 (V.length xs - 1)

findThreeSum :: V.Vector Int -> Int
findThreeSum xs = threeSum 0
  where
    threeSum i
      | isJust twoPointerResult = fromJust twoPointerResult * currentVal
      | otherwise = threeSum (i + 1)
      where
        currentVal = xs V.! i
        twoPointerResult = twoPointer xs (2020 - currentVal) (i + 1) (V.length xs - 1)

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
