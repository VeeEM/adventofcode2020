module Day6 where

import Data.List
import Data.List.Split

main :: IO ()
main = do
  str <- readFile "input6.txt"
  print $ sum $ map (length . foldr1 union . words) $ splitOn "\n\n" str
  print $ sum $ map (length . foldr1 intersect . words) $ splitOn "\n\n" str
