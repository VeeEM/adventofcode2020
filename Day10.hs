module Day10 where

import Data.List

main :: IO ()
main = do
  str <- readFile "test10.txt"
  let numbers = sort $ map read $ lines str :: [Int]
  let results1 = drop 1 $ map length $ group $ sort $ map snd $ foldl' (\((p, pr):ps) x -> (x, x-p):(p, pr):ps) [(0, 0)] numbers
  print results1
  print $ length numbers
  print $ results1 !! 0 * (results1 !! (length results1 - 1) + 1)
