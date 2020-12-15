module Day15 where

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List.Split

game :: M.Map Int Int -> Int -> Int -> Int -> Int
game numbers last turn target
  | turn == target + 1 = last
  | isJust turnLast = game (M.insert last (turn - 1) numbers) (turn - 1 - fromJust turnLast) (turn + 1) target
  | isNothing turnLast = game (M.insert last (turn - 1) numbers) 0 (turn + 1) target
  where
    turnLast = numbers M.!? last

main :: IO ()
main = do
  str <- readFile "input15.txt"
  let numbersList = map read $ splitOn "," str :: [Int]
  let numbersMap = M.fromList $ flip zip [1..] $ init numbersList :: M.Map Int Int
  print $ game numbersMap (numbersList !! (length numbersList - 1)) (length numbersList + 1) 2020
  print $ game numbersMap (numbersList !! (length numbersList - 1)) (length numbersList + 1) 30000000
