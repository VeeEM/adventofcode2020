module Day13 where

import Data.List
import Data.List.Split
import Data.Char
import Data.Monoid
import qualified Data.Map.Strict as M

modAndKeep :: Int -> Int -> (Int, Int)
modAndKeep time bus
  | busMinusMod == bus = (0, bus)
  | otherwise = (busMinusMod, bus)
  where
    busMinusMod = bus - mod time bus

printResult1 :: M.Map Int Int -> IO ()
printResult1 busMap = do
  let (timeLeft, bus) = M.findMin busMap
  print $ timeLeft * bus

readSnd :: (Int, String) -> (Int, Int)
readSnd (a, str) = (a, read str)

findNum3 :: [(Int, Int)] -> Int
findNum3 nums = mod (sum prods) bigN
  where
    smallNs = map (div bigN) modus
    modus = map snd nums
    remainders = map fst nums
    bigN = product $ modus
    inverses = zipWith inverseMod smallNs modus
    prods = zipWith (*) (zipWith (*) remainders smallNs) inverses

ingredients :: [(Int, Int)] -> [(Int, Int)]
ingredients xs = map f xs
  where
    f (a, b) = (mod (b-a) b, b)

inverseMod :: Int -> Int -> Int
inverseMod n m = f 1
  where
    nm = mod n m
    f x
      | mod (nm * x) m == 1 = x
      | otherwise = f (x + 1)

main :: IO ()
main = do
  str <- readFile "input13.txt"
  let [timeLn, busLn] = lines str
  let time = read timeLn :: Int
  let busses = map read $ filter (getAll . foldMap All . map isDigit) $ splitOn "," busLn :: [Int]
  let busses2 = map readSnd $ filter (getAll . foldMap All . map isDigit . snd) $ zip [0..] $ splitOn "," busLn
  printResult1 $ M.fromList $ map (modAndKeep time) busses
  print $ findNum3 (ingredients busses2)
