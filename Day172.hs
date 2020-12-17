module Day172 where

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List

type Point = (Int, Int, Int, Int)

adjacents (x, y, z, k) = [(a, b, c, d) | a <- [x-1 .. x+1], b <- [y-1 .. y+1], c <- [z-1 .. z+1], d <- [k-1 .. k+1], not $ a == x && b == y && c == z && d == k]

lookupAdjacents :: Point -> M.Map Point Char -> [Char]
lookupAdjacents p m = map fromJust $ filter isJust $ map (m M.!?) (adjacents p)

toggleBox :: M.Map Point Char -> Point -> M.Map Point Char
toggleBox m p
  | isNothing currentVal || currentVal == (Just '.') = if adjActive == 3 then M.insert p '#' m else m
  | currentVal == (Just '#') = if adjActive == 2 || adjActive == 3 then m else M.insert p '.' m
  where
    adjActive = length $ filter (== '#') $ lookupAdjacents p m
    currentVal = m M.!? p

queueUpdates :: M.Map Point Char -> [(Point, Char)] -> Point -> [(Point, Char)]
queueUpdates m updates p
  | isNothing currentVal || currentVal == (Just '.') = if adjActive == 3 then (p, '#'):updates else updates
  | currentVal == (Just '#') = if adjActive == 2 || adjActive == 3 then updates else (p, '.'):updates
  where
    adjActive = length $ filter (== '#') $ lookupAdjacents p m
    currentVal = m M.!? p

updateN :: Int -> M.Map Point Char -> M.Map Point Char
updateN 0 m = m
updateN n m = updateN (n-1) $ updateMap m

updateMap :: M.Map Point Char -> M.Map Point Char
updateMap charMap = foldl' (\m (key, value) -> M.insert key value m) charMap updates
  where
    updates = foldl' (queueUpdates charMap) [] boxes
    boxes = nub $ foldMap adjacents $ M.keys charMap

inputToMap :: String -> M.Map Point Char
inputToMap str = f 0 0 str M.empty
  where
    f :: Int -> Int -> String -> M.Map Point Char ->  M.Map Point Char
    f _ _ [] boxMap = boxMap
    f x y (c:cs) boxMap
      | c == '\n' = f 0 (y+1) cs boxMap
      | otherwise = f (x+1) y cs (M.insert (x, y, 0, 0) c boxMap)

main :: IO ()
main = do
  str <- readFile "input17.txt"
  let charMap = inputToMap str
  let adjacentBoxes = lookupAdjacents (0,0,0,0) charMap
  print $ length $ M.filter (== '#') $ updateN 6 charMap
