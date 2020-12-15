module Day11 where

import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe

mapChars :: String -> Int -> Int -> [((Int, Int), Char)]
mapChars [] _ _ = []
mapChars ('\n':cs) x y = mapChars cs 0 (y + 1)
mapChars (c:cs) x y = ((x, y), c) : mapChars cs (x + 1) y

adjacentIndices :: (Int, Int) -> [(Int, Int)]
adjacentIndices (x, y) = [
  (adjacentX, adjacentY) |
  adjacentX <- [(x - 1)..(x + 1)],
  adjacentY <- [(y - 1)..(y + 1)],
  not (adjacentX == x && adjacentY == y) ]

adjacentChars :: M.Map (Int, Int) Char -> (Int, Int) -> [Char]
adjacentChars charMap (x, y) = map fromJust $ filter isJust $ map (charMap M.!?) $ adjacentIndices (x, y)

visibleAllDirections :: Int -> Int -> M.Map (Int, Int) Char -> (Int, Int) -> [Char]
visibleAllDirections width height charMap (x, y) =
  map fromJust $ filter isJust $ map (firstVisibleWithIncrement width height charMap (x, y)) increments
  where
    incRange = [(-1), 0, 1]
    increments = [(ix, iy) | ix <- incRange, iy <- incRange, not (ix == 0 && iy == 0)]

firstVisibleWithIncrement :: Int -> Int -> M.Map (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Maybe Char
firstVisibleWithIncrement width height charMap (x, y) (ix, iy)
  | curX > maxX || curY > maxY || curX < 0 || curY < 0 = Nothing
  | curChar /= '.' = Just curChar
  | otherwise = firstVisibleWithIncrement width height charMap (curX, curY) (ix, iy)
  where
    maxX = width - 1
    maxY = height - 1
    curX = x + ix
    curY = y + iy
    curChar = charMap M.! (x + ix, y + iy)

occupiedCount :: [Char] -> Int
occupiedCount = length . filter ((==) '#')

emptyCount :: [Char] -> Int
emptyCount = length . filter ((==) 'L')

transitionSeat :: (M.Map (Int, Int) Char -> (Int, Int) -> [Char]) -> Int -> M.Map (Int, Int) Char -> (Int, Int) -> Char -> Char
transitionSeat f occLimit charMap key '.' = '.'
transitionSeat f occLimit charMap key c
  | c == 'L' = if occupied == 0 then '#' else 'L'
  | c == '#' = if occupied >= occLimit then 'L' else '#'
  where
    chairs = f charMap key
    occupied = occupiedCount chairs

countTransitions :: (M.Map (Int, Int) Char -> (Int, Int) -> Char -> Char) -> M.Map (Int, Int) Char -> Int
countTransitions f charMap
  | charMap == newMap = length $ M.filter ((==) '#') charMap
  | otherwise = countTransitions f newMap
  where
    newMap = M.mapWithKey (f charMap) charMap

main :: IO ()
main = do
  str <- readFile "input11.txt"
  let width = length $ head $ lines str
  let height = length $ lines str
  let charMap = M.fromList $ mapChars str 0 0
  print $ countTransitions (transitionSeat adjacentChars 4) charMap
  print $ countTransitions (transitionSeat (visibleAllDirections width height) 5) charMap
