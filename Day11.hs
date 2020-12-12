module Day11 where

import qualified Data.Map.Strict as M
import Data.List
import Data.Maybe

mapChars :: String -> Int -> Int -> [((Int, Int), Char)]
mapChars [] _ _ = []
mapChars ('\n':cs) x y = mapChars cs 0 (y + 1)
mapChars (c:cs) x y = ((x, y), c) : mapChars cs (x + 1) y

adjacentIndices :: (Int, Int) -> [(Int, Int)]
adjacentIndices (x, y) = [(adjacentX, adjacentY) | adjacentX <- [(x - 1)..(x + 1)], adjacentY <- [(y - 1)..(y + 1)], not (adjacentX == x && adjacentY == y)]

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

transitionSeat :: M.Map (Int, Int) Char -> (Int, Int) -> Char -> Char
transitionSeat charMap key '.' = '.'
transitionSeat charMap key c
  | c == 'L' = if adjacentOccupied == 0 then '#' else 'L'
  | c == '#' = if adjacentOccupied >= 4 then 'L' else '#'
  where
    adjacentChairs = adjacentChars charMap key
    adjacentEmpty = emptyCount adjacentChairs
    adjacentOccupied = occupiedCount adjacentChairs

transitionSeat2 :: Int -> Int -> M.Map (Int, Int) Char -> (Int, Int) -> Char -> Char
transitionSeat2 width height charMap key '.' = '.'
transitionSeat2 width height charMap key c
  | c == 'L' = if adjacentOccupied == 0 then '#' else 'L'
  | c == '#' = if adjacentOccupied >= 5 then 'L' else '#'
  where
    visibleChairs = visibleAllDirections width height charMap key
    adjacentEmpty = emptyCount visibleChairs
    adjacentOccupied = occupiedCount visibleChairs


printMap :: Int -> Int -> M.Map (Int, Int) Char -> String
printMap width height charMap = f 0 0
  where
    maxX = width - 1
    maxY = height - 1
    f x y
      | x > maxX = '\n' : f 0 (y + 1)
      | y > maxY = []
      | otherwise = charMap M.! (x, y) : f (x + 1) y

countTransitions2 :: Int -> Int -> M.Map (Int, Int) Char -> Int
countTransitions2 width height charMap
  | charMap == newMap = length $ M.keys $ M.filter ((==) '#') charMap
  | otherwise = countTransitions2 width height newMap
  where
    newMap = M.mapWithKey (transitionSeat2 width height charMap) charMap

countTransitions :: M.Map (Int, Int) Char -> Int -> Int
countTransitions charMap counter
  | charMap == newMap = length $ M.keys $ M.filter ((==) '#') charMap
  | otherwise = countTransitions newMap (counter + 1)
  where
    newMap = M.mapWithKey (transitionSeat charMap) charMap

main :: IO ()
main = do
  str <- readFile "input11.txt"
  let width = length $ head $ lines str
  let height = length $ lines str
  let charMap = M.fromList $ mapChars str 0 0
  print (width, height)
  putStrLn $ printMap width height charMap
  print $ adjacentChars charMap (0, 0)
  let transitionedMap = M.mapWithKey (transitionSeat charMap) charMap
  putStrLn $ printMap width height transitionedMap
  print $ emptyCount $ adjacentChars charMap (0,0)
  print $ countTransitions charMap 0
  print $ visibleAllDirections width height charMap (3, 4)
  print $ countTransitions2 width height charMap
