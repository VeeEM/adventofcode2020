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

printMap :: Int -> Int -> M.Map (Int, Int) Char -> String
printMap width height charMap = f 0 0
  where
    maxX = width - 1
    maxY = height - 1
    f x y
      | x > maxX = '\n' : f 0 (y + 1)
      | y > maxY = []
      | otherwise = charMap M.! (x, y) : f (x + 1) y

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
