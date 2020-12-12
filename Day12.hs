module Day12 where

import Data.List

data Ship = Ship {
  direction :: Int
  ,position :: (Int, Int)
  ,waypoint :: (Int, Int)
} deriving (Show)

parseInstruction :: String -> (Ship -> Ship)
parseInstruction (ic:rest)
  | elem ic "LR" = turn ic units
  | elem ic "ESWN" = move ic units
  | ic == 'F' = forward units
  where
    units = read rest :: Int

parseInstruction2 :: String -> (Ship -> Ship)
parseInstruction2 (ic:rest)
  | elem ic "LR" = rotateWaypoint ic units
  | elem ic "ESWN" = moveWaypoint ic units
  | ic == 'F' = moveToWaypoint units
  where
    units = read rest :: Int

forward :: Int -> Ship -> Ship
forward units ship
  | d == 0 = ship { position = (x + units, y) }
  | d == 90 = ship { position = (x, y + units) }
  | d == 180 = ship { position = (x - units, y) }
  | d == 270 = ship { position = (x, y - units) }
  where
    d = direction ship
    (x, y) = position ship

turn :: Char -> Int -> Ship -> Ship
turn 'L' deg ship = ship { direction = mod (direction ship - deg) 360 }
turn 'R' deg ship = ship { direction = mod (direction ship + deg) 360 }

move :: Char -> Int -> Ship -> Ship
move d units ship
  | d == 'E' = ship { position = (cx + units, cy) }
  | d == 'S' = ship { position = (cx, cy + units) }
  | d == 'W' = ship { position = (cx - units, cy) }
  | d == 'N' = ship { position = (cx, cy - units) }
  where
    (cx, cy) = position ship

moveWaypoint :: Char -> Int -> Ship -> Ship
moveWaypoint d units ship
  | d == 'E' = ship { waypoint = (cx + units, cy) }
  | d == 'S' = ship { waypoint = (cx, cy + units) }
  | d == 'W' = ship { waypoint = (cx - units, cy) }
  | d == 'N' = ship { waypoint = (cx, cy - units) }
  where
    (cx, cy) = waypoint ship

rotateWaypoint :: Char -> Int -> Ship -> Ship
rotateWaypoint d deg ship
  | deg == 180 = ship { waypoint = ((-x), (-y)) }
  | d == 'L' && deg == 90 || d == 'R' && deg == 270 = ship { waypoint = (y, -x) }
  | d == 'R' && deg == 90 || d == 'L' && deg == 270 = ship { waypoint = (-y, x) }
  where
    (x, y) = waypoint ship

moveToWaypoint :: Int -> Ship -> Ship
moveToWaypoint times ship = ship { position = (ox + times * wpx, oy + times * wpy) }
  where
    (ox, oy) = position ship
    (wpx, wpy) = waypoint ship

initialShip = Ship 0 (0,0) (10, -1)

printResult :: Ship -> IO ()
printResult ship = print $ abs x + abs y
  where
    (x, y) = position ship

main :: IO ()
main = do
  str <- readFile "input12.txt"
  let instructions = map parseInstruction $ lines str
  let instructions2 = map parseInstruction2 $ lines str
  let firstShip = foldl' (\ship f -> f ship) initialShip instructions
  let secondShip = foldl' (\ship f -> f ship) initialShip instructions2
  printResult firstShip
  printResult secondShip
