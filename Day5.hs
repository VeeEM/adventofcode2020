import Data.Foldable
import Data.List

startRange :: (Int, Int, Int, Int)
startRange = (0, 127, 0, 7)

findSeat :: (Int, Int, Int, Int) -> Char -> (Int, Int, Int, Int)
findSeat (low, high, lowSeat, highSeat) c
  | c == 'F' = (low, high - div rowSpace 2, lowSeat, highSeat)
  | c == 'B' = (low + div rowSpace 2, high, lowSeat, highSeat)
  | c == 'L' = (low, high, lowSeat, highSeat - div seatSpace 2)
  | c == 'R' = (low, high, lowSeat + div seatSpace 2, highSeat)
  where
    rowSpace = high - low + 1
    seatSpace = highSeat - lowSeat + 1

seatId :: (Int, Int, Int, Int) -> Int
seatId (row, _, column, _) = row * 8 + column

main :: IO ()
main = do
  passes <- lines <$> readFile "input5.txt"
  let seatIds = sort $ map (seatId . foldl' findSeat startRange) passes
  let minId = head seatIds
  let maxId = maximum seatIds
  print $ maxId
  print $ sum [minId .. maxId] - sum seatIds
