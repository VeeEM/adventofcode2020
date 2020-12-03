module Day3 where

import qualified Data.ByteString.Char8 as C8
import Data.Monoid

indexSlope :: C8.ByteString -> Int -> Int -> Char
indexSlope bs x y = C8.index bs (actualY + mod x (width - 1))
  where
    width = 1 + (C8.length $ head $ C8.lines bs)
    actualY = width * y

countTrees :: C8.ByteString -> Int -> Int -> Int
countTrees bs right down = countTrees' right down 0
  where
    width = 1 + (C8.length $ head $ C8.lines bs)
    lastRow = div (C8.length bs) width - 1
    countTrees' x y treeCount
      | y > lastRow = treeCount
      | indexSlope bs x y == '#' = countTrees' (x + right) (y + down) (treeCount + 1)
      | indexSlope bs x y /= '#' = countTrees' (x + right) (y + down) treeCount

main = do
  fileContent <- C8.readFile "input3.txt"
  print $ countTrees fileContent 3 1
  let rights = [1, 3, 5, 7, 1]
  let downs = [1, 1, 1, 1, 2]
  print $ getProduct $ foldMap Product $ zipWith (countTrees fileContent) rights downs
