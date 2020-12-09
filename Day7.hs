module Day7 where

import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Query.MaxFlow
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
import Data.List.Split
import Data.Char

replaceBags :: String -> String
replaceBags "bags" = "bag"
replaceBags "bags," = "bag,"
replaceBags x = x

findNum :: [(Int, String)] -> String -> Int
findNum xs str = fst . head $ filter ((==str) . snd) xs

parseLines :: [(Int, String)] -> [((Int, String), [(Int, Int, Int)])]
parseLines xs = map parseLine xs
  where
    numberedVertices = zip [1..] $ map (head . splitOn " contain " . snd) xs
    parseLine :: (Int, String) -> ((Int, String), [(Int, Int, Int)])
    parseLine (v, str) = ((v, label), foldMap parseEdgeString edgeStrings)
      where
        [label, rest] = splitOn " contain " str
        edgeStrings = splitOn ", " rest
        parseEdgeString :: String -> [(Int, Int, Int)]
        parseEdgeString es
          | es == "no other bag" = []
          | otherwise = [(v, findNum numberedVertices toLabel, read $ takeWhile isDigit es)]
            where 
              toLabel = drop 1 $ dropWhile isDigit es

reverseEdge :: (Int, Int, Int) -> (Int, Int, Int)
reverseEdge (from, to, weight) = (to, from, weight)

countAllBags :: Gr String Int -> ([(Int, Int)], Int, String, [(Int, Int)]) -> Int
countAllBags graph (fromnodes, node, label, []) = 1
countAllBags graph (fromnodes, node, label, tonodes) = foldr f 1 tonodes
  where
    f (weight, tonode) acc = weight * countAllBags graph (context graph tonode) + acc

main :: IO ()
main = do
  str <- readFile "input7.txt"
  let nodes = zip [1..] $ map (unwords . map replaceBags . words . \x -> take (length x - 1) x) $ lines str
  let nodesAndEdges = parseLines nodes
  let nodes = map fst nodesAndEdges
  let edges = foldMap snd nodesAndEdges
  let graph = mkGraph nodes edges :: Gr String Int
  let graphInverted = mkGraph nodes (map reverseEdge edges) :: Gr String Int

  print $ length $ reachable (findNum nodes "shiny gold bag") graphInverted
  -- Subtract 1 to exclude shiny gold bag from count
  print $ countAllBags graph (context graph (findNum nodes "shiny gold bag")) - 1
