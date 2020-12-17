module Day16 where

import Data.Maybe
import Text.Parsec
import Data.Monoid
import Data.List

type Range = (Int, Int)
type Ticket = [Int]

intP :: Parsec String () Int
intP = read <$> many1 digit

ticketP :: Parsec String () Ticket
ticketP = do
  ticket <- sepEndBy1 intP (string ",")
  return ticket

testRule :: Int -> (String, Range, Range) -> Bool
testRule tNum (label, (min1, max1), (min2, max2)) = (tNum >= min1 && tNum <= max1) || (tNum >= min2 && tNum <= max2)

ruleFolder :: (Int, [(String, Range, Range)]) -> Ticket -> (Int, [(String, Range, Range)])
ruleFolder (i, rules) ticket = (i, filter (testRule tNum) rules)
  where
    tNum = ticket !! i

deleteRule :: (String, Range, Range) -> [(Int, [(String, Range, Range)])] -> [(Int, [(String, Range, Range)])]
deleteRule _ [] = []
deleteRule rule ((i, rules):xs) = (i, delete rule rules) : deleteRule rule xs

deleteIndex :: Int -> [a] -> [a]
deleteIndex _ [] = []
deleteIndex i xs = f 0 xs
  where
    f c [] = []
    f c (x:xs)
      | c /= i = x : f (c+1) xs
      | c == i = f (c+1) xs

findCorrect :: [(Int, [(String, Range, Range)])] -> [Ticket] -> [(Int, [(String, Range, Range)])]
findCorrect [] _ = []
findCorrect ruleList ticketList = bestMatch : findCorrect (deleteRule actualMatch $ deleteIndex matchIndex ruleList) ticketList
  where
    matchedRules = map (\ruleEntry -> foldl' ruleFolder ruleEntry ticketList) ruleList
    bestMatch = minimumBy miniLengthSecond matchedRules
    actualMatch = head $ snd bestMatch
    (Just matchIndex) = findIndex ((==) (fst bestMatch) . fst) ruleList

findDepartures :: [(Int, [(String, Range, Range)])] -> [Int]
findDepartures = map fst . filter ((\(label, r1, r2) -> take 9 label == "departure") . head . snd)

miniLengthSecond :: (a, [b]) -> (a, [b]) -> Ordering
miniLengthSecond a b = compare (length $ snd a) (length $ snd b)

countError :: [(String, Range, Range)] -> Ticket -> Int
countError rules ticket = sum errors
  where
    validNum :: (String, Range, Range) -> Int -> Bool
    validNum (_, (min1, max1), (min2, max2)) i = i >= min1 && i <= max1 || i >= min2 && i <= max2
    allRules :: [(String, Range, Range)] -> Int -> Bool
    allRules rs i = getAny $ foldMap Any $ fmap ($ i) (validNum <$> rs)
    errors = filter (not . allRules rules) ticket

filterTicket :: [(String, Range, Range)] -> Ticket -> Bool
filterTicket rules ticket = isEmpty
  where
    validNum :: (String, Range, Range) -> Int -> Bool
    validNum (_, (min1, max1), (min2, max2)) i = i >= min1 && i <= max1 || i >= min2 && i <= max2
    allRules :: [(String, Range, Range)] -> Int -> Bool
    allRules rs i = getAny $ foldMap Any $ fmap ($ i) (validNum <$> rs)
    isEmpty = null $ filter (not . allRules rules) ticket

fieldP :: Parsec String () (String, Range, Range)
fieldP = do
  fieldName <- many1 (letter <|> char ' ')
  string ": "
  min1 <- intP
  char '-'
  max1 <- intP
  string " or "
  min2 <- intP
  char '-'
  max2 <- intP
  char '\n'
  return (fieldName, (min1, max1), (min2, max2))

allFieldsP :: Parsec String () [(String, Range, Range)]
allFieldsP = manyTill fieldP (try (string "\n"))

nearbyTicketsP :: Parsec String () [(Ticket)]
nearbyTicketsP = do
  string "nearby tickets:\n"
  sepEndBy1 ticketP (char '\n')

yourTicketP :: Parsec String () Ticket
yourTicketP = do
  string "your ticket:\n"
  --many1 (digit <|> char ',')
  ticket <- ticketP
  string "\n"
  return ticket

inputP :: Parsec String () Int
inputP = do
  rules <- allFieldsP
  yourTicketP
  char '\n'
  nt <- nearbyTicketsP
  return $ sum $ map (countError rules) nt

input2P :: Parsec String () Int
input2P = do
  rules <- allFieldsP
  yourticket <- yourTicketP
  char '\n'
  nt <- nearbyTicketsP
  let filteredTickets = filter (filterTicket rules) nt
  let numberedRules = zip [0..(length rules - 1)] (repeat rules)
  let correct = findCorrect numberedRules filteredTickets
  let departures = findDepartures correct
  return $ product $ map (yourticket !!) departures

  
main :: IO ()
main = do
  str <- readFile "input16.txt"
  print $ runParser fieldP () "asd" str
  print $ runParser allFieldsP () "asd" str
  print $ runParser inputP () "asd" str
  print $ runParser input2P () "asd" str
  --print $ (map $ length . snd) <$> runParser input2P () "asd" str
