{-# LANGUAGE FlexibleContexts #-}
module Day2 where

import qualified Text.Parsec as P
import System.IO

type MinCount = Int
type MaxCount = Int
type PolicyChar = Char
type Password = String

parse rule text = P.parse rule "input2.txt" text

pInt :: P.Parsec String () Int
pInt = read <$> P.many1 P.digit

pLine :: P.Parsec String () Char
pLine = P.char '-'

passwordRuleParser :: P.Parsec String () (MinCount, MaxCount, PolicyChar, Password)
passwordRuleParser = do
  min <- pInt
  P.char '-'
  max <- pInt
  P.space
  policyC <- P.letter
  P.char ':'
  P.space
  pw <- P.many1 P.letter
  P.space
  return (min, max, policyC, pw)

main :: IO ()
main = do
  fileContent <- readFile "input2.txt"
  let eitherPwRules = parse (P.many1 passwordRuleParser) fileContent
  case eitherPwRules of
    Left e -> print e
    Right pwRules -> do
      print $ length $ filter legalPassword pwRules
      print $ length $ filter legalPassword2 pwRules

legalPassword :: (MinCount, MaxCount, PolicyChar, Password) -> Bool
legalPassword (min, max, policyC, pw) = lengthFiltered >= min && lengthFiltered <= max
  where
    lengthFiltered = length $ filter (==policyC) pw

legalPassword2 :: (MinCount, MaxCount, PolicyChar, Password) -> Bool
legalPassword2 (min, max, policyC, pw) =
  firstC == policyC && lastC /= policyC ||
  firstC /= policyC && lastC == policyC
  where
    firstC = pw !! (min - 1)
    lastC = pw !! (max - 1)
