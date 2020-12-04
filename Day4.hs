module Day4 where

import Text.Parsec



birthP :: Parsec String () Int
birthP = do
  string "byr:"
  byr <- read <$> many1 digit
  return byr

issueP :: Parsec String () Int
issueP = do
  string "iyr:"
  iyr <- read <$> many1 digit
  return iyr

expirationP :: Parsec String () Int
expirationP = do
  string "eyr:"
  eyr <- read <$> many1 digit
  return eyr

heightP :: Parsec String () String
heightP = do
  string "hgt:"
  hgtNum <- many1 digit
  hgtStr <- many1 letter
  return $ hgtNum ++ hgtStr

hairP :: Parsec String () String
hairP = do
  string "hcl:"
  char '#'
  hcl <- many1 (letter <|> digit)
  return $ '#' : hcl

eyeP :: Parsec String () String
eyeP = do
  string "ecl:"
  ecl <- many1 letter
  return $ ecl

passportP :: Parsec String () String
passportP = do
  string "pid:"
  pid <- many1 digit
  return pid

  
countryP :: Parsec String () String
countryP = do
  string "cid:"
  cid <- many1 digit
  return cid

fieldP :: Parsec String () (String, String)
fieldP = do
  code <- count 3 letter
  char ':'
  value <- many1 $ noneOf " \n"
  return (code, value)

fieldsP :: Parsec String () [(String, String)]
fieldsP = sepBy fieldP space

allFieldsP :: Parsec String () [[(String, String)]]
allFieldsP = sepBy fieldsP (string "\n\n")

main = do
  str <- readFile "input4.txt"
  print $ parse fieldsP "input4" str
  print $ parse allFieldsP "input4" str
