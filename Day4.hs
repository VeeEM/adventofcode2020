module Day4 where

import Data.Char
import Data.Either
import Text.Parsec
import qualified Data.Map.Strict as M

fieldP :: Parsec String () (String, String)
fieldP = do
  code <- count 3 letter
  char ':'
  value <- many1 $ noneOf " \n"
  return (code, value)

fieldsP :: Parsec String () [(String, String)]
fieldsP = sepEndBy fieldP space

allFieldsP :: Parsec String () [[(String, String)]]
allFieldsP = sepEndBy fieldsP (string "\n")

validPassport :: M.Map String String -> Bool
validPassport entry =
  M.member "byr" entry &&
  M.member "iyr" entry &&
  M.member "eyr" entry &&
  M.member "hgt" entry &&
  M.member "hcl" entry &&
  M.member "ecl" entry &&
  M.member "pid" entry

validPassport2 :: M.Map String String -> Bool
validPassport2 entry =
  numInRange (entry M.! "byr") 1920 2002 &&
  numInRange (entry M.! "iyr") 2010 2020 &&
  numInRange (entry M.! "eyr") 2020 2030 &&
  validHeight (entry M.! "hgt") &&
  validHair (entry M.! "hcl") &&
  validEye (entry M.! "ecl") &&
  validPid (entry M.! "pid")

numInRange :: String -> Int -> Int -> Bool
numInRange s min max = num >= min && num <= max
  where
    num = read s :: Int

validHeight :: String -> Bool
validHeight s =
  (unit == "cm" && height >= 150 && height <= 193) ||
  (unit == "in" && height >= 59 && height <= 76)
  where
    unit = dropWhile isDigit s
    height = read $ takeWhile isDigit s :: Int

validHair :: String -> Bool
validHair (x:xs) = x == '#' && length xs == 6 && all allowedChars xs
  where
    allowedChars = (flip elem) (['0'..'9'] ++ ['a'..'f'])

validEye :: String -> Bool
validEye s = (flip elem) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] s

validPid :: String -> Bool
validPid s = length (filter isDigit s) == 9
  
main = do
  str <- readFile "input4.txt"
  let entries = fromRight [] $ parse allFieldsP "input4" str
  let entriesMaps = map M.fromList entries
  print $ length $ filter validPassport entriesMaps
  let part1Valids = filter validPassport entriesMaps
  print $ length $ filter validPassport2 part1Valids
