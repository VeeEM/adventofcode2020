module Day14 where

import Data.Char
import Data.Word
import Data.Bits
import qualified Data.Map.Strict as M
import Data.Bool

bitSet = flip (setBit) :: Int -> Word64 -> Word64
bitClear = flip (clearBit) :: Int -> Word64 -> Word64

createMask :: String -> (Word64 -> Word64)
createMask str = createMask' id (reverse str) 0
  where
    createMask' f [] _ = f
    createMask' f (x:xs) counter
      | x == '1' = createMask' (bitSet counter . f) xs (counter + 1)
      | x == '0' = createMask' (bitClear counter . f) xs (counter + 1)
      | otherwise = createMask' f xs (counter + 1)

parseMask :: String -> (Word64 -> Word64)
parseMask str = createMask $ maskStr str

maskStr :: String -> String
maskStr = drop 2 . dropWhile (/= '=')

parseValue :: String -> (Word64, Word64)
parseValue str = (read $ takeWhile isDigit stop1, read $ takeWhile isDigit stop2)
  where
    stop1 = dropWhile (not . isDigit) str
    stop2 = dropWhile (not . isDigit) $ dropWhile isDigit stop1

word36ToString :: Word64 -> String
word36ToString w = reverse $ map (bool '0' '1' . testBit w) [0..35]

maskRunner :: [String] -> M.Map Word64 Word64
maskRunner ls = maskRunner' ls id M.empty
  where
    maskRunner' :: [String] -> (Word64 -> Word64) -> M.Map Word64 Word64 -> M.Map Word64 Word64
    maskRunner' [] _ memory = memory
    maskRunner' (l:ls) mask memory
      | take 4 l == "mask" = maskRunner' ls (parseMask l) memory
      | take 3 l == "mem" = maskRunner' ls mask (M.insert address maskedValue memory)
      where
        (address, value) = parseValue l
        maskedValue = mask value

maskRunner2 :: [String] -> M.Map Word64 Word64
maskRunner2 ls = maskRunner2' ls "" M.empty
  where
    maskRunner2' :: [String] -> String -> M.Map Word64 Word64 -> M.Map Word64 Word64
    maskRunner2' [] _ memory = memory
    maskRunner2' (l:ls) mask memory
      | take 4 l == "mask" = maskRunner2' ls (maskStr l) memory
      | take 3 l == "mem" = maskRunner2' ls mask (M.union newMap memory)
      where
        (address, value) = parseValue l
        ma = maskAddress mask (word36ToString address)
        addresses = unfloatAddress2 ma
        newMap = M.fromList $ zip addresses (repeat value)

main :: IO ()
main = do
  str <- readFile "input14.txt"
  print $ sum $ maskRunner $ lines str
  print $ sum $ maskRunner2 $ lines str

maskAddress :: String -> String -> String
maskAddress [] _ = []
maskAddress _ [] = []
maskAddress (m:ms) (c:cs)
  | m == '0' = c : maskAddress ms cs
  | m == '1' || m == 'X' = m : maskAddress ms cs

unfloatAddress2 :: String -> [Word64]
unfloatAddress2 str = f (reverse str) [0] 0
  where
    f [] acc counter = acc
    f (x:xs) acc counter
      | x == '1' = f xs (map (bitSet counter) acc) (counter + 1)
      | x == '0' = f xs (map (bitClear counter) acc) (counter + 1)
      | x == 'X' = f xs (map (bitSet counter) acc ++ map (bitClear counter) acc) (counter + 1)
