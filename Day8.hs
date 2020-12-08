module Day8 where

import qualified Data.Vector as V
import Data.Char

parseInstruction :: String -> (String, Int)
parseInstruction s
  | sign == '-' = (instruction, (-argumentNum))
  | sign == '+' = (instruction, argumentNum)
  where
    (instruction, rest) = break isSpace s
    argumentStr = tail rest
    sign = head argumentStr
    argumentNum = read $ tail argumentStr :: Int

runInstruction :: (String, Int) -> Int -> Int -> (Int, Int)
runInstruction ("nop", _) acc pc = (acc, pc + 1)
runInstruction ("acc", arg) acc pc = (acc + arg, pc + 1)
runInstruction ("jmp", arg) acc pc = (acc, pc + arg)

runInstructionSwitched :: (String, Int) -> Int -> Int -> (Int, Int)
runInstructionSwitched ("nop", arg) acc pc = runInstruction ("jmp", arg) acc pc
runInstructionSwitched ("jmp", arg) acc pc = runInstruction ("nop", arg) acc pc
runInstructionSwitched instruction acc pc = runInstruction instruction acc pc

runProgram :: V.Vector (String, Int) -> [Int] -> Int -> Int -> (Int, Int, [Int])
runProgram program visited acc pc
  | pc > (programLength - 1) = (acc, pc, visited)
  | elem pc visited = (acc, pc, visited)
  | otherwise = runProgram program (pc:visited) new_acc new_pc
  where
    (inctruction, _) = program V.! pc
    (new_acc, new_pc) = runInstruction (program V.! pc) acc pc
    programLength = V.length program

runProgram2 :: V.Vector (String, Int) -> [Int] -> Int -> Int -> (Int, Int, [Int])
runProgram2 program visited acc pc
  | sresult_pc > programLength - 1 = (sresult_acc, sresult_pc, sresult_visited)
  | otherwise = runProgram2 program (pc:visited) new_acc new_pc
  where
    (instruction, arg) = program V.! pc
    (new_acc, new_pc) = runInstruction (instruction, arg) acc pc
    programLength = V.length program
    (switched_acc, switched_pc) = runInstructionSwitched (instruction, arg) acc pc
    (sresult_acc, sresult_pc, sresult_visited) = runProgram program (pc:visited) switched_acc switched_pc

main :: IO ()
main = do
  str <- readFile "input8.txt"
  let program = V.fromList $ map parseInstruction $ lines str
  print $ runProgram program [] 0 0
  print $ runProgram2 program [] 0 0 
