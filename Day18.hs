module Day18 where

import Data.Either
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

-- https://hackage.haskell.org/package/parsec-3.1.14.0/docs/Text-Parsec-Expr.html#v:buildExpressionParser
-- emptyDef is more than enough for these AoC expressions
lexer = makeTokenParser emptyDef

expr1 = buildExpressionParser table1 term1
expr2 = buildExpressionParser table2 term2

term1 = parens lexer expr1
  <|> natural lexer

term2 = parens lexer expr2
  <|> natural lexer

table1 = [[binary "*" (*) AssocLeft, binary "+" (+) AssocLeft ]]

table2 = [
           [binary "+" (+) AssocLeft]
         , [binary "*" (*) AssocLeft]
         ]

binary  name fun assoc = Infix (do{ reservedOp lexer name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp lexer name; return fun })
postfix name fun       = Postfix (do{ reservedOp lexer name; return fun })

main :: IO ()
main = do
  str <- readFile "input18.txt"
  print $ sum $ map ((\(Right x) -> x) . parse expr1 "Part 1") $ lines str
  print $ sum $ map ((\(Right x) -> x) . parse expr2 "Part 2") $ lines str
