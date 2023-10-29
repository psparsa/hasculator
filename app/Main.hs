module Main where

import           Control.Monad.State  (evalStateT)
import qualified Data.Map             as M
import           Expression           (Expression (..), Statement (..),
                                       printStatement)
import           Text.Parsec          (eof, oneOf, parse, (<|>))
import           Text.Parsec.Expr     (Assoc (AssocLeft),
                                       Operator (Infix, Prefix),
                                       buildExpressionParser)
import           Text.Parsec.Language (javaStyle)
import           Text.Parsec.String   (Parser)
import           Text.Parsec.Token    (GenLanguageDef (opLetter, opStart),
                                       GenTokenParser (naturalOrFloat, parens, reservedOp, whiteSpace),
                                       TokenParser, makeTokenParser)



lexer :: TokenParser ()
lexer = makeTokenParser (javaStyle  { opStart  = oneOf "+-*/%"
                                    , opLetter = oneOf "+-*/%" })

parseNumber :: Parser Expression
parseNumber = do
  parsedValue <- naturalOrFloat lexer
  case parsedValue of
    Left  l -> return (Constant (fromIntegral l))
    Right r -> return (Constant r)

parseTerm :: Parser Expression
parseTerm = parens lexer parseExpression <|> parseNumber

parseExpression :: Parser Expression
parseExpression = (flip buildExpressionParser) parseTerm $ [
    [
      Prefix (reservedOp lexer "-" >> return Negation),
      Prefix (reservedOp lexer "+" >> return id),
      Infix  (reservedOp lexer "*" >> return Multiplication)  AssocLeft,
      Infix  (reservedOp lexer "/" >> return Division)        AssocLeft,
      Infix  (reservedOp lexer "%" >> return Modulo)          AssocLeft,
      Infix  (reservedOp lexer "+" >> return Addition)        AssocLeft,
      Infix  (reservedOp lexer "-" >> return Subtraction)     AssocLeft
    ]
  ]


parseInput :: Parser Statement
parseInput = do
  whiteSpace lexer
  result <- parseExpression
  eof
  return (PrintStatement result)


calculate :: String -> IO ()
calculate "" = putStrLn ""
calculate str =
  case result of
    Left  _ -> putStrLn "Error: Invalid expression"
    Right n -> evalStateT (printStatement n) M.empty
  where
    result = parse parseInput "" str

main :: IO ()
main = do
  putStrLn "Enter your mathematical expressions:"
  getContents >>= (mapM_ calculate) . lines
