module Main where

import Control.Monad
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Float
  | String String
  | Bool Bool
  deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (noneOf "\"")
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumberDec :: Parser LispVal
parseNumberDec = liftM (Number . read) $ many1 digit

readSingleHex :: String -> Integer
readSingleHex = fst . head . readHex

parseNumberHex :: Parser LispVal
parseNumberHex = do
  string "#x"
  n <- many (letter <|> digit)
  let out = Number . readSingleHex $ n
  return out

-- Ex 1 1
-- parseNumber' :: Parser LispVal
-- parseNumber' = do
--   n <- many1 digit
--   let out = Number . read $ n
--   return out
--
-- parseNumber'' :: Parser LispVal
-- parseNumber'' = many1 digit >>= prep
--   where
--     prep x = pure $ (Number . read) x
-- / Ex 1 1

parseNumber :: Parser LispVal
parseNumber =
  parseNumberDec
    <|> parseNumberHex

readSingleFloat :: String -> Float
readSingleFloat x = case readFloat x of
  [] -> 42
  [(val, _)] -> val

parseFloat :: Parser LispVal
parseFloat = do
  i <- many1 digit
  char '.'
  d <- many1 digit
  let out = Float . readSingleFloat $ i ++ "." ++ d
  return out

parseNumberOrFloat :: Parser LispVal
parseNumberOrFloat = try parseFloat <|> parseNumber

-- Recursive Parsers

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseNumberOrFloat
    <|> parseString
    <|> parseAtom
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value" ++ show val

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)
