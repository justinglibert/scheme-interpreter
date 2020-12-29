module Main where

import           Control.Monad
import           Numeric
import           System.Environment
import           Text.Parsec             hiding ( spaces )
import           Text.Parsec.String
import           Text.Parsec.Combinator

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Float
  | String String
  | Bool Bool

instance Show LispVal where
  show = showVal

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
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumberDec :: Parser LispVal
parseNumberDec = Number . read <$> many1 digit

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
parseNumber = parseNumberDec <|> parseNumberHex

readSingleFloat :: String -> Float
readSingleFloat x = case readFloat x of
  []         -> error "Error while parsing float"
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
parseList = List <$> parseExpr `sepBy` spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- parseExpr `endBy` spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
  parseString <|> parseAtom <|> parseNumberOrFloat <|> parseQuoted <|> do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> String $ "No match: " ++ show err
  Right val -> val

-- Evaluation

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom   name    ) = name
showVal (Number contents) = show contents
showVal (Float  contents) = show contents
showVal (Bool   True    ) = "#t"
showVal (Bool   False   ) = "#f"
showVal (List   contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

eval :: LispVal -> LispVal
eval val@(String _                  ) = val
eval val@(Number _                  ) = val
eval val@(Bool   _                  ) = val
eval val@(Float  _                  ) = val
eval (    List   [Atom "quote", val]) = val
eval (    List   (Atom func : args) ) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+"        , numericBinop (+))
  , ("-"        , numericBinop (-))
  , ("*"        , numericBinop (*))
  , ("/"        , numericBinop div)
  , ("mod"      , numericBinop mod)
  , ("quotient" , numericBinop quot)
  , ("remainder", numericBinop rem)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
-- TODO hack until I figure out how to do float stuff
unpackNum (Float  n) = round n
unpackNum (Number n) = n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)]
  in  if null parsed then 0 else fst . head $ parsed
unpackNum (List [n]) = unpackNum n
unpackNum _          = 0

main :: IO ()
main = getArgs >>= print . evalExpr . head

evalExpr :: String -> LispVal
evalExpr = eval . readExpr
