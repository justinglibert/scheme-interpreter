module Main where
import           Control.Monad
import           Data.Functor
import           Numeric
import           System.Environment
import           Text.Parsec             hiding ( spaces )
import           Text.Parsec.String
import           Text.Parsec.Combinator
import           Control.Monad.Except

-- Types

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | ParserE ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar     message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form   ) = message ++ ": " ++ show form
showError (NotFunction    message func   ) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (ParserE parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where
  show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Float
  | String String
  | Bool Bool

-- Parsing

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

parseNumber :: Parser LispVal
parseNumber = parseNumberDec <|> parseNumberHex

readSingleFloat :: String -> Maybe Float
readSingleFloat x = case readFloat x of
  []         -> Nothing
  [(val, _)] -> Just val

parseFloat :: Parser LispVal
parseFloat = do
  i <- many1 digit
  char '.'
  d <- many1 digit
  let val = readSingleFloat $ i ++ "." ++ d
  maybe (fail "Could not parse this float") (return . Float) val

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

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> throwError $ ParserE err
  Right val -> return val

-- Pretty Printing

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

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal

-- Evaluation


eval :: LispVal -> ThrowsError LispVal
eval val@(String _                  ) = return val
eval val@(Number _                  ) = return val
eval val@(Bool   _                  ) = return val
eval val@(Float  _                  ) = return val
eval (    List   [Atom "quote", val]) = return val
eval (    List   (Atom func : args) ) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized Form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe (throwError $ NotFunction "Unrecognized primitive function" func)
        ($ args)
    $ lookup func primitives


primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+"        , numericBinop (+))
  , ("-"        , numericBinop (-))
  , ("*"        , numericBinop (*))
  , ("/"        , numericBinop div)
  , ("mod"      , numericBinop mod)
  , ("quotient" , numericBinop quot)
  , ("remainder", numericBinop rem)
  ]

numericBinop
  :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op []            = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = Number . foldl1 op <$> mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
-- TODO hack until I figure out how to do float stuff
unpackNum (Float  n) = return $ round n
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)]
  in  if null parsed
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst . head $ parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

-- Shortcut to quickly eval a string in the Repl
s = scheme

scheme :: String -> IO ()
scheme i = do
  let evaled = show <$> (readExpr i >>= eval)
  putStrLn $ extractValue $ trapError evaled

main :: IO ()
main = do
  [a] <- getArgs
  scheme a
