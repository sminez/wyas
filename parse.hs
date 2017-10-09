module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import Control.Monad -- liftM


{- Data Types -}
-- This is an algebraic datatype (product type) with each constructor separated by `|`
-- Theme `deriving` clause is the same idea as Rust's derive: auto implement the typeclasses
data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Double
  | String String
  | Char Char
  | Bool Bool

instance Show LispVal where show = showVal

-- helper for showing LispVals
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- String format our LispVals
showVal :: LispVal -> String
showVal (List l) = "(" ++ unwordsList l ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"
showVal (String s) = "\"" ++ s ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Float f) = show f
showVal (Char c) = show c
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"


{- Parsers for individual components -}
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escaped :: Parser String
escaped = do
  a <- char '\\'
  b <- oneOf "\\\"0nrtbf"
  return [a,b]

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\t\b\f"

character :: Parser String
character = fmap return nonEscape <|> escaped

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  a <- many character
  _ <- char '"'
  return (String  (concat a))

parseChar :: Parser LispVal
parseChar = do
  _ <- char '\''
  a <- anyChar
  _ <- char '\''
  return (Char a)

parseAtom :: Parser LispVal
parseAtom = do
  -- <|> :: f a -> f a -> f a
  -- ========================
  -- The <|> operator is pronounced "choice" or "parallel" depending on the
  -- particular Applicative instance in question. (For Parser it is choice)
  -- It lets us try the each one in turn and use the first one that matches
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom


-- In Scheme we are allowed octal and hex numbers as well.
parseNumber :: Parser LispVal
parseNumber = many1 digit >>= \x -> return $ Number (read x)

parseFloat :: Parser LispVal
parseFloat = do
  a <- many1 digit
  _ <- char '.'
  b <- many1 digit
  return $ Float $ fst $ (readFloat $ a ++ "." ++ b) !! 0

 -- Need `try` here to prevent parseFloat from consuming input if it fails
 -- TODO :: add hex/oct, fractional & complex
parseNumeric :: Parser LispVal
parseNumeric = try parseFloat <|> parseNumber


{- ALTERNATIVE IMPLEMENTATIONS FOR parseNumber
Here, liftM (from Control.Monad) allows us to operate on the value _inside_
of the monadic Parser value
In total we: read as many `digit` elements as we can find.
`read` the string as an Integer that we then pass to our Number constructor
> parseNumber = liftM (Number . read) $ many1 digit


Here, we use modadic `do` notation to sugar the binds and lambda from above.
> parseNumber = do
>   x <- many1 digit
>   return $ Number (read x)
-}

-- Parse lists of elements as s-expressions
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- parse dotted lists (improper lists)
parseDotted :: Parser LispVal
parseDotted = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail


-- Helper for making parsers prefixed expressions
prefixParser :: String -> String -> Parser LispVal
prefixParser prefix tag = do
  _ <- string prefix
  a <- parseExpr
  return $ List [Atom tag, a]

parseQuoted :: Parser LispVal
parseQuoted = prefixParser "'" "quote"

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = prefixParser "`" "quasiquote"

parseUnQuote :: Parser LispVal
parseUnQuote = prefixParser "," "unquote"

parseUnQuoteSplicing :: Parser LispVal
parseUnQuoteSplicing = prefixParser ",@" "unquote-splicing"


-- Compound parser for parsing any valid expression
parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> try parseChar -- need try to avoid clobbering quoting later
  <|> parseString
  <|> parseNumeric
  <|> parseQuoted
  <|> parseQuasiQuoted
  <|> try parseUnQuoteSplicing <|> parseUnQuote
  <|> do char '('
         l <- try parseList <|> parseDotted
         char ')'
         return l


{- Reader Function using the parser and the main program -}
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> show val


main :: IO ()
main = do
  -- This pattern matches agains the list from getArgs and pulls off the head
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
