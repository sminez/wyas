module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
-- import Control.Monad -- Needed if we are using liftM


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
  | Bool Bool deriving (Show, Eq)



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
  x <- many character
  _ <- char '"'
  return (String  (concat x))

parseChar :: Parser LispVal
parseChar = do
  _ <- char '\''
  x <- anyChar
  _ <- char '\''
  return (Char x)

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
  c <- many1 digit
  return $ Float $ fst $ (readFloat $ a++"."++c) !! 0

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

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseChar
  <|> parseString
  <|> parseNumeric


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
