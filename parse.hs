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
showVal (List l)               = "(" ++ unwordsList l ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"
showVal (String s)             = "\"" ++ s ++ "\""
showVal (Atom name)            = name
showVal (Number n)             = show n
showVal (Float f)              = show f
showVal (Char c)               = show c
showVal (Bool True)            = "#t"
showVal (Bool False)           = "#f"


---------------------------------------
-- Parsers for individual components --
---------------------------------------
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

-------------------------
-- Built in operations --
-------------------------
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numBinop (+)),
              ("-", numBinop (-)),
              ("*", numBinop (*)),
              ("/", numBinop div),
              ("mod", numBinop mod),
              ("quotient", numBinop quot),
              ("remainder", numBinop rem),
              ("symbol?", isSymbol),
              ("number?", isNumber),
              ("string?", isString)]

-- Convert LispVals to their wrapped values if they are numeric
numToInteger :: LispVal -> Integer
numToInteger (Number n) = n
numToInteger _          = 0  -- fixme!

-- Perform a LISPy reduction using a binary operation
-- Extracts the Integers within Numbers and then folds using the binop
numBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numBinop op params = Number (foldl1 op (map numToInteger params))

-- Looks like you can't paramaterise a pattern match so we need individual
-- functions to match each constructor... #sadpanda
isSymbol :: [LispVal] -> LispVal
isSymbol [Atom a] = Bool True
isSymbol _        = Bool False

isString :: [LispVal] -> LispVal
isString [String s] = Bool True
isString _          = Bool False

isNumber :: [LispVal] -> LispVal
isNumber [Number n] = Bool True
isNumber _          = Bool False

------------------------------------
-- read >> eval >> apply >> print --
------------------------------------
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err  -> String ("No match: " ++ show err)
  Right val -> val

-- Adding unqoute, quasi-quote and splicing will be harder...!
eval :: LispVal -> LispVal
eval val@(String _)             = val
eval val@(Char _)               = val
eval val@(Number _)             = val
eval val@(Float _)              = val
eval val@(Bool _)               = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args))  = apply func $ map eval args

-- Look up a function in our list of known operations
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
