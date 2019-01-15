module SimpleParser (
    readExpr
    ) where

-- ref: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing
import           Control.Monad
import           Data.Char
import           Data.List.Split               (splitOn)
import           Eval
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

-- data LispVal = Atom String
--              | List [LispVal]
--              | DottedList [LispVal] LispVal
--              | Function LispVal LispVal
--              | Call LispVal LispVal
--              | Binary Char LispVal LispVal
--              | Args [String]
--              | Pair (LispVal, LispVal)
--              | Number Integer
--              | String String
--              | ID String
--              | Add LispVal LispVal
--              | Minus LispVal LispVal
--              | Multi LispVal LispVal
--              | Mod LispVal LispVal
--              | Div LispVal LispVal
--              | Bool Bool
--              | Err String deriving (Show)

parseString :: Parser Value
parseString = do
                char '"'
                x <- many (noneOf "\"")
                char '"'
                return $ String x

-- parseAtom :: Parser Value
-- parseAtom = do
--               first <- letter <|> symbol
--               rest <- many (letter <|> digit <|> symbol)
--               let atom = first:rest
--               return $ case atom of
--                          "#t" -> Bool True
--                          "#f" -> Bool False
--                          _    -> Atom atom

parseNumber :: Parser Value
parseNumber = Number . read <$> many1 digit

parseID :: Parser Value
parseID = ID <$> many1 letter

-- parseList :: Parser Value
-- parseList = List <$> sepBy parseExpr spaces
--
-- parseDottedList :: Parser Value
-- parseDottedList = do
--     head <- endBy parseExpr spaces
--     tail <- char '.' >> spaces >> parseExpr
--     return $ DottedList head tail

-- parseQuoted :: Parser Value
-- parseQuoted = do
--     char '\''
--     x <- parseExpr
--     return $ List [Atom "quote", x]

parseOperator :: Parser Value
parseOperator = try $ do
    head <- try (spaces >> parseNumber)
        <|> try parseNumber
        <|> try (spaces >> parseCall)
        <|> parseCall
        <|> try (spaces >> parseID)
        <|> parseID
    op <- try (spaces >> operator) <|> operator
    tail <- try (spaces >> parseExpr)
        <|> parseExpr
    if op == '+'
       then return $ Add head tail
       else if op == '-'
       then return $ Minus head tail
       else if op == '*'
       then return $ Multi head tail
       else if op == '/'
       then return $ Div head tail
       else return $ Mod head tail

parseParenOperator :: Parser Value
parseParenOperator = try $ do
    char '('
    head <- try (spaces >> parseNumber)
        <|> try parseNumber
        <|> try (spaces >> parseCall)
        <|> parseCall
        <|> try (spaces >> parseID)
        <|> parseID
    op <- try (spaces >> operator) <|> operator
    tail <- try (spaces >> parseExpr)
        <|> parseExpr
    char ')'
    if op == '+'
       then return $ Add head tail
       else if op == '-'
       then return $ Minus head tail
       else if op == '*'
       then return $ Multi head tail
       else if op == '/'
       then return $ Div head tail
       else return $ Mod head tail

parseFunction :: Parser Value
parseFunction = try $ do
    def <- try (spaces >> many1 letter) <|> many1 letter
    -- args <- try (spaces >> digit) <|> spaces >> letter
    args <- try (spaces >> many (noneOf "=")) <|> many (noneOf "=")
    char '='
    body <- try (spaces >> parseExpr) <|> parseExpr
    let splitedArgs = map (\x -> if all isDigit x then Number (read x :: Integer) else ID x) $ filter (not . null) $ splitOn " " args
    if not (null splitedArgs)
       then return $ Function (Pair (ID def, Args splitedArgs)) body
       else return $ Function (ID def) body

parseCall :: Parser Value
parseCall = try $ do
    def <- try (spaces >> many1 letter) <|> many1 letter
    args <- try (spaces >> parseExpr) <|> parseExpr
    return $ Call (ID def) args

parseExpr :: Parser Value
parseExpr = parseString
         <|> parseOperator
         <|> parseParenOperator
         <|> parseNumber
         <|> parseFunction
         <|> parseCall
         <|> parseID

readExpr :: String -> Value
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> Err ("No match: " ++ show err)
    Right val -> val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

operator :: Parser Char
operator = oneOf "+-*/%"

spaces :: Parser ()
spaces = skipMany1 space

-- main :: IO ()
-- main = do
--          expr <- getContents
--          print (map readExpr $ filter (not . null) $ splitOn "\n" expr)
