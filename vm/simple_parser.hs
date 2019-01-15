-- ref: https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Parsing
import           Control.Monad
import           Data.List.Split               (splitOn)
import           System.Environment
import           Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Function LispVal LispVal
             | Call LispVal LispVal
             | Binary Char LispVal LispVal
             | Args [String]
             | Pair (LispVal, LispVal)
             | Number Integer
             | String String
             | ID String
             | Bool Bool deriving (Show)

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
              let atom = first:rest
              return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseID :: Parser LispVal
parseID = ID <$> many1 letter

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

parseOperator :: Parser LispVal
parseOperator = try $ do
    head <- try (spaces >> parseNumber)
        <|> try parseNumber
        <|> try (spaces >> parseID)
        <|> parseID
    op <- try (spaces >> operator) <|> operator
    tail <- try (spaces >> parseExpr)
        <|> parseExpr
    return $ Binary op head tail

parseParenOperator :: Parser LispVal
parseParenOperator = try $ do
    char '('
    head <- try (spaces >> parseNumber)
        <|> try parseNumber
        <|> try (spaces >> parseID)
        <|> parseID
    op <- try (spaces >> operator) <|> operator
    tail <- try (spaces >> parseExpr)
        <|> parseExpr
    char ')'
    return $ Binary op head tail

parseFunction :: Parser LispVal
parseFunction = try $ do
    def <- try (spaces >> many1 letter) <|> many1 letter
    -- args <- try (spaces >> digit) <|> spaces >> letter
    args <- try (spaces >> many (noneOf "=")) <|> many (noneOf "=")
    char '='
    body <- try (spaces >> parseExpr) <|> parseExpr
    let splitedArgs = filter (not . null) $ splitOn " " args
    if not (null splitedArgs)
       then return $ Function (Pair (ID def, Args splitedArgs)) body
       else return $ Function (ID def) body

parseCall :: Parser LispVal
parseCall = try $ do
    def <- try (spaces >> many1 letter) <|> many1 letter
    args <- try (spaces >> parseExpr) <|> parseExpr
    return $ Call (ID def) args

parseExpr :: Parser LispVal
parseExpr = parseString
         <|> parseOperator
         <|> parseParenOperator
         <|> parseNumber
         <|> parseFunction
         <|> parseCall
         <|> parseID

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "No match: " ++ show err
    Right val -> show val

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

operator :: Parser Char
operator = oneOf "+-*/%"

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
         (expr:_) <- getArgs
         putStrLn (readExpr expr)
