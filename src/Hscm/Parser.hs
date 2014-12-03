module Hscm.Parser (readExpr
                   , parseExpr
                   , Expression(..)) where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data  Expression = Symbol String
                | Number Integer
                | Bool Bool
                | Character Char
                | String String
                | Vector Expression
                | List [Expression]
                | DottedPair [Expression] Expression
                  deriving (Eq)

showExpression :: Expression -> String
showExpression (Symbol s) = s
showExpression (Number n) = show n
showExpression (Character c) = "#\\" ++ showCharacter c
    where showCharacter ' ' = "space"
          showCharacter '\n' = "newline"
          showCharacter c' = [c']
showExpression (String s) = "\"" ++ s ++ "\""
showExpression (Bool True) = "#t"
showExpression (Bool False) = "#f"
showExpression (Vector _) = undefined
showExpression (List exprs) = "(" ++ showListContent exprs ++ ")"
showExpression (DottedPair exprs e) = "(" ++ showListContent exprs ++ " . " ++ show e ++ show ")"

showListContent :: [Expression] -> String
showListContent = unwords . map show

instance Show Expression where
  show = showExpression

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> Expression
readExpr input = case parse parseExpr "" input of
                   Left err -> String $ "Invalid expression: " ++ show err
                   Right val -> val

parseExpr :: Parser Expression
parseExpr = parseSpaces >>
            (parseNumber
             <|> parseCharacter
             <|> parseSymbol
             <|> parseString
             <|> parseQuoted
             <|> parseList)

parseSpaces :: Parser String
parseSpaces = many space

parseCharacter :: Parser Expression
parseCharacter = do
  _ <- char '#'
  _ <- char '\\'
  c <- (try specialCharacter <|> anyChar)
  return $ Character c
  where specialCharacter = space' <|> newline'
        space' =  string "space" >> return ' '
        newline' = string "newline" >> return '\n'

parseString :: Parser Expression
parseString = do
  _ <- char '"'
  x <- many (escapedCharacter <|> noneOf "\"")
  _ <- char '"'
  return $ String x

parseSymbol :: Parser Expression
parseSymbol = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let sym = first:rest
  return $ case sym of
             "#t" -> Bool True
             "#f" -> Bool False
             _    -> Symbol sym

parseQuoted :: Parser Expression
parseQuoted = do
  _ <- char '\''
  expr <- parseExpr
  return $ List [Symbol "quote", expr]

parseList :: Parser Expression
parseList = do
  _ <- char '('
  lst <- try parseProperList <|> parseDottedList
  _ <- char ')'
  return lst

parseProperList :: Parser Expression
parseProperList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser Expression
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedPair h t

parseNumber :: Parser Expression
parseNumber = liftM Number number

spaces :: Parser ()
spaces = skipMany1 space

escapedCharacter :: Parser Char
escapedCharacter = do
  _ <- char '\\'
  c <- oneOf $ "\\\"" ++ controlCharacters
  if (c `elem` controlCharacters)
  then
      return $ getControlCharacter c
  else
      return c
    where controlCharacters = "nrt"
          getControlCharacter 'n' = '\n'
          getControlCharacter 'r' = '\r'
          getControlCharacter 't' = '\t'
          getControlCharacter _ = undefined

number :: Parser Integer
number = negativeInteger <|> integer

integer :: Parser Integer
integer = fmap read $ many1 digit

negativeInteger :: Parser Integer
negativeInteger = char '-' >> integer >>= (\n -> return $ -n)
