{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# LANGUAGE LambdaCase #-}

module LispParser where

import qualified Text.Parsec as P
import Text.Parsec (try)
import Text.Parsec.String (Parser)
import Text.Parsec (ParseError)
import qualified Text.Parsec.Char as C
import Text.Parsec.Char (char, digit)
import Text.Parsec.Combinator (eof, many1, sepBy)
import Control.Monad (void)
import Control.Applicative ((<|>), many)
import System.Environment (getArgs)

oneOf :: [Char] -> Parser Char
oneOf = C.oneOf

noneOf :: [Char] -> Parser Char
noneOf = C.noneOf

-- from example code
parse :: Parser a -> String -> Either ParseError a
parse p = P.parse p ""

-- from example code
parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = P.parse (whitespace *> p <* eof) ""

data Token
  = Form String [Token]
  | Num Integer
  | Var String
  | Quote Token
  deriving (Show, Eq)

num :: Parser Token
num = Num <$> read <$> (lexeme $ many1 digit)

wsChars :: String
wsChars = " \n\t"

whitespace :: Parser ()
whitespace = void $ many $ oneOf wsChars

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

varName :: Parser String
varName = lexeme (many1 $ noneOf $ wsChars ++ "()")  

var :: Parser Token
var = Var <$> varName 

symbol :: Parser Token
symbol = quote <|> form <|> try num <|> var

symbolList :: Parser [Token]
symbolList = symbol `sepBy` whitespace

form :: Parser Token
form =
  do
    lexeme (char '(')
    f <- varName
    p <- symbolList
    lexeme (char ')')
    return $ Form f p

-- improperForm :: Parser Token
-- improperForm = Form <$>
--   (lexeme (char '(') *> symbolList <* lexeme (char ')'))

quote :: Parser Token
quote = Quote <$>
  (lexeme (char '\'') *> form)

statement :: Parser Token
statement = quote <|> form

statements :: Parser [Token]
statements = many1 statement

parseFile :: String -> IO ()
parseFile path =
  do
    text <- readFile path
    either print print $ parseWithEof statements text

main :: IO ()
main =
  do
    a <- getArgs
    case a of
      [str] ->
        do
          fText <- readFile str
          let pRes = parseWithEof statements fText
            in either print print pRes
      _ -> error "please provide file path"
