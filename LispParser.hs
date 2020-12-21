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

data Expr
  = Form [Expr]
  | Num Integer
  | Var String
  | Quote Expr
  deriving (Show, Eq)

num :: Parser Expr
num = Num <$> read <$> (lexeme $ many1 digit)

wsChars :: String
wsChars = " \n\t"

whitespace :: Parser ()
whitespace = void $ many $ oneOf wsChars

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

var :: Parser Expr
var = Var <$> lexeme (many1 $ noneOf $ wsChars ++ "()") 

symbol :: Parser Expr
symbol = quote <|> form <|> try num <|> var

symbolList :: Parser [Expr]
symbolList = symbol `sepBy` whitespace

form :: Parser Expr
form =
  do
    lexeme (char '(')
    f <- var
    p <- symbolList
    lexeme (char ')')
    return $ Form (f:p)

improperForm :: Parser Expr
improperForm = Form <$>
  (lexeme (char '(') *> symbolList <* lexeme (char ')'))

quote :: Parser Expr
quote = Quote <$>
  (lexeme (char '\'') *> form)

statement :: Parser Expr
statement = quote <|> form

statements :: Parser [Expr]
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
