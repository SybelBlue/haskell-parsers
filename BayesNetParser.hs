-- | made as practice
-- | designed to parse the BayesNet files
{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module BayesNetParser where

import qualified Text.Parsec as P

import Control.Monad (void)
import Control.Applicative ((<|>), many)

import Text.Parsec (ParseError, try, manyTill, lookAhead)
import Text.Parsec.Char (noneOf, oneOf, digit, string)
import Text.Parsec.Combinator (eof, many1)
import Text.Parsec.String (Parser)

import System.Environment (getArgs)

-- from example code
parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = P.parse (whitespace *> p <* eof) ""

whitespaceChars :: [Char]
whitespaceChars = " \n\t"

parseFile :: String -> IO ()
parseFile path =
  do
    text <- readFile path
    either print print $ parseWithEof script text

main :: IO ()
main =
  do
    a <- getArgs
    case a of
      [str] ->
        do
          fText <- readFile str
          let pRes = parseWithEof script fText
            in either print print pRes
      _ -> error "please provide parse file path"

whitespace :: Parser ()
whitespace = void . many $ oneOf whitespaceChars

eol :: Parser String
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

num :: Parser String
num = many digit

symLexeme :: Parser String
symLexeme = lexeme $ many1 $ noneOf (whitespaceChars ++ "[]")

strLexeme :: String -> Parser ()
strLexeme = void . lexeme . string
  
data Node =
  Node { name :: String, values :: [String] } deriving Show
type Edge = (String, String)
type Condition = (String, String)
type Probability = (String, Float)
data TableEntry =
  TableEntry { conditions :: [Condition],  probabilities :: [Probability] } deriving Show
type Table = (String, [TableEntry])

untilDashes :: Parser a -> Parser [a]
untilDashes p = manyTill p $ strLexeme "---" 

block :: String -> Parser a -> Parser [a]
block s p = strLexeme s *> untilDashes p

node :: Parser Node
node =
  do
    name <- symLexeme
    values <- manyTill symLexeme eol 
    return $ Node { name, values }

nodeBlock :: Parser [Node]
nodeBlock = block "Nodes" node

edge :: Parser Edge
edge = (,) <$> symLexeme <*> symLexeme

edgeBlock :: Parser [Edge] 
edgeBlock = block "Edges" edge

condition :: Parser Condition
condition = (,) <$> symLexeme <*> (strLexeme "=" *> symLexeme)

probFloat :: Parser Float
probFloat = read <$> lexeme prob
  where
    prob = string "1" <|> try decimal <|> string "0"
    decimal = (++) <$> string "0." <*> many1 digit

probability :: Parser Probability
probability = (,) <$> (symLexeme <* strLexeme "=") <*> probFloat

squareList :: Parser a -> Parser [a]
squareList p = strLexeme "[" *> manyTill p (strLexeme "]")

tableEntry :: Parser TableEntry
tableEntry =
  do
    conditions <- squareList condition
    probabilities <- manyTill probability $ lookAhead tableEntryTerminator
    return $ TableEntry { conditions, probabilities }
    where tableEntryTerminator = string "[" <|> string "---"

cptBlock :: Parser Table
cptBlock = (,) <$> (strLexeme "CPT" *> symLexeme) <*> untilDashes tableEntry

cpts :: Parser [Table]
cpts = strLexeme "TABLES" *> many cptBlock

script :: Parser ([Node], [Edge], [Table])
script = whitespace *> ((,,) <$> nodeBlock <*> edgeBlock <*> cpts)

