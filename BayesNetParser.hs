-- | made as practice
-- | designed to parse the BayesNet files
{-# OPTIONS -fno-warn-unused-do-bind #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module BayesNetParser where

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as C

import Control.Monad (void)
import Control.Applicative ((<|>), many)

import Text.Parsec (ParseError, try, manyTill, lookAhead)
import Text.Parsec.Char (digit, string)
import Text.Parsec.Combinator (eof, many1, sepBy)
import Text.Parsec.String (Parser)

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
      _ -> error "please provide file path"

whitespace :: Parser ()
whitespace = void . many $ oneOf whitespaceChars

wsDelimList :: Parser a -> Parser [a]
wsDelimList p = try p `sepBy` (many1 $ oneOf " \t") 

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

num :: Parser String
num = many digit

symbol :: Parser String
symbol = many1 $ noneOf (whitespaceChars ++ "[]")

symLexeme :: Parser String
symLexeme = lexeme $ symbol 

strLexeme :: String -> Parser String
strLexeme = lexeme . string
  
data Node =
  Node { name :: String, values :: [String] } deriving Show
type Edge = (String, String)
type Condition = (String, String)
type Probability = (String, Float)
data TableEntry =
  TableEntry { conditions :: [Condition],  probabilities :: [Probability] } deriving Show
type Table = (String, [TableEntry])

node :: Parser Node
node =
  do
    name <- symLexeme
    values <- wsDelimList symbol
    whitespace
    return $ Node { name, values }

untilDashes :: Parser a -> Parser [a]
untilDashes p = manyTill p (strLexeme "---") 

block :: String -> Parser a -> Parser [a]
block s p = strLexeme s *> untilDashes p

nodeBlock :: Parser [Node]
nodeBlock = block "Nodes" node

edge :: Parser Edge
edge = (,) <$> symLexeme <*> symLexeme

edgeBlock :: Parser [Edge] 
edgeBlock = block "Edges" edge

condition :: Parser Condition
condition = 
  do
    name <- symLexeme
    strLexeme "="
    val <- symLexeme
    return (name, val)

probFloat :: Parser Float
probFloat = read <$> lexeme prob
  where
    prob = string "1" <|> decimal
    decimal = (++) <$> string "0." <*> many1 digit

probability :: Parser Probability
probability =
  do
    name <- symLexeme
    strLexeme "="
    val <- probFloat
    return (name, val)

squareWsList :: Parser a -> Parser [a]
squareWsList p = strLexeme "[" *> manyTill p (strLexeme "]")

tableEntry :: Parser TableEntry
tableEntry =
  do
    conditions <- squareWsList condition
    probabilities <- manyTill probability (lookAhead tableEntryTerminator)
    return $ TableEntry { conditions, probabilities }
    where tableEntryTerminator = string "[" <|> string "---"

cptBlock :: Parser Table
cptBlock =
  do
    strLexeme "CPT"
    name <- symLexeme
    entries <- untilDashes $ tableEntry <* whitespace
    return (name, entries)

script :: Parser ([Node], [Edge], [Table])
script =
  do
    whitespace
    nodes <- nodeBlock
    edges <- edgeBlock
    strLexeme "TABLES"
    tables <- many1 cptBlock
    return (nodes, edges, tables)

