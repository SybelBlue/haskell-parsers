-- | made as practice
-- | designed to parse the BayesNet files
--{-# OPTIONS -fno-warn-unused-do-bind #-}  -- don't warn on unused parse captures
{-# LANGUAGE NamedFieldPuns #-}  -- allow Rust-like record construction

module BayesNetParser where

import Control.Monad (void)
import Control.Applicative ((<|>), many)

import Text.Parsec (ParseError, try, manyTill, lookAhead, parse)
import Text.Parsec.Char (noneOf, oneOf, digit, string)
import Text.Parsec.Combinator (eof, many1, sepEndBy)
import Text.Parsec.String (Parser)

import System.Environment (getArgs)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (whitespace *> p <* eof) ""

whitespaceChars :: [Char]
whitespaceChars = " \n\t"

parseFile :: String -> IO ()
parseFile path =
  do
    text <- readFile path
    either print print $ parse script path text

main :: IO ()
main =
  do
    a <- getArgs
    case a of
      [str] -> parseFile str
      _ -> error "please provide parse file path"

whitespace :: Parser ()
whitespace = void . many $ oneOf whitespaceChars

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace

rawSymbol :: Parser String
rawSymbol = many1 $ noneOf (whitespaceChars ++ "[]")

symbol :: Parser String
symbol = lexeme rawSymbol 

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
    name <- symbol
    values <- lexeme $ rawSymbol `sepEndBy` oneOf " \t"
    return $ Node { name, values }

nodeBlock :: Parser [Node]
nodeBlock = block "Nodes" node

edge :: Parser Edge
edge = (,) <$> symbol <*> symbol

edgeBlock :: Parser [Edge] 
edgeBlock = block "Edges" edge

condition :: Parser Condition
condition = (,) <$> symbol <*> (strLexeme "=" *> symbol)

probFloat :: Parser Float
probFloat = read <$> lexeme prob
  where
    prob = string "1" <|> try decimal <|> string "0"
    decimal = (++) <$> string "0." <*> many1 digit

probability :: Parser Probability
probability = (,) <$> (symbol <* strLexeme "=") <*> probFloat

squareList :: Parser a -> Parser [a]
squareList p = strLexeme "[" *> manyTill p (strLexeme "]")

tableEntry :: Parser TableEntry
tableEntry =
  do
    conditions <- squareList condition
    probabilities <- manyTill probability $ lookAhead tableEntryTerminator
    return $ TableEntry { conditions, probabilities }
    where tableEntryTerminator = string "[" <|> string "---" <|> (eof *> return [])

cptBlock :: Parser Table
cptBlock = (,) <$> (strLexeme "CPT" *> symbol) <*> untilDashes tableEntry

cpts :: Parser [Table]
cpts = strLexeme "TABLES" *> many cptBlock

script :: Parser ([Node], [Edge], [Table])
script = whitespace *> ((,,) <$> nodeBlock <*> edgeBlock <*> cpts)
