{-# OPTIONS -fno-warn-unused-do-bind #-}  -- don't warn on unused parse captures
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module DataDeskParser where

import Data.List (find)
import Data.Tuple (fst, snd)
import Data.Maybe (fromJust)

import Control.Monad (void)
-- import Control.Applicative ((<|>), many)

import Text.Parsec 
-- import Text.Parsec.Char 
import Text.Parsec.Combinator (eof, sepEndBy)
import Text.Parsec.String (Parser)

import System.Environment (getArgs)

failNothing :: Monad m => String -> Maybe a -> m a
failNothing msg = maybe (fail msg) return

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (spaces *> p <* eof) ""

whitespaceChars :: [Char]
whitespaceChars = " \n\t"

-- parseFile :: String -> IO ()
-- parseFile path =
--   do
--     text <- readFile path
--     either print print $ parse script path text

-- main :: IO ()
-- main =
--   do
--     a <- getArgs
--     case a of
--       [str] -> parseFile str
--       _ -> error "please provide parse file path"

notChar :: Char -> Parser Char
notChar c = satisfy (c /=)

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces 

keyword :: String -> Parser String
keyword s = lexeme (string s <* notFollowedBy alphaNum)

identifier :: Parser String
identifier = lexeme ((:) <$> letter <*> many alphaNum)

strLexeme :: String -> Parser ()
strLexeme = void . lexeme . string

semicolon :: Parser ()
semicolon = void $ lexeme $ char ';'

colon :: Parser ()
colon = void $ lexeme $ char ':'

doubleColon :: Parser ()
doubleColon = void $ lexeme $ string "::"

numberLiteral :: Parser String
numberLiteral = lexeme $
  (:) <$> digit <*> many (alphaNum <|> char '.')

betweenCharPair :: (Char, Char) -> Parser a -> Parser a
betweenCharPair (a, b) = between (lexeme $ char a) (lexeme $ char b)

stringLiteral :: Parser String
stringLiteral = betweenCharPair ('"', '"') (many innerChar)
  where innerChar = noneOf ['\\','\"'] <|> escapedChar

escapedChar :: Parser Char
escapedChar = getEscape <$> char '\\' *> oneOf (fst <$> escapedPairs)
  where
    escapedPairs = zip ['n', '"', 'r', 't'] ['\"', '\n', '\r', '\t']
    getEscape x = fromJust $ findPair x escapedPairs

charLiteral :: Parser Char
charLiteral = betweenCharPair ('\'', '\'') innerChar
  where innerChar = notChar '\\' <|> escapedChar

data BinOp
  = Plus
  | Minus
  | Mult
  | Div
  | Mod
  | LBS
  | RBS
  | BitAnd
  | BitOr
  | BoolAnd
  | BoolOr
  deriving (Show, Eq)

findPair :: (Foldable f, Eq a) => a -> f (a, b) -> Maybe b
findPair x = fmap snd . find (\(s, _) -> s == x)

-- Warning: in precedence order
binOpPairs :: [(String, BinOp)]
binOpPairs = 
  [ ("&&", BoolAnd)
  , ("||", BoolOr)
  , ("<<", LBS)
  , (">>", RBS)
  , ("&", BitAnd)
  , ("|", BitOr)
  , ("*", Mult)
  , ("/", Div)  
  , ("%", Mod)
  , ("+", Plus)
  , ("-", Minus)
  ]

toBinOp :: String -> Maybe BinOp
toBinOp s = findPair s binOpPairs 

data UnrOp
  = NumNeg
  | BoolNot
  | BitNeg
  deriving (Show, Eq)

unrOpPairs :: [(Char, UnrOp)]
unrOpPairs =
  [ ('-', NumNeg)
  , ('!', BoolNot)
  , ('~', BitNeg)
  ]

toUnrOp :: Char -> Maybe UnrOp
toUnrOp c = findPair c unrOpPairs

data Expr
  = Idntfr String
  | NumLit String
  | StrLit String
  | ChrLit Char
  | BinExp Expr BinOp Expr
  | UnrExp UnrOp Expr
  deriving (Show, Eq)

simpleExpression :: Parser Expr
simpleExpression = (NumLit <$> numberLiteral)
         <|> (StrLit <$> stringLiteral)
         <|> (ChrLit <$> charLiteral)
         <|> unaryExpression
         <|> Idntfr <$> identifier

expression :: Parser Expr
expression = try binaryExpression <|> simpleExpression

unaryExpression :: Parser Expr
unaryExpression =
  do
    maybeOp <- lexeme $ toUnrOp <$> oneOf (fst <$> unrOpPairs)
    op <- failNothing "expected unary operator" maybeOp
    UnrExp op <$> expression

binaryOps :: Parser String
binaryOps = lexeme $ choice (string . fst <$> binOpPairs)

-- TODO Use Text.Parsec.Expr to respect operator precedence
binaryOperation :: Parser BinOp
binaryOperation = toBinOp <$> binaryOps >>= failNothing "expected binary operator"

binaryExpression :: Parser Expr
binaryExpression = BinExp <$> simpleExpression <*> binaryOperation <*> expression
    
data Decl = Decl String Type deriving (Show, Eq)

type StructLiteral = [Decl]
type UnionLiteral = [Decl]

data Type
  = Pointer Type
  | TIdntfr String
  | TStruct StructLiteral
  | TUnion UnionLiteral
  | Array Expr Type
  deriving (Show, Eq)

typeP :: Parser Type
typeP = star
    <|> (TStruct <$> structLiteral)
    <|> (TUnion <$> unionLiteral)
    <|> array 
    <|> (TIdntfr <$> identifier)
    where
      star = (lexeme $ char '*') *> (Pointer <$>  typeP)
      array = Array <$> (betweenCharPair ('[', ']') expression) <*> typeP

declaration :: Parser Decl
declaration = Decl <$> identifier <* colon <*> typeP <* semicolon

declarationList :: Parser [Decl]
declarationList = betweenBraces (many declaration)

betweenBraces :: Parser a -> Parser a
betweenBraces = betweenCharPair ('{', '}') 

keywordThen :: String -> Parser a -> Parser a
keywordThen k p = (try $ keyword k) *> p

unionLiteral :: Parser StructLiteral
unionLiteral = "union" `keywordThen` declarationList

structLiteral :: Parser StructLiteral
structLiteral = "struct" `keywordThen` declarationList

data Statement
  = Union String UnionLiteral
  | Struct String StructLiteral
  | Flags String FlagsLiteral
  | Enum String EnumLiteral
  | Const String Expr
  | Proc [Decl] (Maybe Type)
  deriving (Show, Eq)

type FlagsLiteral = [String]
type EnumLiteral = [String]

statement :: Parser Statement
statement  = (identified Union unionLiteral)
         <|> (identified Struct structLiteral)
         <|> (identified Flags flagsLiteral)
         <|> (identified Enum enumLiteral)

identified :: (String -> a -> Statement) -> Parser a -> Parser Statement
identified f p = f <$> (identifier <* doubleColon) <*> p

identifierList :: Parser [String]
identifierList = betweenBraces (identifier `sepEndBy` (lexeme $ oneOf [',', ';']))

flagsLiteral :: Parser FlagsLiteral
flagsLiteral = "flags" `keywordThen` identifierList

enumLiteral :: Parser EnumLiteral
enumLiteral = "enum" `keywordThen` identifierList
