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

eol :: Parser ()
eol = (void . lexeme $ char '\n') <|> eof

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

notChar :: Char -> Parser Char
notChar c = satisfy (c /=)

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces 

identifierChar :: Parser Char
identifierChar = alphaNum <|> char '_'

keyword :: String -> Parser Identifier
keyword s = lexeme (string s <* notFollowedBy identifierChar)

identifier :: Parser Identifier
identifier = lexeme ((:) <$> letter <*> many identifierChar)

strLexeme :: String -> Parser ()
strLexeme = void . lexeme . string

semicolonOrComma :: Parser ()
semicolonOrComma = void $ lexeme $ oneOf [';', ',']

colon :: Parser ()
colon = void $ lexeme $ char ':'

doubleColon :: Parser ()
doubleColon = void $ lexeme $ string "::"

numberLiteral :: Parser String
numberLiteral = lexeme $
  (:) <$> digit <*> many (alphaNum <|> char '.')

betweenChars :: (Char, Char) -> Parser a -> Parser a
betweenChars (a, b) = between (lexeme $ char a) (lexeme $ char b)

stringLiteral :: Parser String
stringLiteral = betweenChars ('"', '"') (many innerChar)
  where innerChar = noneOf ['\\','\"'] <|> escapedChar

escapedChar :: Parser Char
escapedChar = getEscape <$> char '\\' *> oneOf (fst <$> escapedPairs)
  where
    escapedPairs = zip ['n', '"', 'r', 't'] ['\"', '\n', '\r', '\t']
    getEscape x = fromJust $ findPair x escapedPairs

charLiteral :: Parser Char
charLiteral = betweenChars ('\'', '\'') innerChar
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

type Identifier = String

type FlagsLiteral = [Identifier]
type EnumLiteral = [Identifier]

data Declaration
  = Decl Identifier Type
  | DeclTags [Tag] Declaration
  deriving (Show, Eq)

type StructLiteral = [Declaration]
type UnionLiteral = [Declaration]

data Expr
  = Idntfr Identifier
  | NumLit String
  | StrLit String
  | ChrLit Char
  | BinExp Expr BinOp Expr
  | UnrExp UnrOp Expr
  deriving (Show, Eq)

data Type
  = Pointer Type
  | TIdntfr Identifier
  | TStruct StructLiteral
  | TUnion UnionLiteral
  | Array Expr Type
  deriving (Show, Eq)

data Statement
  = Union Identifier UnionLiteral
  | Struct Identifier StructLiteral
  | Flags Identifier FlagsLiteral
  | Enum Identifier EnumLiteral
  | Const Identifier Expr
  | Proc Identifier [Declaration] (Maybe Type)
  | Tagged [Tag] Statement
  deriving (Show, Eq)

typeP :: Parser Type
typeP = star
    <|> (TStruct <$> structLiteral)
    <|> (TUnion <$> unionLiteral)
    <|> array 
    <|> (TIdntfr <$> identifier)
    where
      star = (lexeme $ char '*') *> (Pointer <$>  typeP)
      array = Array <$> (betweenChars ('[', ']') expression) <*> typeP

declaration :: Parser Declaration
declaration = (DeclTags <$> (many1 tag) <*> declaration)
  <|> (Decl <$> identifier <* colon <*> typeP)

declarationList :: Parser [Declaration]
declarationList = betweenBraces (declaration `sepEndBy` semicolonOrComma)

betweenBraces :: Parser a -> Parser a
betweenBraces = betweenChars ('{', '}') 

keywordThen :: Identifier -> Parser a -> Parser a
keywordThen k p = (try $ keyword k) *> p

unionLiteral :: Parser StructLiteral
unionLiteral = "union" `keywordThen` declarationList

structLiteral :: Parser StructLiteral
structLiteral = "struct" `keywordThen` declarationList

statement :: Parser Statement
statement = skipMany (try comment) *> ((Tagged <$> many1 tag) <|> return id) <*> simpleStatement

simpleStatement :: Parser Statement
simpleStatement = (identified Union unionLiteral)
         <|> (identified Struct structLiteral)
         <|> (identified Flags flagsLiteral)
         <|> (identified Enum enumLiteral)
         -- <|> (identified Proc (proc-stuff))
         <|> (identified Const expression)

comment :: Parser ()
comment = (try lineComment) <|> (try blockComment)

lineComment :: Parser ()
lineComment = void . lexeme $ (string "//") *> manyTill anyChar eol

blockComment :: Parser ()
blockComment = void . lexeme $ (string "/*") *> manyTill anyChar (try $ string "*/")

identified :: (Identifier -> a -> Statement) -> Parser a -> Parser Statement
identified f p = try (f <$> (identifier <* doubleColon) <*> p)

identifierList :: Parser [Identifier]
identifierList = betweenBraces (identifier `sepEndBy` semicolonOrComma)

flagsLiteral :: Parser FlagsLiteral
flagsLiteral = "flags" `keywordThen` identifierList

enumLiteral :: Parser EnumLiteral
enumLiteral = "enum" `keywordThen` identifierList

data Tag = Tag Identifier [Expr] deriving (Show, Eq)

tag :: Parser Tag
tag = Tag <$> ((lexeme $ char '@') *> identifier) <*> (try argList <|> return [])

genericParamList :: Parser a -> Parser [a]
genericParamList = betweenChars ('(', ')') . flip sepEndBy (lexeme $ char ',')

paramList :: Parser [Identifier]
paramList = genericParamList identifier

argList :: Parser [Expr]
argList = genericParamList expression

script :: Parser [Statement]
script = (:) <$> statement <*> ((lookAhead eof *> return []) <|> script)

