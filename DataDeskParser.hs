{-# OPTIONS -fno-warn-unused-do-bind #-}
-- don't warn on unused parse captures
module DataDeskParser where

import Control.Monad (void)
import Data.Functor.Identity (Identity)
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Tuple (fst, snd)
import System.Environment (getArgs)
import Text.Parsec
import Text.Parsec.Combinator (eof, sepEndBy)
import Text.Parsec.Expr
import Text.Parsec.String (Parser)
import qualified Data.Functor

failNothing :: MonadFail m => String -> Maybe a -> m a
failNothing msg = maybe (fail msg) return

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (spaces *> p <* eof) ""

eol :: Parser ()
eol = (void . lexeme $ char '\n') <|> eof

whitespaceChars :: String
whitespaceChars = " \n\t"

parseFile :: String -> IO ()
parseFile path =
  do
    text <- readFile path
    either print (mapM_ print) $ parse script path text

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
numberLiteral =
  lexeme $
    (:) <$> digit <*> many (alphaNum <|> char '.')

betweenChars :: (Char, Char) -> Parser a -> Parser a
betweenChars (a, b) = between (lexeme $ char a) (lexeme $ char b)

stringLiteral :: Parser String
stringLiteral = betweenChars ('"', '"') (many innerChar)
  where
    innerChar = noneOf ['\\', '\"'] <|> escapedChar

escapedChar :: Parser Char
escapedChar = getEscape <$> char '\\' *> oneOf (fst <$> escapedPairs)
  where
    escapedPairs = zip ['n', '"', 'r', 't'] ['\"', '\n', '\r', '\t']
    getEscape x = fromJust $ findPair x escapedPairs

charLiteral :: Parser Char
charLiteral = betweenChars ('\'', '\'') innerChar
  where
    innerChar = notChar '\\' <|> escapedChar

toUnrOp :: Char -> Maybe UnrOp
toUnrOp c = findPair c unrOpPairs

simpleExpression :: Parser Expr
simpleExpression =
  betweenChars ('(', ')') expression
    <|> NumLit <$> numberLiteral
    <|> StrLit <$> stringLiteral
    <|> ChrLit <$> charLiteral
    <|> Idntfr <$> identifier

expression :: Parser Expr
expression = try nAryExpression <|> simpleExpression

binaryOps :: Parser String
binaryOps = lexeme $ choice (string . fst <$> binOpPairs)

unaryOps :: Parser Char
unaryOps = lexeme $ choice (char . fst <$> unrOpPairs)

binaryOperation :: String -> Parser BinOp
binaryOperation op = lexeme (string op) *> failNothing ("no such op " ++ op) (toBinOp op)

unaryOperation :: Char -> Parser UnrOp
unaryOperation op = lexeme (char op) *> failNothing ("no such op " ++ [op]) (toUnrOp op)

nAryExpression :: Parser Expr
nAryExpression = buildExpressionParser table simpleExpression

table :: [[Operator String () Identity Expr]]
table =
  [ [unary '~', unary '-'],
    [binary "&" AssocLeft, binary "|" AssocLeft],
    [binary "<<" AssocRight, binary ">>" AssocRight],
    [binary "*" AssocLeft, binary "/" AssocLeft, binary "%" AssocLeft],
    [binary "+" AssocLeft, binary "-" AssocLeft],
    -- ,[binary "<" AssocNone, binary ">" AssocNone]
    [binary "=" AssocRight],
    [unary '!'],
    [binary "&&" AssocLeft],
    [binary "||" AssocLeft]
  ]
  where
    binary name = Infix (flip BinExp <$> binaryOperation name)
    unary name = Prefix (UnrExp <$> unaryOperation name)

type Identifier = String

data Tag = Tag Identifier [Expr] deriving (Show, Eq)

data Tagged a = Tagged [Tag] a deriving (Show, Eq)

data Declaration = Declaration Identifier Type deriving (Show, Eq)

data Expr
  = Idntfr Identifier
  | NumLit String
  | StrLit String
  | ChrLit Char
  | BinExp Expr BinOp Expr
  | UnrExp UnrOp Expr
  deriving (Show, Eq)

type FlagsLiteral = [Tagged Identifier]

type EnumLiteral = [Tagged Identifier]

type StructLiteral = [Tagged Declaration]

type UnionLiteral = [Tagged Declaration]

type ProcLiteral = ([Tagged Declaration], Maybe Type)

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
  | Proc Identifier ProcLiteral
  deriving (Show, Eq)

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
  [ ("&&", BoolAnd),
    ("||", BoolOr),
    ("<<", LBS),
    (">>", RBS),
    ("&", BitAnd),
    ("|", BitOr),
    ("*", Mult),
    ("/", Div),
    ("%", Mod),
    ("+", Plus),
    ("-", Minus)
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
  [ ('-', NumNeg),
    ('!', BoolNot),
    ('~', BitNeg)
  ]

typeP :: Parser Type
typeP =
  star
    <|> TStruct <$> structLiteral
    <|> TUnion <$> unionLiteral
    <|> array
    <|> TIdntfr <$> identifier
  where
    star = lexeme (char '*') *> (Pointer <$> typeP)
    array = Array <$> betweenChars ('[', ']') expression <*> typeP

declaration :: Parser Declaration
declaration = Declaration <$> (identifier <* colon) <*> typeP

declarationList :: (Char, Char) -> Parser [Tagged Declaration]
declarationList c = betweenChars c $ tagged declaration `sepEndBy` semicolonOrComma

betweenBraces :: Parser a -> Parser a
betweenBraces = betweenChars ('{', '}')

keywordThen :: Identifier -> Parser a -> Parser a
keywordThen k p = try (keyword k) *> p

unionLiteral :: Parser StructLiteral
unionLiteral = "union" `keywordThen` declarationList ('{', '}')

structLiteral :: Parser StructLiteral
structLiteral = "struct" `keywordThen` declarationList ('{', '}')

tagged :: Parser a -> Parser (Tagged a)
tagged p = Tagged <$> many tag <*> p

statement :: Parser (Tagged Statement)
statement = skipMany (try comment) *> tagged simpleStatement

simpleStatement :: Parser Statement
simpleStatement =
  identified Union unionLiteral
    <|> identified Struct structLiteral
    <|> identified Flags flagsLiteral
    <|> identified Enum enumLiteral
    <|> identified Proc procLiteral
    <|> identified Const expression

comment :: Parser ()
comment = try lineComment <|> try blockComment

lineComment :: Parser ()
lineComment = void . lexeme $ string "//" *> manyTill anyChar eol

blockComment :: Parser ()
blockComment = void . lexeme $ string "/*" *> manyTill anyChar (try $ string "*/")

identified :: (Identifier -> a -> Statement) -> Parser a -> Parser Statement
identified f p = try (f <$> (identifier <* doubleColon) <*> p)

identifierList :: Parser [Tagged Identifier]
identifierList = betweenBraces (tagged identifier `sepEndBy` semicolonOrComma)

flagsLiteral :: Parser FlagsLiteral
flagsLiteral = "flags" `keywordThen` identifierList

enumLiteral :: Parser EnumLiteral
enumLiteral = "enum" `keywordThen` identifierList

tag :: Parser Tag
tag = Tag <$> (lexeme (char '@') *> identifier) <*> (try argList <|> return [])

genericParamList :: Parser a -> Parser [a]
genericParamList = betweenChars ('(', ')') . flip sepEndBy (lexeme $ char ',')

paramList :: Parser [Identifier]
paramList = genericParamList identifier

argList :: Parser [Expr]
argList = genericParamList expression

script :: Parser [Tagged Statement]
script = (:) <$> statement <*> ((lookAhead eof Data.Functor.$> []) <|> script)

procLiteral :: Parser ProcLiteral
procLiteral =
  (,)
    <$> "proc" `keywordThen` declarationList ('(', ')')
    <*> optionMaybe ("->" `keywordThen` typeP)
