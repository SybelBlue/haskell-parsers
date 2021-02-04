{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParser where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)] }


runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not conusme entire stream"
    _           -> error "Parser error."

item :: Parser Char
item = Parser $
  \case
    []     -> []
    (c:cs) -> [(c, cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $
  \s ->
    concatMap innerMap $ parse p s
    where
      innerMap (a, s') = parse (f a) s'

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) =
    Parser (\s ->
              [ (f a, s2)
              | (f, s1) <- cs1 s
              , (a, s2) <- cs2 s1
              ]
           )

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser $ const []

option :: Parser a -> Parser a -> Parser a
option  p q = Parser $ \s ->
  case parse p s of
    []  -> parse q s
    res -> res

instance Monad Parser where
  return = unit
  (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option
-- with Alternative come some and many
-- many :: f a -> f [a]
--      many repeatedly applies a single function
--      until the function fails
-- some :: f a -> f [a]
--      some is similar to many except that it will
--      fail itself if there is not at least one match

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item `bind` \c ->
  if p c then return c else Parser (const [])

oneOf :: [Char] -> Parser Char
oneOf = satisfy . flip elem

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

-- parses one or more occurrences of p, separated by op and
-- returns a value obtained by a recursing until failure on
-- the left hand side of the stream.
-- This can be used to parse left-recursive grammar.
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do { a <- p; rest a }
        where rest a = (do f <- op
                           b <- p
                           rest (f a b))
                       <|> return a

-- using these last four functions, we can write
-- several parsers for common patterns of text,
-- eg numbers, parenthetical expressions, whitespace

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) =
  do
    char c
    string cs
    return (c:cs)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

token :: Parser a -> Parser a
token p = do { a <- p; spaces; return a }

reserved :: String -> Parser String
reserved = token . string

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number =
  do
    s <- string "-" <|> return []
    cs <- some digit
    return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m =
  do
    reserved "("
    n <- m
    reserved ")"
    return n

--- Calculator Grammar Spec
-- number = [ "" ] digit { digit }.
-- digit  = "0" | "1" | ... | "8" | "9".
-- expr   = term { addop term }.
-- term   = factor { mulop factor }.
-- factor = "(" expr ")" | number.
-- addop  = "+" | "-".
-- mulop  = "*".

-- now we implement the parser for this grammar

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving Show

eval :: Expr -> Int
eval = \case
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Lit n   -> n

int :: Parser Expr
int = Lit <$> number

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor = int <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop :: Parser (Expr -> Expr -> Expr)
addop = infixOp "+" Add <|> infixOp "-" Sub

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

run :: String -> Expr
run = runParser expr

main :: IO ()
main = forever $
  do
    putStr "> "
    a <- getLine
    print $ eval $ run a
