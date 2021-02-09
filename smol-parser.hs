{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Data.Bifunctor
import Data.List
import Control.Monad

-- example datas to permute between
data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

newtype Parser a b = Parser { parse :: [a] -> Either String (b, [a]) }

coerceParse :: Parser a b -> [a] -> b
coerceParse p = (\(Right (b, _)) -> b) . parse p

instance Functor (Parser a) where
    fmap fb p = Parser $ second (first fb) . parse p

instance Applicative (Parser a) where
  pure b = Parser $ Right . (b,)
  pfa <*> pa = Parser $ \l -> do
    (fa, l0) <- parse pfa l
    ( a, l1) <- parse pa  l0
    Right (fa a, l1)

instance Monad (Parser a) where
    return = pure
    (>>=) :: Parser a b -> (b -> Parser a c) -> Parser a c
    pb >>= fpc = Parser $ parse pb >=> \(res, rest) -> parse (fpc res) rest

instance MonadFail (Parser a) where
  fail s = Parser (const $ Left s)

instance Alternative (Parser a) where
  empty = fail "empty"
  pa <|> pb = Parser $ \l ->
    case parse pa l of
      Left e0 -> 
        case parse pb l of
          Left e1 -> Left (e0 ++ " and " ++ e1)
          x -> x
      x -> x

parser :: (a -> Either String b) -> Parser a b
parser f = Parser inner
  where inner (x:xs) = second (, xs) (f x)
        inner [] = Left "empty stream"

number :: Parser Token AST
number = parser num
  where num (TInt x) = Right (Imm x)
        num _        = Left "not a number"

identifier :: Parser Token String
identifier = parser ident
  where ident (TStr s) = Right s
        ident _        = Left "not an identifier"

variable :: [String] -> Parser Token AST
variable vars = do
  name <- identifier
  maybe (fail $ "unkown symbol " ++ name) (return . Arg) (name `elemIndex` vars)

char :: Char -> Parser Token Char
char c = parser chr
  where chr (TChar x) | x == c = Right c
        chr _                  = Left $ "token is not '" ++ c:"'"

betweenChars :: (Char, Char) -> Parser Token b -> Parser Token b
betweenChars (a, b) p = char a *> p <* char b

function :: Parser Token AST
function = expression =<< betweenChars ('[', ']') (many identifier)


----------------- WARNING, DOES NOT PEMDAS CORRECTLY --------------
expression :: [String] -> Parser Token AST
expression args = add <|> sub <|> term args
  where add = Add <$> term args <* char '+' <*> term args
        sub = Sub <$> term args <* char '-' <*> term args

term :: [String] -> Parser Token AST
term args = mul <|> div <|> factor args
  where mul = Mul <$> factor args <* char '*' <*> term args
        div = Div <$> factor args <* char '/' <*> term args
---------------------------------------------------------------------

factor :: [String] -> Parser Token AST
factor args = number <|> variable args <|> betweenChars ('(', ')') (expression args)
