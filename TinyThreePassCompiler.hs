{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
module TinyThreePassCompiler where

import Data.List
import Data.Bifunctor
import Control.Monad
import Control.Applicative

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

newtype Parser a b = Parser { parse :: [a] -> Either String (b, [a]) }

alpha, digit :: String
alpha = ['a'..'z'] ++ ['A'..'Z']
digit = ['0'..'9']

tokenize :: String -> [Token]
tokenize "" = []
tokenize xxs@(c:cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i) = TInt (read i) : tokenize is
  | not (null s) = TStr s : tokenize ss
  | otherwise = tokenize cs
  where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs

compile :: String -> [String]
compile = pass3 . pass2 . pass1

pass1 :: String -> AST
pass1 = coerceParse function . tokenize

pass2 :: AST -> AST
pass2 (Add a b) = immMap (+) Add (pass2 a) (pass2 b)
pass2 (Sub a b) = immMap (-) Sub (pass2 a) (pass2 b)
pass2 (Mul a b) = immMap (*) Mul (pass2 a) (pass2 b)
pass2 (Div a b) = immMap div Div (pass2 a) (pass2 b)
pass2 x = x

immMap :: (Int -> Int -> Int) -> (AST -> AST -> AST) -> (AST -> AST -> AST)
immMap f _ (Imm x) (Imm y) = Imm (f x y)
immMap _ g a b = g a b

pass3 :: AST -> [String]
pass3 (Add x y) = pushDown "AD" x y
pass3 (Sub x y) = pushDown "SU" x y
pass3 (Mul x y) = pushDown "MU" x y
pass3 (Div x y) = pushDown "DI" x y
pass3 (Imm num) = ["IM " ++ show num]
pass3 (Arg num) = ["AR " ++ show num]

pushDown :: String -> AST -> AST -> [String]
pushDown combInst x y = if oneInst x && oneInst y 
  then pass3 y ++ "SW" : pass3 x ++ [combInst]
  else pass3 x ++ "PU" : pass3 y ++ ["SW", "PO", combInst]
  where oneInst (Imm _) = True
        oneInst (Arg _) = True
        oneInst _       = False

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

expression :: [String] -> Parser Token AST
expression args = add <|> sub <|> term args
  where add = Add <$> term args <* char '+' <*> term args
        sub = Sub <$> term args <* char '-' <*> term args

term :: [String] -> Parser Token AST
term args = mul <|> div <|> factor args
  where mul = Mul <$> factor args <* char '*' <*> term args
        div = Div <$> factor args <* char '/' <*> term args

factor :: [String] -> Parser Token AST
factor args = number <|> variable args <|> betweenChars ('(', ')') (expression args)

simulate :: [String] -> [Int] -> Int
simulate asm argv = takeR0 $ foldl' step (0, 0, []) asm where
  step (r0,r1,stack) ins =
    case ins of
      ('I':'M':xs) -> (read xs, r1, stack)
      ('A':'R':xs) -> (argv !! n, r1, stack) where n = read xs
      "SW" -> (r1, r0, stack)
      "PU" -> (r0, r1, r0:stack)
      "PO" -> (head stack, r1, tail stack)
      "AD" -> (r0 + r1, r1, stack)
      "SU" -> (r0 - r1, r1, stack)
      "MU" -> (r0 * r1, r1, stack)
      "DI" -> (r0 `div` r1, r1, stack)
  takeR0 (r0,_,_) = r0

main = simulate (compile "[ a b ] a*a + b*b") [2, 3]
