{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
module TinyThreePassCompiler where


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

splitAtFirst :: (a -> Bool) -> [a] -> ([a], [a])
splitAtFirst p = either (,[]) id . foldl folder (Left []) 
  where folder (Left xs) x = if p x then Right (xs, []) else Left (x:xs)
        folder (Right (xs, ys)) y = Right (xs, y:ys)

pass1 :: String -> AST
pass1 s = do
  let tokens = tokenize s
  let (argList, tokens) = splitAtFirst ((==) $ TChar ']') tokens
  undefined

pass2 :: AST -> AST
pass2 = undefined

pass3 :: AST -> [String]
pass3 = undefined


-- function   ::= '[' arg-list ']' expression

-- arg-list   ::= /* nothing */
--               | variable arg-list

-- expression ::= term
--               | expression '+' term
--               | expression '-' term

-- term       ::= factor
--               | term '*' factor
--               | term '/' factor

-- factor     ::= number
--               | variable
--               | '(' expression ')'



newtype Parser a b = Parser { parse :: [a] -> Either String (b, [a]) }

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

oneOf :: (Eq a, Show a) => [a] -> Parser a a
oneOf as = Parser $ \case
  [] -> Left "empty stream"
  (x:xs) | x `elem` as -> Right (x, xs)
  (x:xs) -> Left (show x ++ " does not match any of " ++ show xs)

numberLiteral :: Parser Char Int
numberLiteral = read <$> many (oneOf digit)


