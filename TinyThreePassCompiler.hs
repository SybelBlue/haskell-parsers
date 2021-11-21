{-# LANGUAGE TupleSections #-}

module TinyThreePassCompiler where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe

data AST
  = Imm Int
  | Arg Int
  | Add AST AST
  | Sub AST AST
  | Mul AST AST
  | Div AST AST
  | Parens AST
  deriving (Eq, Show)

data Token
  = TChar Char
  | TInt Int
  | TStr String
  deriving (Eq, Show)

alpha, digit :: String
alpha = ['a' .. 'z'] ++ ['A' .. 'Z']
digit = ['0' .. '9']

tokenize :: String -> [Token]
tokenize "" = []
tokenize xxs@(c : cs)
  | c `elem` "-+*/()[]" = TChar c : tokenize cs
  | not (null i) = TInt (read i) : tokenize is
  | not (null s) = TStr s : tokenize ss
  | otherwise = tokenize cs
  where
    (i, is) = span (`elem` digit) xxs
    (s, ss) = span (`elem` alpha) xxs

compile :: String -> [String]
compile = pass3 . pass2 . pass1

type ParseError = String

newtype Parser s t = Parser {parse :: [s] -> Either ParseError (t, [s])}

unwrap p ss =
  case parse p ss of
    Left e -> error e
    Right (x, []) -> x
    Right (_, ts) -> error ("Excess tokens: " ++ show ts)

parser f = Parser {parse = f}

maybeToRight msg = maybe (Left msg) Right

instance Functor (Parser s) where
  f `fmap` p = parser (right (first f) . parse p)

instance Applicative (Parser s) where
  pure = return
  liftA2 fabc pa pb = parser $ \ss -> do
    (a, rest) <- parse pa ss
    first (fabc a) <$> parse pb rest

instance Monad (Parser s) where
  return x = parser (Right . (x,))
  pa >>= fapb = parser $ \ss -> do
    (a, rst) <- parse pa ss
    parse (fapb a) rst

instance MonadFail (Parser s) where
  fail = parser . const . Left

instance Alternative (Parser s) where
  empty = fail "Empty"
  pa <|> pb = parser $ \ss ->
    case parse pa ss of
      Left e0 -> case parse pb ss of
        Left e1 -> Left $ e0 ++ " or " ++ e1
        pass -> pass
      pass -> pass

instance MonadPlus (Parser s)

pass1 :: String -> AST
pass1 = unwrap p . tokenize
  where
    p :: Parser Token AST
    p = do
      token (TChar '[')
      args <- takeUntil (TChar ']')
      arg_names <- mapM extractor args
      body arg_names
    extractor (TStr n) = return n
    extractor t = fail ("not a valid param name: " ++ show t)
    body args = parser (parse (expression args) . reverse)
    expression args = do
      s <- term args
      (do
        op <- next "op"
        case op of
          TChar '+' -> do
            f <- expression args
            return (Add f s)
          TChar '-' -> do
            f <- expression args
            return (Sub f s)
          _ ->
            fail "expecting op") 
        <|> return s
    term args = do
      s <- factor args
      (do
        op <- next "op"
        case op of
          TChar '*' -> do
            f <- term args
            return (Mul f s)
          TChar '/' -> do
            f <- term args
            return (Div f s)
          _ ->
            fail "expecting op") 
        <|> return s
    factor args = parenExpr args <|> number <|> variable args
    parenExpr args = token (TChar ')') *> expression args <* token (TChar '(')
    variable args = do
      x <- next "variable name"
      case x of
        TStr n -> if n `elem` args 
          then return (Arg . fromJust $ n `elemIndex` args) 
          else fail ("expecting name in: " ++ show args)
        _ -> fail "expecting variable name"
    number = do
      x <- next "number"
      case x of
        TInt n -> return (Imm n)
        _ -> fail "number"

token t = do
  x <- next (show t)
  if x == t then return () else fail ("expecting " ++ show t)

takeUntil x = parser $ \ss ->
  case elemIndex x ss of
    Just n -> Right $ second tail (splitAt n ss)
    Nothing -> Left ("End of input searching for " ++ show x)

next msg = parser (maybeToRight ("eof expecting " ++ msg) . uncons)

pass2 :: AST -> AST
pass2 = undefined

pass3 :: AST -> [String]
pass3 = undefined