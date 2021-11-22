{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module TinyThreePassCompiler where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe

data BinOp = Add
  | Sub
  | Mul
  | Div deriving (Eq, Show)

data AST
  = Imm Int
  | Arg Int
  | BinOp BinOp AST AST
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
pass1 = unwrap program . tokenize
  where
    program = do
      token (TChar '[')
      args <- takeUntil (TChar ']')
      arg_names <- mapM extractor args
      body arg_names
    extractor (TStr n) = return n
    extractor t = fail ("not a valid param name: " ++ show t)
    body args = parser (parse (expression args) . reverse)
    expression args = do
      s <- term args
      binOp ('+', Add) ('-', Sub) (expression args) (return s)
        <|> return s
    term args = do
      s <- factor args
      binOp ('*', Mul) ('/', Div) (term args) (return s)
        <|> return s
    factor args = parenExpr args <|> number <|> variable args
    parenExpr args = token (TChar ')') *> expression args <* token (TChar '(')
    variable args =
      next "variable name"
        >>= \case
          TStr n | n `elem` args -> return (Arg . fromJust $ n `elemIndex` args)
          _ -> fail ("expecting name in: " ++ show args)
    number =
      next "number"
        >>= \case TInt n -> return (Imm n); _ -> fail "number"
    binOp (x0, x1) (y0, y1) = liftM3 BinOp
      ( next "op" >>= \case
          TChar c | c == x0 -> return x1
          TChar c | c == y0 -> return y1
          _ -> fail $ "expecting " ++ [x0] ++ " or " ++ [y0]
      )

token t = do
  x <- next (show t)
  if x == t then return () else fail ("expecting " ++ show t)

takeUntil x = parser $ \ss ->
  case elemIndex x ss of
    Just n -> Right $ second tail (splitAt n ss)
    Nothing -> Left ("End of input searching for " ++ show x)

next msg = parser (maybeToRight ("eof expecting " ++ msg) . uncons)

pass2 :: AST -> AST
pass2 (BinOp op x y) = case (op, pass2 x, pass2 y) of
    (Add, Imm 0, y) -> y
    (Add, x, Imm 0) -> x
    (Add, Imm a, Imm b) -> Imm (a + b)
    (Sub, x, Imm 0) -> x
    (Sub, Imm a, Imm b) -> Imm (a - b)
    (Mul, Imm 0, _) -> Imm 0
    (Mul, _, Imm 0) -> Imm 0
    (Mul, Imm 1, y) -> y
    (Mul, x, Imm 1) -> x
    (Mul, Imm a, Imm b) -> Imm (a * b)
    (Div, Imm 0, _) -> Imm 0
    (Div, x, Imm 1) -> x
    (Div, Imm a, Imm b) -> Imm (a `div` b)
    (op, x, y) -> BinOp op x y
pass2 ast = ast

pass3 :: AST -> [String]
pass3 = undefined