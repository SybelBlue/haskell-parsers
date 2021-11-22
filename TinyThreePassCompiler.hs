{-# LANGUAGE LambdaCase #-}

module TinyThreePassCompiler where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Functor
import Data.List

data BinOp = Add | Sub | Mul | Div deriving (Eq, Show)

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

newtype Parser s t = Parser {parse :: [s] -> Maybe (t, [s])}
parser f = Parser {parse = f}

instance Functor (Parser s) where
  f `fmap` p = parser (fmap (first f) . parse p)

instance Applicative (Parser s) where
  pure = return
  liftA2 fabc pa pb = parser $ \ss -> do
    (a, rest) <- parse pa ss
    first (fabc a) <$> parse pb rest

instance Monad (Parser s) where
  return x = parser (Just . (,) x)
  pa >>= fapb = parser (uncurry (parse . fapb) <=< parse pa)


instance Alternative (Parser s) where
  empty = parser (const Nothing)
  pa <|> pb = parser (\ss -> parse pa ss <|> parse pb ss)

pass1 :: String -> AST
pass1 = (\(Just (ast, [])) -> ast) . parse program . tokenize
  where
    program = do
      char '['
      args <- takeUntil (TChar ']')
      arg_names <- mapM (\case TStr n -> return n; _ -> empty) args
      body arg_names
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
    parenExpr args = char ')' *> expression args <* char '('
    variable args =
      next >>= \case
        TStr n | n `elem` args -> maybe empty (return . Arg) (n `elemIndex` args)
        _ -> empty
    number = next >>= \case TInt n -> return (Imm n); _ -> empty
    binOp (x0, x1) (y0, y1) = liftM3 BinOp ((char x0 $> x1) <|> (char y0 $> y1))
    char t = next >>= (`unless` empty) . (== TChar t)
    takeUntil x = parser (\ss -> second tail . flip splitAt ss <$> elemIndex x ss)
    next = parser uncons

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
pass3 (Arg n) = ["AR " ++ show n]
pass3 (Imm n) = ["IM " ++ show n]
pass3 (BinOp op x y) =
  case (pass3 x, pass3 y) of
    ([x], ys) -> ys ++ ["SW", x, opCode]
    (xs, [y]) -> xs ++ ["SW", y] ++ ["SW" | op == Sub || op == Div] ++ [opCode]
    (xs, ys) -> xs ++ ["PU"] ++ ys ++ ["SW", "PO", opCode]
  where
    opCode = case op of Add -> "AD"; Sub -> "SU"; Mul -> "MU"; Div -> "DI"