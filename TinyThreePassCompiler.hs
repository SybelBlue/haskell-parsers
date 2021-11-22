{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module TinyThreePassCompiler where

import Control.Applicative
import Control.Arrow
import Control.Monad.State
import Data.Functor
import Data.List

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Eq, Show)

data AST
  = Imm Int
  | Arg Int
  | BinOp BinOp AST AST
  deriving (Eq, Show)

data Instruction
  = IM Int
  | AR Int
  | AD
  | SU
  | MU
  | DI
  | PU
  | PO
  | SW
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

compile :: String -> Either ParseError [Instruction]
compile s = pass3 . pass2 <$> pass1 s

type ParseError = String

type Parser s t = StateT [s] (Either ParseError) t

instance MonadFail (Either ParseError) where fail = Left

pass1 :: String -> Either ParseError AST
pass1 = finish <=< runStateT program . tokenize
  where
    finish (ast, ts) = if null ts then return ast else fail "expected eof"
    program = do
      char '['
      args <- takeUntil (TChar ']')
      arg_names <- mapM extractor args
      body arg_names
    extractor (TStr n) = return n
    extractor t = fail ("not a valid param name: " ++ show t)
    body args = parser (runStateT (expression args) . reverse)
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
      next "variable name" >>= \case
        TStr n | n `elem` args -> maybe (fail "impossible") (return . Arg) (n `elemIndex` args)
        _ -> fail ("expecting name in: " ++ show args)
    number = next "number" >>= \case TInt n -> return (Imm n); _ -> fail "expecting number"
    binOp (x0, x1) (y0, y1) = liftM3 BinOp ((char x0 $> x1) <|> (char y0 $> y1))
    char t = do
      x <- next ("expecting char " ++ show t)
      unless (x == TChar t) (fail $ "expecting char " ++ show t)
    takeUntil x = parser $ \ss ->
      case elemIndex x ss of
        Just n -> Right $ second tail (splitAt n ss)
        Nothing -> Left ("End of input searching for " ++ show x)
    next msg = parser (maybe (Left $ "eof expecting " ++ msg) Right . uncons)
    parser runStateT = StateT {runStateT}

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

pass3 :: AST -> [Instruction]
pass3 (Arg n) = [AR n]
pass3 (Imm n) = [IM n]
pass3 (BinOp op x y) =
  case (pass3 x, pass3 y) of
    ([x], ys) -> ys ++ [SW, x, opCode]
    (xs, [y]) -> xs ++ [SW, y] ++ [SW | op == Sub || op == Div] ++ [opCode]
    (xs, ys) -> xs ++ [PU] ++ ys ++ [SW, PO, opCode]
  where
    opCode = case op of Add -> AD; Sub -> SU; Mul -> MU; Div -> DI
