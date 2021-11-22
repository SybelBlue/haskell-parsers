{-# LANGUAGE LambdaCase #-}

module TinyThreePassCompiler where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Functor
import Data.List

data AST
  = Imm Int
  | Arg Int
  | Add AST AST
  | Sub AST AST
  | Mul AST AST
  | Div AST AST
  deriving (Eq, Show)

destruct = \case Add x y -> ("AD", x, y); Sub x y -> ("SU", x, y); Mul x y -> ("MU", x, y); Div x y -> ("DI", x, y); _ -> error "impossible"

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
    binOp (x0, x1) (y0, y1) f s = (char x0 $> x1 <|> char y0 $> y1) <*> f <*> s
    char t = next >>= (`unless` empty) . (== TChar t)
    takeUntil x = parser (\ss -> second tail . flip splitAt ss <$> elemIndex x ss)
    next = parser uncons

pass2 :: AST -> AST
pass2 (Imm x) = Imm x
pass2 (Arg x) = Arg x
pass2 binop = let (op, x, y) = destruct binop in
  case (op, pass2 x, pass2 y) of
    ("AD", Imm 0, y) -> y
    ("AD", x, Imm 0) -> x
    ("AD", Imm a, Imm b) -> Imm (a + b)
    ("SU", x, Imm 0) -> x
    ("SU", Imm a, Imm b) -> Imm (a - b)
    ("MU", Imm 0, _) -> Imm 0
    ("MU", _, Imm 0) -> Imm 0
    ("MU", Imm 1, y) -> y
    ("MU", x, Imm 1) -> x
    ("MU", Imm a, Imm b) -> Imm (a * b)
    ("DI", Imm 0, _) -> Imm 0
    ("DI", x, Imm 1) -> x
    ("DI", Imm a, Imm b) -> Imm (a `div` b)
    ("AD", x, y) -> Add x y
    ("SU", x, y) -> Sub x y
    ("MU", x, y) -> Mul x y
    ("DI", x, y) -> Div x y
    _ -> error "impossible"

pass3 :: AST -> [String]
pass3 (Arg n) = ["AR " ++ show n]
pass3 (Imm n) = ["IM " ++ show n]
pass3 ast =
  case (pass3 x, pass3 y) of
    ([x], ys) -> ys ++ ["SW", x, opCode]
    (xs, [y]) -> xs ++ ["SW", y] ++ ["SW" | opCode == "SU" || opCode == "DI"] ++ [opCode]
    (xs, ys) -> xs ++ ["PU"] ++ ys ++ ["SW", "PO", opCode]
  where
    (opCode, x, y) = destruct ast