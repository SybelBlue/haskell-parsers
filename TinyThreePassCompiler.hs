{-# LANGUAGE TupleSections #-}
module TinyThreePassCompiler where
import Control.Arrow
import Control.Applicative
import Control.Monad

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         | Parens AST
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

type ParseError = String
newtype Parser s t = Parser { parse :: [s] -> Either ParseError (t, [s])  }

parser f = Parser { parse = f }

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
        Left e1 -> Left $ e0 ++ " and " ++ e1
        pass -> pass
      pass -> pass

instance MonadPlus (Parser s) where

pass1 :: String -> AST
pass1 = undefined

pass2 :: AST -> AST
pass2 = undefined

pass3 :: AST -> [String]
pass3 = undefined