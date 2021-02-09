{-# LANGUAGE TupleSections #-}

module TinyThreePassCompiler where

import Data.List
import Data.Bifunctor
import Control.Monad
import Control.Applicative
import Data.Functor

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

pass1 :: String -> AST
pass1 = (\(Just (ast, _)) -> ast) . parse function . tokenize

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

function = fmap cleanParens $ expr =<< betweenChars ('[', ']') (many identifier)

cleanParens (Parens x) = cleanParens x
cleanParens (Add x y) = Add (cleanParens x) (cleanParens y)
cleanParens (Sub x y) = Sub (cleanParens x) (cleanParens y)
cleanParens (Mul x y) = Mul (cleanParens x) (cleanParens y)
cleanParens (Div x y) = Div (cleanParens x) (cleanParens y)
cleanParens x = x

expr args = addSub args <|> term args

addSub args = do
  left <- term args
  op <- (char '+' $> Add) <|> (char '-' $> Sub)
  right <- expr args
  return $ rebind (op left right)

term args = mulDiv args <|> factor args

mulDiv args = do
  left <- factor args
  op <- (char '*' $> Mul) <|> (char '/' $> Div)
  right <- term args
  return $ rebind (op left right)

rebind (Div l (Mul a b)) = Mul (rebind (Div l a)) b
rebind (Div l (Sub a b)) = Sub (rebind (Div l a)) b
rebind (Div l (Add a b)) = Add (rebind (Div l a)) b
rebind (Mul l (Sub a b)) = Sub (rebind (Mul l a)) b
rebind (Mul l (Add a b)) = Add (rebind (Mul l a)) b
rebind (Sub l (Add a b)) = Add (rebind (Sub l a)) b
rebind x = x

identifier = parser iden
  where iden (TStr name) = Just name
        iden _ = Nothing

factor args = number <|> variable args <|> betweenChars ('(', ')') (Parens <$> expr args)

number = parser num
  where num (TInt n) = Just (Imm n)
        num _ = Nothing

variable args = parser var
  where var (TStr s) = Arg <$> s `elemIndex` args
        var _ = Nothing

char c = parser chr
  where chr (TChar x) | x == c = Just x
        chr _ = Nothing

betweenChars (a, b) p = char a *> p <* char b


parser :: (Token -> Maybe a) -> Parser a
parser f = Parser inner
  where inner [] = Nothing
        inner (x:xs) = (, xs) <$> f x

newtype Parser a = Parser { parse :: [Token] -> Maybe (a, [Token]) }

instance Functor Parser where
    fmap fb p = Parser $ fmap (first fb) . parse p

instance Applicative Parser where
  pure b = Parser $ Just . (b,)
  pfa <*> pa = Parser $ \l -> do
    (fa, l0) <- parse pfa l
    ( a, l1) <- parse pa  l0
    Just (fa a, l1)

instance Monad Parser where
    return = pure
    pb >>= fpc = Parser $ parse pb >=> \(res, rest) -> parse (fpc res) rest

instance MonadFail Parser where
  fail s = Parser (const Nothing)

instance Alternative Parser where
  empty = fail "empty"
  pa <|> pb = Parser $ \l -> parse pa l <|> parse pb l




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

detokenize [] = ""
detokenize (x:xs) = (++ detokenize xs) . (++ " ") $ case x of
  TChar c -> [c]
  TStr s -> s
  TInt n -> show n

angryString = "[ x y z ] ( 2 * 3 * x + 5 * y - 3 * z ) / ( 1 + 3 + 2 * 2 )"
expectedPass1 = Div (Sub (Add (Mul (Mul (Imm 2) (Imm 3)) (Arg 0)) (Mul (Imm 5) (Arg 1))) (Mul (Imm 3) (Arg 2))) (Add (Add (Imm 1) (Imm 3)) (Mul (Imm 2) (Imm 2)))


main = simulate (compile "[ a b ] a*a + b*b") [2, 3]