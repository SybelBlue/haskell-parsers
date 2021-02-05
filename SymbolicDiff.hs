module SymbolicDiff where

import Data.Functor

import Text.Printf ( printf )
import Text.Parsec
import Text.Parsec.String (Parser)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (spaces *> p <* eof) ""

data UnrOp = Cos | Sin | Tan | Exp | Ln deriving (Show, Eq)
data BinOp = Add | Sub | Mul | Div | Pow deriving (Show, Eq)

data Token 
    = Unary UnrOp Token
    | Binary Token BinOp Token
    | TInt Int
    | TFlt Float
    | Var String
    deriving (Show, Eq)

restringify :: Token -> String
restringify (Unary Cos x) = printf "(cos %s)" (restringify x)
restringify (Unary Sin x) = printf "(sin %s)" (restringify x)
restringify (Unary Tan x) = printf "(tan %s)" (restringify x)
restringify (Unary Exp x) = printf "(exp %s)" (restringify x)
restringify (Unary Ln x) =  printf "(ln %s)"  (restringify x)
restringify (Binary x Add y) = printf "(+ %s %s)" (restringify x) (restringify y)
restringify (Binary x Sub y) = printf "(- %s %s)" (restringify x) (restringify y)
restringify (Binary x Mul y) = printf "(* %s %s)" (restringify x) (restringify y)
restringify (Binary x Div y) = printf "(/ %s %s)" (restringify x) (restringify y)
restringify (Binary x Pow y) = printf "(^ %s %s)" (restringify x) (restringify y)
restringify (TInt x) = show x
restringify (TFlt x) = show x
restringify (Var x) = x

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

op :: String -> a -> Parser a
op s x = try (lexeme (string s) $> x)

unrOp :: Parser UnrOp
unrOp = op "cos" Cos <|> op "sin" Sin <|> op "tan" Tan <|> op "exp" Exp <|> op "ln" Ln

binOp :: Parser BinOp
binOp = op "+" Add <|> op "-" Sub <|> op "*" Mul <|> op "/" Div <|> op "^" Pow

form :: Parser Token
form = lexeme . between (char '(') (char ')') $ 
    try (Unary <$> unrOp <*> symbol) <|> (flip Binary <$> binOp <*> symbol <*> symbol)

numberLiteral :: Parser Token
numberLiteral = lexeme $
  do 
    naturals <- (:) <$> option ' ' (char '-') <*> many1 digit
    floatingPointDigits <- try ((:) <$> char '.' <*> many1 digit) <|> return []
    if null floatingPointDigits 
        then return . TInt $ read naturals
        else return . TFlt . read $ naturals ++ floatingPointDigits

symbol :: Parser Token
symbol = try form 
    <|> try numberLiteral
    <|> (Var <$> lexeme ((:) <$> letter <*> many (alphaNum <|> char '_')))

mul = flip Binary Mul

add = flip Binary Add

sub = flip Binary Sub

chainRule :: (Token -> Token) -> Token -> Token
chainRule f x = mul (f x) (derive x)

divT = flip Binary Div

pow x = Binary x Pow

derive :: Token -> Token
derive (TInt _) = TInt 0
derive (TFlt _) = TInt 0
derive (Var _)  = TInt 1
derive (Unary Sin x) = Unary Cos `chainRule` x 
derive (Unary Cos x) = (mul (TInt (-1)) . Unary Sin) `chainRule` x
derive (Unary Tan x) = (add (TInt 1) . flip pow (TInt 2) . Unary Tan) `chainRule` x
derive (Unary Exp x) = Unary Exp `chainRule` x
derive (Unary Ln  x) = divT (TInt 1) `chainRule` x
derive (Binary x Add y) = derive x `add` derive y
derive (Binary x Sub y) = derive x `sub` derive y
derive (Binary x Mul y) = mul (derive x) y `add` mul x (derive y)
derive (Binary x Div y) = (mul (derive x) y `sub` Binary x Mul (derive y)) `divT` (y `pow` TInt 2)
derive (Binary x Pow (TInt n)) = if n == 0 then TInt 0 else (TInt n `mul` (x `pow` TInt (n - 1))) `mul` derive x
derive (Binary x Pow (TFlt n)) = if n == 0 then TInt 0 else (TFlt n `mul` (x `pow` TFlt (n - 1))) `mul` derive x
derive (Binary (TInt n) Pow x) = derive $ Unary Exp $ Unary Ln (TInt n) `mul` x
derive (Binary (TFlt n) Pow x) = derive $ Unary Exp $ Unary Ln (TFlt n) `mul` x
derive x = error (show x ++ " unknown derivitive!")

removeUnit n f = removeUnit' n (\a b -> TInt (f a b))

isNum (TFlt _) = True
isNum (TInt _) = True
isNum _ = False

removeUnit' _ applyI applyF _ _ _ (TInt x) (TInt y) = applyI x y
removeUnit' _ applyI applyF _ _ _ (TInt x) (TFlt y) = TFlt (applyF (fromIntegral x) y)
removeUnit' _ applyI applyF _ _ _ (TFlt x) (TInt y) = TFlt (applyF x (fromIntegral y))
removeUnit' _ applyI applyF _ _ _ (TFlt x) (TFlt y) = TFlt (applyF x y)
removeUnit' unit i f first s merge (TInt n) y
    | isNum sy  = removeUnit' unit i f first s merge (TInt n) sy
    | n == unit = first sy
    | otherwise = merge (TInt n) sy
    where sy = simplify y
removeUnit' unit i f first s merge (TFlt n) y
    | isNum sy               = removeUnit' unit i f first s merge (TFlt n) sy
    | n == fromIntegral unit = first sy
    | otherwise              = merge (TFlt n) sy
    where sy = simplify y
removeUnit' unit i f fr second merge x (TInt m)
    | isNum sx  = removeUnit' unit i f fr second merge sx (TInt m)
    | m == unit = second sx
    | otherwise = merge sx (TInt m)
    where sx = simplify x
removeUnit' unit i f fr second merge x (TFlt m)
    | isNum sx               = removeUnit' unit i f fr second merge sx (TFlt m)
    | m == fromIntegral unit = second sx
    | otherwise              = merge sx (TFlt m)
    where sx = simplify x
removeUnit' u ai af f s merge x y = 
    let sx = simplify x
        sy = simplify y
    in if isNum sx || isNum sy 
        then removeUnit' u ai af f s merge sx sy 
        else merge sx sy

simplify :: Token -> Token
simplify (Binary x Add y) = removeUnit 0 (+) (+) id id add x y
simplify (Binary x Sub y) = removeUnit 0 (-) (-) (TInt (-1) `mul`) id sub x y
simplify (Binary _ Mul (TInt 0)) = TInt 0
simplify (Binary (TInt 0) Mul _) = TInt 0
simplify (Binary x Mul y) = removeUnit 1 (*) (*) id id mul x y
simplify (Binary x Div (TInt 1)) = simplify x
simplify (Binary x Div (TFlt 1)) = simplify x
simplify (Binary x Div y) = removeUnit' 0 divInt (/) (const $ TInt 0) divZeroError divT x y
    where divZeroError = error . (++) "Div by 0 in " . restringify
          divInt a b = TFlt (fromIntegral a / fromIntegral b)
simplify (Binary _ Pow (TInt 0)) = TInt 1
simplify (Binary (TInt 0) Pow _) = TInt 0
simplify (Binary x Pow y) = removeUnit 1 (^) (**) (const $ TInt 1) id (`Binary` Pow) x y
simplify (Unary Exp (TInt 0)) = TInt 1
simplify (Unary Exp (Unary Ln x)) = simplify x
simplify (Unary Exp x) = Unary Exp (simplify x)
simplify (Unary Ln (TInt 1)) = TInt 0
simplify (Unary Ln (Unary Exp x)) = simplify x
simplify (Unary Ln x) = Unary Ln (simplify x)
simplify x = x

sort :: Token -> Token
sort (Binary x Mul y) | isNum y && not (isNum x) = Binary y Mul (sort x)
sort (Binary x op y) = Binary (sort x) op (sort y)
sort (Unary op x) = Unary op (sort x)
sort x = x

diff :: String -> String
diff =  either show (restringify . sort . simplify . sort . derive . simplify) . parseWithEof symbol

main :: IO ()
main = putStrLn . diff =<< getLine
