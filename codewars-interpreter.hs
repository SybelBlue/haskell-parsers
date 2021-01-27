{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Data.Functor.Identity (Identity)
import Data.Either (either)

import qualified Data.Map.Strict as Map

import Control.Monad ((>=>), void, unless)

import Text.Printf (printf)

import Text.Parsec
import Text.Parsec.Expr

import Text.Parsec.Combinator (eof, sepEndBy)
import Text.Parsec.String (Parser)

type Identifier = String

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft _ (Right x) = Right x
mapLeft f (Left m)  = Left (f m)

parseWithEof :: Parser a -> String -> Either ParseError a
parseWithEof p = parse (spaces *> p <* eof) ""

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

betweenChars :: (Char, Char) -> Parser a -> Parser a
betweenChars (a, b) = between (lexeme $ char a) (lexeme $ char b)

identifierChar :: Parser Char
identifierChar = alphaNum <|> char '_'

identifier :: Parser Identifier
identifier = lexeme $ (:) <$> letter <*> many identifierChar

keyword :: String -> Parser Identifier
keyword s = lexeme (string s <* notFollowedBy identifierChar)

keywordThen :: Identifier -> Parser a -> Parser a
keywordThen k p = (try $ keyword k) *> p

numberLiteral :: Parser Float
numberLiteral = lexeme $
  do 
    naturals <- many1 digit
    floatingPointDigits <- ((:) <$> char '.' <*> many1 digit) <|> return []
    let floatString = naturals ++ floatingPointDigits
    return $ read floatString
    
data Expr 
    = NumLit Float
    | Assign Identifier Expr
    | Lambda [Identifier] Expr
    | Invoke Identifier [Expr]
    | BinExp Expr String Expr
    deriving (Show, Eq)

simpleExpression :: Parser Expr
simpleExpression 
    = betweenChars ('(', ')') expression
    <|> (NumLit <$> numberLiteral)
    <|> try assignment
    <|> (Invoke <$> identifier <*> many expression)

expression :: Parser Expr
expression = try binaryExpression <|> simpleExpression

statement :: Parser Expr
statement = funcDefinition <|> expression

funcDefinition :: Parser Expr
funcDefinition = Assign <$> ("fn" `keywordThen` identifier)
    <*> (Lambda <$> (many identifier <* keyword "=>") <*> expression)

assignment :: Parser Expr
assignment = Assign <$> identifier <*> "=" `keywordThen` expression

binaryExpression :: Parser Expr
binaryExpression = buildExpressionParser table simpleExpression

table :: [[Operator String () Identity Expr]]
table = [ [ binary "*", binary "/", binary "%" ]
        , [ binary "+", binary "-" ]
        ]
  where binary name = Infix (flip BinExp <$> (lexeme $ string name)) AssocLeft

binOpFromString :: String -> Maybe (Float -> Float -> Float)
binOpFromString "+" = Just (+)
binOpFromString "-" = Just (-)
binOpFromString "/" = Just (/)
binOpFromString "*" = Just (*)
binOpFromString "%" = Just (\a b -> a - b * (fromIntegral $ truncate $ a / b))
binOpFromString  _  = Nothing

type Envr = Map.Map Identifier Expr

update :: Identifier -> Expr -> Envr -> Envr
update = Map.insert

fetch :: Identifier -> Envr -> GenEvalResult Expr
fetch x = failNothing ("Unknown Variable: " ++ x) . Map.lookup x

envr :: Envr
envr = Map.empty

sub :: Identifier -> Expr -> Expr -> Expr
sub var (BinExp a op b) rep = BinExp (sub var a rep) op (sub var b rep)
sub var (Invoke name []) rep = if var == name then rep else Invoke name []
sub var (Invoke name args) rep = Invoke name $ map (flip (sub var) rep) args
sub var (Assign name val) rep = Assign name $ sub var val rep
sub var (Lambda args body) rep = Lambda args $ if var `elem` args then body else (sub var body rep)
sub _ exp _ = exp

type EvalError = String
type GenEvalResult a = Either EvalError a
type EvalResult = GenEvalResult (Envr, Float)

failNothing :: EvalError -> Maybe a -> GenEvalResult a
failNothing _ (Just x) = return x
failNothing msg _ = Left msg

evalExpr :: Expr -> Envr -> EvalResult
evalExpr (NumLit f) env = return (env, f)
evalExpr (BinExp a opStr b) env = 
  do
    (envA, resA) <- evalExpr a env
    (envB, resB) <- evalExpr b envA
    op <- failNothing ("Inalid Op " ++ opStr) (binOpFromString opStr)
    return (envB, op resA resB)
evalExpr (Assign name exp) env = 
  do
    (newEnv, res) <- evalExpr exp env
    return (update name (NumLit res) newEnv, res)
evalExpr (Lambda [] body) env = evalExpr body env
evalExpr (Lambda (a:as) body) env = 
  do
    rep <- fetch a env
    evalExpr (Lambda as $ sub a body rep) env
evalExpr (Invoke name params) env = fetch name env >>= \case
        Lambda args body -> do
            unless (length params == length args)
                (Left $ printf 
                    "Wrong number of parameters supplied to %s (expected %d, got %d)"
                    name 
                    (length args)
                    (length params))
            let newEnv = foldr (uncurry update) env $ zip args params
            (_, res) <- evalExpr (Lambda [] body) newEnv
            return (env, res)
        exp -> evalExpr exp env

eval :: String -> Envr -> EvalResult
eval s env = mapLeft show (parseWithEof statement s) >>= flip evalExpr env
