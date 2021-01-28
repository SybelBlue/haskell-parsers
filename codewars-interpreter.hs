{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

import Data.Functor.Identity (Identity)
import Data.Either (either)

import qualified Data.Map.Strict as Map

import Control.Monad ((>=>), void, unless, when, foldM)

import Text.Printf (printf)

import Text.Parsec
import Text.Parsec.Expr

import Text.Parsec.Combinator (eof, sepEndBy)
import Text.Parsec.String (Parser)

type Identifier = String

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f = either (Left . f) Right

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
    | Delete Identifier
    deriving (Show, Eq)

simpleExpression :: Parser Expr
simpleExpression 
    = betweenChars ('(', ')') expression
    <|> (NumLit <$> numberLiteral)
    <|> try assignment
    <|> try deletion
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

deletion :: Parser Expr
deletion = Delete <$> "del" `keywordThen` identifier

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

delete :: Identifier -> Envr -> Envr
delete = Map.delete

fetch :: Identifier -> Envr -> GenEvalResult Expr
fetch x = failNothing ("Unknown Variable: " ++ x) . Map.lookup x

blankEnvr :: Envr
blankEnvr = Map.empty

sub :: Identifier -> Expr -> Expr -> Expr
sub var (BinExp a op b) rep = BinExp (sub var a rep) op (sub var b rep)
sub var (Invoke name []) rep = if var == name then rep else Invoke name []
sub var (Invoke name args) rep = Invoke name $ map (flip (sub var) rep) args
sub var (Assign name val) rep = Assign name $ sub var val rep
sub var (Lambda args body) rep = Lambda args $ if var `elem` args then body else (sub var body rep)
sub _ exp _ = exp

type EvalError = String
type GenEvalResult a = Either EvalError a
type EvalResult = GenEvalResult (Envr, Maybe Float)

failNothing :: EvalError -> Maybe a -> GenEvalResult a
failNothing _ (Just x) = return x
failNothing msg _ = Left msg

coerce :: Maybe Float -> GenEvalResult Float
coerce = failNothing "Non-numeric value"

evalExpr :: Expr -> Envr -> EvalResult
evalExpr (NumLit f) env = return (env, Just f)
evalExpr (BinExp a opStr b) env = 
  do
    (envA, mResA) <- evalExpr a env
    resA <- coerce mResA
    (envB, mResB) <- evalExpr b envA
    resB <- coerce mResB
    op <- failNothing ("Inalid Op " ++ opStr) (binOpFromString opStr)
    return (envB, Just $ op resA resB)
evalExpr (Assign name (Lambda n as)) env = return (update name (Lambda n as) env, Nothing)
evalExpr (Assign name exp) env = 
  do
    (newEnv, mRes) <- evalExpr exp env
    res <- coerce mRes
    return (update name (NumLit res) newEnv, Just res)
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
            (finalEnv, mEvaledParams) <- foldM paramEval (env, []) params
            evaledParams <- foldM paramCoerce [] mEvaledParams
            let newEnv = foldr (uncurry update) blankEnvr $ zip args evaledParams
            (_, res) <- evalExpr (Lambda [] body) newEnv
            return (finalEnv, res)
          where 
            paramEval :: (Envr, [Maybe Float]) -> Expr -> GenEvalResult (Envr, [Maybe Float])
            paramEval (innerEnv, fs) exp = do
              (newEnv, f) <- evalExpr exp innerEnv
              return (newEnv, f:fs)
            paramCoerce :: [Expr] -> Maybe Float -> GenEvalResult [Expr]
            paramCoerce exs mf = (:exs) <$> NumLit <$> coerce mf
        exp -> evalExpr exp env
evalExpr (Delete name) env = return (delete name env, Nothing)

eval :: Envr -> String -> EvalResult
eval env = mapLeft show . parseWithEof statement >=> flip evalExpr env

evalAll :: [String] -> Envr -> GenEvalResult [Maybe Float]
evalAll []     _   = return []
evalAll (c:cs) env = 
 do (envC, res) <- eval env c
    (res :) <$> evalAll cs envC

evalLines :: String -> GenEvalResult [Maybe Float]
evalLines = flip evalAll blankEnvr . lines

printResult :: (Envr -> IO ()) -> Envr -> EvalResult -> IO ()
printResult f env (Left err) = putStrLn err >> f env
printResult f _   (Right (env, Nothing)) = f env
printResult f _   (Right (env, Just n)) = print n >> f env

main :: IO ()
main = mainLoop testEnvr
  where 
    mainLoop env = do 
      line <- getLine
      if line == "envr" 
        then print env *> mainLoop env
        else unless (line == "quit") $ printResult mainLoop env $ eval env line
  
testEnvr :: Envr
testEnvr = Map.fromList 
  [ ("iden", Lambda ["x"] $ Invoke "x" [])
  , ("incr", Lambda ["x"] $ BinExp (Invoke "x" []) "+" (NumLit 1.0))
  ]