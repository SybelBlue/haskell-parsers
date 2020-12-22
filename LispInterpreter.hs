{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module LispInterpreter where

import Data.Tuple (snd)
import Data.Map.Strict (Map, (!?), fromList, insert, union)

import qualified LispParser as P

data Expr
  = Call String [Expr]
  | Func String Expr
  | Sym String
  | Num Integer
  | Def String Expr
  | Quote Expr
  | Unit
  deriving (Show, Eq, Read)

type Envr = Map String Expr
type IntrError = String
type IntrResult = Either IntrError Expr

iok :: Expr -> Either a Expr
iok = Right

ierr :: IntrError -> Either IntrError a
ierr = Left

get :: Envr -> String -> IntrResult
get env name = maybe failed iok (env !? name)
  where failed = ierr $ "Undefined symbol " ++ name

fromTokens :: [P.Token] -> [Expr]
fromTokens = (<$>) fromToken

fromToken :: P.Token -> Expr
fromToken = \case
  P.Def name args expr -> Def name (curried args $ fromToken expr)
  P.Form name ts -> Call name $ fromTokens ts
  P.Num n -> Num n
  P.Var s -> Sym s
  P.Quote t -> Quote (fromToken t)

curried :: [String] -> Expr -> Expr
curried [] = id
curried (a:as) = Func a . curried as 

call :: Envr -> String -> [Expr] -> IntrResult
call env name args =
  do
    stored <- get env name
    (argPairs, body) <- uncurryWith env stored args
    newEnv <- Right $ union env (fromList argPairs)
    res <- snd $ interpret newEnv body
    return res 

uncurryWith :: Envr -> Expr -> [Expr] -> Either IntrError ([(String, Expr)], Expr)
uncurryWith _ expr [] = Right ([], expr)
uncurryWith env (Func name expr) (a:args) =
  do
    argEval <- snd $ interpret env a
    (pairs, body) <- uncurryWith env expr args
    return . (,body) . (:pairs) $ (name, argEval) 
uncurryWith _ _ _ = ierr "Given too many arguments"

interpret :: Envr -> Expr -> (Envr, IntrResult)
interpret env (Call name exprs) = (env, call env name exprs)
interpret env (Sym name) = (env, get env name)
interpret env (Def name expr) = (, iok Unit) $ insert name expr env
interpret env numOrQuote = (env, iok numOrQuote)    

main :: IO ()
main = print $ P.Num 123
