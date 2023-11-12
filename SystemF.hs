{-# LANGUAGE GADTs, KindSignatures, FlexibleInstances, EmptyDataDecls #-}

module SystemF where

import Prelude hiding (succ)
import Data.Either (isRight)

-- Background
-- This is a copy of the SystemF implementation by DanBurton.

type ErrOr = Either String

good = Right
err = Left

isGood = isRight

get (Right d) = d
get (Left e) = error e

-- The language has primitives.
data Primitive :: * -> * where
    Num  :: Integer -> Primitive Integer
    Succ :: Primitive (Integer -> Integer)
-- Note, Num is not (Num a), but a *

-- In System F, types are also values.
-- You apply Type Literals to Type Abstractions

-- 3 types are provided here, Num, Functions, and Type Abstraction
-- represented here with Forall (V).
-- The 4th, TyVar, is just a hack for printing/eq testing
data Type :: * -> * where
    NumTy :: Type Integer
    FunTy :: Type a -> Type b -> Type (a -> b)
    VTy   :: (Type a -> Type b) -> Type (V a b)
    TyVar :: Char -> Type a

-- This represents our forall quantification
data V a b

instance Eq (Type a) where
    (==) = eqTy (['a'..'z'] ++ ['A'..'Z'])

-- Type equality here defined based on a limited list of chars
-- which provides for 52 type variables.
eqTy :: [Char] -> Type a -> Type b -> Bool
eqTy _ NumTy NumTy = True
eqTy cs (FunTy p b) (FunTy p' b') = eqTy cs p p' && eqTy cs b b'
eqTy (c:cs) (VTy f) (VTy f') = eqTy cs (f (TyVar c)) (f' (TyVar c))
eqTy [] _ _ = error "Out of tyvar names"
eqTy _ (TyVar c) (TyVar c') = c == c'
eqTy _ _ _ = False


instance Show (Type a) where
    show = showTy ("xyz" ++ ['a'..'w'])

-- Show defined on a limited list of chars which provides for 26 applications
showTy :: [Char] -> Type a -> [Char]
showTy _ NumTy = "Num"
showTy cs (FunTy p b) = "(" ++ showTy cs p ++ " -> " ++ showTy cs b ++ ")"
showTy (c:cs) (VTy f) = "(∀ " ++ [c] ++ ". " ++ showTy cs (f (TyVar c)) ++ ")"
showTy [] VTy{} = error "Too many nested type applications"
showTy _ (TyVar t) = [t]


-- Terms!
data Term :: * -> * where
    Prim :: Primitive a -> Term a
    Abs  :: Type a -> (Term a -> Term b) -> Term (a -> b)
    App  :: Term (a -> b) -> Term a -> Term b
    TAbs :: (Type a -> Term b) -> Term (V a b)
    TApp :: Term (V a b) -> Type a -> Term b
    Unknown :: Char -> Term a
-- Unknown is another hack like TyVar

-- to get ghci to act as system f repl, need to define a show
instance Show a => Show (Term a) where
    show t = let v = get $ eval t
                 ty = get $ typeOf t
             in show v ++ " :: " ++ show ty


instance Num (Term Integer) where
  fromInteger = num
  (+) = undefined
  (*) = undefined
  abs = undefined
  signum = undefined
  (-) = undefined


-- Now for evaluation
-- using big step reduction
eval' :: Term a -> ErrOr (Term a)
eval' (Prim p)  = good $ Prim p
eval' (Abs t f) = good $ Abs t f
eval' (TAbs f)  = good $ TAbs f
eval' (App f x) = do
    f' <- eval f
    res <- runApp f' <*> eval' x
    eval' res
eval' (TApp f x) = do
    f' <- eval' f
    res <- runTApp f' <*> pure x
    eval' res
eval' Unknown{} = undefined

eval :: Term a -> ErrOr a
eval t = eval' t >>= valueOf


-- Functions todo

valueOf :: Term a -> ErrOr a
valueOf (Prim n) = fromPrim n
valueOf _ = err "Not a value"

fromPrim :: Primitive a -> ErrOr a
fromPrim (Num n) = good n
fromPrim _ = err "fromPrim used wrong"


runApp :: Term (a -> b) -> ErrOr (Term a -> Term b)
runApp (Abs t f) = good f
runApp (Prim p)  = runAppPrim p
runApp _ = err "unexpected non-abstraction used in application"

runAppPrim :: Primitive (a -> b) -> ErrOr (Term a -> Term b)
runAppPrim Succ = good $ \(Prim (Num n)) -> num (n + 1)

runTApp :: Term (V a b) -> ErrOr (Type a -> Term b)
runTApp (TAbs f) = good f
runTApp _ = err "runTApp failed"

--Typing