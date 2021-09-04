-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE KindSignatures #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module IMPInterpreter where

-- God bless Stephanie Weirich
-- https://github.com/sweirich/dth/blob/master/regexp/src/OccDict.hs

import GHC.TypeLits (Symbol)
import Data.Kind (Type)

data ImpData = I | B

type SymMap = [(Symbol, ImpData)]

data Entry :: Symbol -> ImpData -> Type where
    E :: forall n o. ImpType o -> Entry n o

type family ImpType (o :: ImpData) = (res :: Type) | res -> o where
    ImpType I = Int
    ImpType B = Bool

data Envr :: SymMap -> Type where
    Nil :: Envr '[]
    (:>) :: Entry n o -> Envr tl -> Envr ('(n,o) : tl)
infixr 5 :>

exampleEnvr = E @"a" (3 :: Int) :> E @"b" True :> Nil

data Expr t where
    Skip :: Expr ()

    Lit :: forall o a. (Show a, a ~ ImpType o) => ImpType o -> Expr a

    Asn :: Show a => String -> Expr a -> Expr ()
    Val :: String -> Expr a -- how do I fix this
    
    Add :: Num a => Expr a -> Expr a -> Expr a
    Sub :: Num a => Expr a -> Expr a -> Expr a
    Mul :: Num a => Expr a -> Expr a -> Expr a

    And :: Expr Bool -> Expr Bool -> Expr Bool
    Or  :: Expr Bool -> Expr Bool -> Expr Bool
    Not :: Expr Bool -> Expr Bool

    GT :: forall a. (Show a, Ord a) => Expr a -> Expr a -> Expr Bool
    LT :: forall a. (Show a, Ord a) => Expr a -> Expr a -> Expr Bool
    Eq :: forall a. (Show a, Eq a)  => Expr a -> Expr a -> Expr Bool

    Whl :: Expr Bool -> Expr () -> Expr ()
    Seq :: Show a => Expr () -> Expr a -> Expr a

instance Show t => Show (Expr t) where
    show Skip = "Skip"
    show (Lit x) = "Lit " ++ show x
    show (Asn s e) = "Asn " ++ show s  ++ " (" ++ show e ++ ")"
    show (Val s) = "Val " ++ s
    show (Add e0 e1) = "Add (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (Sub e0 e1) = "Sub (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (Mul e0 e1) = "Mul (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (And e0 e1) = "And (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (Or e0 e1)  = "Or ("  ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (Not e) = "Not (" ++ show e ++ ")"
    show (IMPInterpreter.GT e0 e1) = "GT (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (IMPInterpreter.LT e0 e1) = "LT (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (IMPInterpreter.Eq e0 e1) = "LT (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (Whl e0 e1) = "Whl (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (Seq e0 e1) = "Seq (" ++ show e0 ++ ") (" ++ show e1 ++ ")"

main = print $ Seq (Asn "a" (Lit (3 :: Int))) (Whl (IMPInterpreter.GT (Val "a") (Lit (0 :: Int))) (Asn "a" (Sub (Val "a") (Lit (1 :: Int)))))
