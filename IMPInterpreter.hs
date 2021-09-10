{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module IMPInterpreter where

-- God bless Stephanie Weirich
-- https://github.com/sweirich/dth/blob/master/regexp/src/OccDict.hs

import Prelude (Show(..), Int, Bool(..), Num, Ord, Eq, (++), print, ($))

import GHC.TypeLits
import GHC.Records
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


data Expr t where
    Skp :: Expr ()

    Lit :: forall o a. (Show a, a ~ ImpType o) => ImpType o -> Expr a

    Asn :: forall n o s a. (a ~ ImpType o, Show a, Show n, HasField n (Envr s) a) => Envr s -> n -> Expr a -> Expr (Envr s)
    Val :: forall n o s a. (a ~ ImpType o, Show n, HasField n (Envr s) a) => Envr s -> n -> Expr a 
    
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
    show Skp = "Skip"
    show (Lit x) = "Lit " ++ show x
    show (Asn _env s e) = "Asn " ++ show s  ++ " (" ++ show e ++ ")"
    show (Val _env s) = "Val " ++ show s
    show (Add e0 e1) = "Add (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (Sub e0 e1) = "Sub (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (Mul e0 e1) = "Mul (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (And e0 e1) = "And (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (Or e0 e1)  = "Or ("  ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (Not e) = "Not (" ++ show e ++ ")"
    show (GT e0 e1) = "GT (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (LT e0 e1) = "LT (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (Eq e0 e1) = "LT (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (Whl e0 e1) = "Whl (" ++ show e0 ++ ") (" ++ show e1 ++ ")"
    show (Seq e0 e1) = "Seq (" ++ show e0 ++ ") (" ++ show e1 ++ ")"

-------------------------------------------------------------------------
-- Accessor function for dictionaries (env)


-- A proof that a particular name appears in a domain ?!?
data Index (name :: Symbol) (dat :: ImpData) (syms :: SymMap) where
    DH :: Index name dat ('(name, dat):syms)
    DT :: Index name dat syms -> Index name dat (t:syms)

-- Find a name n in s, if it exists (and return a proof!) or produce a TypeError
-- NOTE: We need TypeInType to return a GADT from a type family
type Find n s = (FindH n s s :: Index n o s)

type family ShowSymMap (m :: SymMap) :: ErrorMessage where
  ShowSymMap '[]         = Text ""
  ShowSymMap '[ '(a,o) ] = Text a
  ShowSymMap ('(a,o): m) = Text a :<>: Text ", " :<>: ShowSymMap m

-- The second argument is the original association list
-- Provided so that we can create a more informative error message
type family FindH (n :: Symbol) (s :: SymMap) (s2 :: SymMap) :: Index n o s where
    FindH n ('(n,o): s) s2 = DH
    FindH n ('(t,p): s) s2 = DT (FindH n s s2)
    FindH n '[]         s2 = TypeError (
            Text "NameError: " :<>: Text n :<>: 
            Text " not in scope " :$$:
            Text "{" :<>: ShowSymMap s2 :<>: Text "}")

-- Look up an entry in the dictionary, given an index for a name
-- The functional dependency guides type inference
class Get (p :: Index n o s) | n s -> o where
    getp :: Envr s -> ImpType o

-- The entry we want is here!
instance Get DH where
    getp (E v :> _ ) = v
    {-# INLINE getp #-} -- excuse?

-- Need to keep looking
instance Get l => Get (DT l) where
    getp ( _ :> xs) = getp @_ @_ @_ @l xs
    {-# INLINE getp #-} -- excuse?

-- Instance for the Envr: if we can find the name
-- without producing a type error, then type class
-- resolution for Get will generate the correct accessor
-- function at compile time

instance (Get (Find n s :: Index n o s), t ~ ImpType o) => 
            HasField n (Envr s) t where
  getField = getp @_ @_ @_ @(Find n s)
  {-# INLINE getField #-}

-- eval :: Expr a -> Envr ts -> (a, Envr x)
-- eval (Asn n e) env = ((), E n e :> env)
-- eval _ _ = error ""

exampleEnvr = E @"a" (3 :: Int) :> E @"b" True :> Nil
main = print $ getField @"a" exampleEnvr
