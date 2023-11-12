{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Tagless where

import Prelude hiding (and, or)
import Data.Function (on)


import Test.Hspec hiding (before)

class Language r where
  here   :: r (a, h) a
  before :: r h a -> r (any, h) a
  lambda :: r (a, h) b -> r h (a -> b)
  apply  :: r h (a -> b) -> (r h a -> r h b)

  loop   :: r h (a -> a) -> r h a

  int    :: Int -> r h Int
  add    :: r h Int -> r h Int -> r h Int
  down   :: r h Int -> r h Int    -- \x -> x - 1
  up     :: r h Int -> r h Int    -- \x -> x + 1
  mult   :: r h Int -> r h Int -> r h Int
  gte    :: r h Int -> r h Int -> r h Bool -- greater than or equal

  bool   :: Bool -> r h Bool
  and    :: r h Bool -> r h Bool -> r h Bool
  or     :: r h Bool -> r h Bool -> r h Bool
  neg    :: r h Bool -> r h Bool

  ifte   :: r h Bool -> r h a -> r h a -> r h a -- if true then return left term, else return right term

type Term a = forall r h . Language r => r h a

data Lang h out where
  Here   ::                    Lang (a, h) a
  Before :: Lang h a        -> Lang (x, h) a
  Lambda :: Lang (a, h) b   -> Lang h (a -> b)
  Apply  :: Lang h (a -> b) -> (Lang h a -> Lang h b)

  Loop   :: Lang h (a -> a) -> Lang h a

  Int    :: Int             -> Lang h Int
  Up     :: Lang h Int      -> Lang h Int
  Down   :: Lang h Int      -> Lang h Int

  Add    :: Lang h Int -> Lang h Int -> Lang h Int
  Mult   :: Lang h Int -> Lang h Int -> Lang h Int
  Gte    :: Lang h Int -> Lang h Int -> Lang h Bool

  Bool   :: Bool            -> Lang h Bool
  Neg    :: Lang h Bool     -> Lang h Bool
  And    :: Lang h Bool -> Lang h Bool -> Lang h Bool
  Or     :: Lang h Bool -> Lang h Bool -> Lang h Bool

  If     :: Lang h Bool -> Lang h a -> Lang h a -> Lang h a

instance Language Lang where
  here = Here
  before = Before
  lambda = Lambda
  apply = Apply
  loop = Loop
  int = Int
  add = Add
  down = Down
  up = Up
  mult = Mult
  gte = Gte
  bool = Bool
  and = And
  or = Or
  neg = Neg
  ifte = If


interpret :: Term a -> a
interpret t = run t ()

fix :: (t -> t) -> t
fix f = f (fix f)

run :: Lang h a -> h -> a
run  Here         (head, _) = head
run (Before e   ) (_, ctxt) = run e ctxt
run (Lambda e   ) ctxt      = \a -> run e (a, ctxt)
run (Apply f arg) ctxt      = run f ctxt $ run arg ctxt
run (Loop e     ) ctxt      = fix (run e ctxt)
run (Int n      ) ctxt      = n
run (Up v       ) ctxt      = run v ctxt + 1
run (Down v     ) ctxt      = run v ctxt - 1
run (Add l r    ) ctxt      = on (+) (`run` ctxt) l r
run (Mult l r   ) ctxt      = on (*) (`run` ctxt) l r
run (Gte l r    ) ctxt      = on (>=) (`run` ctxt) l r
run (Bool b     ) ctxt      = b
run (Neg e      ) ctxt      = not $ run e ctxt
run (And l r    ) ctxt      = on (&&) (`run` ctxt) l r
run (Or l r     ) ctxt      = on (||) (`run` ctxt) l r
run (If b l r   ) ctxt      = if run b ctxt then run l ctxt else run r ctxt



main = hspec $ do
  describe "basic programs" $ do
    it "(\\ x . x) 3 is 3"
      $ interpret (apply (lambda here) (int 3)) `shouldBe` 3
    it "if True then 3 else 4 is 3"
      $ shouldBe
          (interpret (ifte (bool True) (int 3) (int 4)))
          3
  describe "factorial" $ do
    it "factorial 10 is 3628800"
    $ let eq0 :: Term (Int -> Bool)
          eq0 = apply ieq (int 0)
          ieq :: Term (Int -> Int -> Bool)
          ieq =
            lambda $
              lambda $
                and (gte here (before here))
                    (gte (before here) here)
          fact_ :: Term ((Int -> Int) -> (Int -> Int))
          fact_ =
            lambda $
              lambda $
                ifte (apply eq0 here)
                     (int 1)
                     (mult here
                           (apply (before here) (down here)))
          fact :: Term (Int -> Int)
          fact = loop fact_

      in interpret (apply fact (int 10))
         `shouldBe` 3628800