{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
module TaglessTypeless where

import Prelude hiding (and, or)

import Control.Applicative
import Control.Category ((>>>))


import Test.Hspec hiding (before)
import Control.Monad.Fix (fix)

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

instance Language (->) where
  here = fst
  before = (>>>) snd
  lambda = flip . curry
  apply = (<*>)
  loop = (.) fix
  int = const
  add = liftA2 (+)
  down = fmap pred
  up = fmap succ
  mult = liftA2 (*)
  gte = liftA2 (>=)
  bool = const
  and = liftA2 (&&)
  or = liftA2 (||)
  neg = fmap not
  ifte = liftA3 $ \b l r -> if b then l else r


interpret :: Term a -> a
interpret t = run t ()

run :: (h -> a) -> h -> a
run = ($)

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