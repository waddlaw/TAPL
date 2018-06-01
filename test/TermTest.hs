{-# LANGUAGE CPP #-}
module TermTest where

import qualified Term.HashSet
#if __GLASGOW_HASKELL__ == 822
import qualified Term.MonadSet
#endif
import qualified Term.Set

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import qualified Data.HashSet          as HashSet
import qualified Data.Set              as Set
#if __GLASGOW_HASKELL__ == 822
import qualified Data.Set.Monad        as MonadSet
#endif

test_size :: TestTree
test_size = testGroup "集合 s のサイズチェック"
  [ testCase "Set.size (s n)" $ do
      Set.size (Term.Set.s 0) @?= 0
      Set.size (Term.Set.s 1) @?= 3
      Set.size (Term.Set.s 2) @?= 39
      Set.size (Term.Set.s 3) @?= 59439
  , testCase "HashSet.size (s n)" $ do
      HashSet.size (Term.HashSet.s 0) @?= 0
      HashSet.size (Term.HashSet.s 1) @?= 3
      HashSet.size (Term.HashSet.s 2) @?= 39
      HashSet.size (Term.HashSet.s 3) @?= 59439
#if __GLASGOW_HASKELL__ == 822
  , testCase "MonadSet.size (s n)" $ do
      MonadSet.size (Term.MonadSet.s 0) @?= 0
      MonadSet.size (Term.MonadSet.s 1) @?= 3
      MonadSet.size (Term.MonadSet.s 2) @?= 39
      MonadSet.size (Term.MonadSet.s 3) @?= 59439
#endif
  ]

prop_inv01:: Term.Set.Term -> Property
prop_inv01 t = preCondition ==> Set.member t (Term.Set.s i)
  where
    preCondition = i < 4 -- 5 以上にすると時間がかかりすぎるため
    i = Term.Set.depth t
