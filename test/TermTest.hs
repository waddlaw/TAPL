module TermTest where

import           Term

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import qualified Data.Set              as Set

test_size :: TestTree
test_size = testGroup "集合 s のサイズチェック"
  [ testCase "Set.size (s n)" $ do
      Set.size (s 0) @?= 0
      Set.size (s 1) @?= 3
      Set.size (s 2) @?= 39
      Set.size (s 3) @?= 59439
  ]

prop_inv01:: Term -> Property
prop_inv01 t = preCondition ==> Set.member t (s i)
  where
    preCondition = i < 4 -- 5 以上にすると時間がかかりすぎるため
    i = depth t
