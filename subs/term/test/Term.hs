module Term where

import RIO
import qualified RIO.HashSet as HS
import qualified RIO.Set as Set
import qualified Term.HashSet
import qualified Term.Set
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

test_size :: TestTree
test_size =
  testGroup "集合 s のサイズチェック"
    [ testCase "Set.size (s n)" $ do
        Set.size (Term.Set.s 0) @?= 0
        Set.size (Term.Set.s 1) @?= 3
        Set.size (Term.Set.s 2) @?= 39
        Set.size (Term.Set.s 3) @?= 59439
    , testCase "Hs.size (s n)" $ do
      HS.size (Term.HashSet.s 0) @?= 0
      HS.size (Term.HashSet.s 1) @?= 3
      HS.size (Term.HashSet.s 2) @?= 39
      HS.size (Term.HashSet.s 3) @?= 59439
    ]

prop_inv01 :: Term.Set.Term -> Property
prop_inv01 t = preCondition ==> Set.member t (Term.Set.s i)
  where
    preCondition = i < 4 -- 5 以上にすると時間がかかりすぎるため
    i = Term.Set.depth t
