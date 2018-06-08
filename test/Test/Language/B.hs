module Test.Language.B where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.B
import           Language.B.Types

test_reduction :: TestTree
test_reduction = testGroup "簡約チェック"
  [ testCase "reduction" $ do
      let true  = BTrue
          false = BFalse
          t1    = BIf false false false
          t     = BIf true t1 true
      reduction Nothing t @?= t1
  ]

test_proof :: TestTree
test_proof = testGroup "証明"
  [ testCase "導出可能" $ do
      let true  = BTrue
          false = BFalse
          s = BIf true false false
          t = BIf s true true
          u = BIf false true true
          z1 = EvalRelation (BIf t false false, BIf u false false)
          z2 = EvalRelation (BIf BTrue BTrue BFalse, BTrue)
          z3 = EvalRelation (BIf BFalse BTrue BFalse, BFalse)

      step E_IFTRUE (step E_IF $ deduce E_IF z1) @?= Nothing
      deduce E_IFTRUE  z2 @?= Nothing
      deduce E_IFFALSE z3 @?= Nothing
  ]
