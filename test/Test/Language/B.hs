module Test.Language.B where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.B
import           Language.B.Types

test_reduction :: TestTree
test_reduction = testGroup "簡約チェック"
  [ testCase "reduction" $ do
      let t1 = TmIf TmFalse TmTrue TmFalse
          t  = TmIf TmTrue t1 TmTrue
      reduction Nothing t @?= t1
  ]

test_proof :: TestTree
test_proof = testGroup "証明"
  [ testCase "導出可能" $ do
      let s  = TmIf TmTrue TmFalse TmFalse
          t  = TmIf s TmTrue TmTrue
          u  = TmIf TmFalse TmTrue TmTrue
          z1 = EvalRelation (TmIf t TmFalse TmFalse, TmIf u TmFalse TmFalse)
          z2 = EvalRelation (TmIf TmTrue TmTrue TmFalse, TmTrue)
          z3 = EvalRelation (TmIf TmFalse TmTrue TmFalse, TmFalse)

      step E_IFTRUE (step E_IF $ deduce E_IF z1) @?= Nothing
      deduce E_IFTRUE  z2 @?= Nothing
      deduce E_IFFALSE z3 @?= Nothing
  ]
