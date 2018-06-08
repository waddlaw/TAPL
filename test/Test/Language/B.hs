module Test.Language.B where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.B
import           Language.B.Types

test_reduction :: TestTree
test_reduction = testGroup "簡約チェック"
  [ testCase "reduction" $ do
      let true  = TmTrue
          false = TmFalse
          t1    = TmIf false false false
          t     = TmIf true t1 true
      reduction Nothing t @?= t1
  ]

test_proof :: TestTree
test_proof = testGroup "証明"
  [ testCase "導出可能" $ do
      let true  = TmTrue
          false = TmFalse
          s = TmIf true false false
          t = TmIf s true true
          u = TmIf false true true
          z1 = EvalRelation (TmIf t false false, TmIf u false false)
          z2 = EvalRelation (TmIf TmTrue TmTrue TmFalse, TmTrue)
          z3 = EvalRelation (TmIf TmFalse TmTrue TmFalse, TmFalse)

      step E_IFTRUE (step E_IF $ deduce E_IF z1) @?= Nothing
      deduce E_IFTRUE  z2 @?= Nothing
      deduce E_IFFALSE z3 @?= Nothing
  ]
