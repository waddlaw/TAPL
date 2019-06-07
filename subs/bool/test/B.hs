module B where

import RIO

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.B
import qualified Language.B.Example as B

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
      let z1 = B.example
          z2 = EvalRelation (TmIf TmTrue TmTrue TmFalse, TmTrue)
          z3 = EvalRelation (TmIf TmFalse TmTrue TmFalse, TmFalse)

      step E_IFTRUE (step E_IF $ deduce E_IF z1) @?= Nothing
      deduce E_IFTRUE  z2 @?= Nothing
      deduce E_IFFALSE z3 @?= Nothing
  ]
