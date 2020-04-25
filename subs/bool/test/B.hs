module B where

import Language.B
import qualified Language.B.Example as B
import RIO
import Test.Tasty
import Test.Tasty.HUnit

test_reduction :: TestTree
test_reduction =
  testGroup
    "Check reduction"
    [ testCase "reduction" $ do
        let t1 = TmIf TmFalse TmTrue TmFalse
            t = TmIf TmTrue t1 TmTrue
        reduction Nothing t @?= t1
    ]

test_proof :: TestTree
test_proof =
  testGroup
    "Proob"
    [ testCase "derivable" $ do
        let z1 = B.example
            z2 = EvalRelation (TmIf TmTrue TmTrue TmFalse, TmTrue)
            z3 = EvalRelation (TmIf TmFalse TmTrue TmFalse, TmFalse)
        step E_IFTRUE (step E_IF $ deduce E_IF z1) @?= Nothing
        deduce E_IFTRUE z2 @?= Nothing
        deduce E_IFFALSE z3 @?= Nothing
    ]
