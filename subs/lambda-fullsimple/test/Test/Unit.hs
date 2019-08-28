module Test.Unit where

import Language.FullSimpleLambda.System.Unit
import RIO
import Test.Tasty
import Test.Tasty.HUnit

test_unit :: TestTree
test_unit =
  testGroup "unit"
    [ testGroup "eval" [],
      testGroup "typeof"
        [ testCase "unit:Unit"
            $ typeof CtxEmpty TmUnit
            @?= TyUnit
          ]
      ]
