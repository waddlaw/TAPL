module Test.Language.UntypedLambda.Lib.Int where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.UntypedLambda
import           Language.UntypedLambda.Lib.Int
import           Language.UntypedLambda.Prelude

test_ul :: TestTree
test_ul = testGroup "UntypedLambda.Lib.Int"
  [ testCase "int" $ do
      eval CallByValue (int 0)    @?= eval CallByValue (mkPair tru (c 0))
      eval CallByValue (int (-3)) @?= eval CallByValue (mkPair fls (c 3))
      eval CallByValue (int 3)    @?= eval CallByValue (mkPair tru (c 3))
  , testCase "succI" $
      eval CallByValue (succI @@ (int 0)) @?= eval CallByValue (int 1)
      -- 振る舞い等価
      -- eval CallByValue (TmApp succI (int (-1))) @?= eval CallByValue (int 0)
      -- eval CallByValue (TmApp succI (int 1))    @?= eval CallByValue (int 2)
  , testCase "succNI" $
      eval CallByValue (succNI @@ mkPair (c 1) (int 0)) @?= eval CallByValue (int 1)
      -- 振る舞い等価
      -- eval CallByValue (TmApp (TmApp plusI (int 1)) (int 1)) @?= eval CallByValue (int 2)
  ]