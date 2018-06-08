module Test.Language.NB where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.NB
import           Language.NB.Types

test_nb :: TestTree
test_nb = testGroup "NB (unit test)"
  [ testCase "isNumericalVal" $ do
      isNumericalVal TmTrue @?= False
      isNumericalVal TmZero @?= True
      isNumericalVal (TmSucc $ TmSucc $ TmSucc TmZero) @?= True
  , testCase "isVal" $ do
      isVal TmTrue @?= True
      isVal TmZero @?= True
      isVal (TmSucc $ TmSucc $ TmSucc TmZero) @?= True
      isVal (TmPred TmZero) @?= False
  , testCase "eval" $ do
      eval TmTrue @?= TmTrue
      eval (TmIf TmTrue TmTrue TmFalse) @?= TmTrue
      eval (TmIsZero (TmPred (TmSucc (TmPred (TmSucc TmZero))))) @?= TmTrue
  ]
