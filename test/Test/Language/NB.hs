module Test.Language.NB where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.NB

test_nb :: TestTree
test_nb = testGroup "NB"
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

test_nb_parser :: TestTree
test_nb_parser = testGroup "NB.Parser"
  [ testCase "runNbParser" $ do
      runNbParser "true" @?= Right TmTrue
      runNbParser "succ (succ (succ 0))" @?= Right (TmSucc (TmSucc (TmSucc TmZero)))
  ]