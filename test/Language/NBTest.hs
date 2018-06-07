module Language.NBTest where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.NB
import           Language.NB.Types

ttrue, tzero, tsucc, tpred :: Term
ttrue = TmTrue
tzero = TmZero
tsucc = TmSucc $ TmSucc $ TmSucc TmZero
tpred = TmPred TmZero

test_nb :: TestTree
test_nb = testGroup "NB"
  [ testCase "isNumericalVal" $ do
      isNumericalVal ttrue @?= False
      isNumericalVal tzero @?= True
      isNumericalVal tsucc @?= True
  , testCase "isVal" $ do
      isVal ttrue @?= True
      isVal tzero @?= True
      isVal tsucc @?= True
      isVal tpred @?= False
  , testCase "eval" $ do
      eval ttrue @?= TmTrue
      eval (TmIf TmTrue TmTrue TmFalse) @?= TmTrue
      eval (TmIsZero (TmPred (TmSucc (TmPred (TmSucc TmZero))))) @?= TmTrue
  ]
