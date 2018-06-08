{-# LANGUAGE OverloadedStrings #-}
module Test.Language.UntypedLambda where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.UntypedLambda
import           Language.Utils.Pretty

test_ul :: TestTree
test_ul = testGroup "UntypedLambda"
  [ testCase "pretty" $ do
      prettyText (TmVar "x") @?= "x"
      prettyText (TmLam "x" (TmVar "x")) @?= "λx. x"
      prettyText (TmApp (TmVar "x") (TmVar "y")) @?= "x y"
  ]
