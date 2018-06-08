{-# LANGUAGE OverloadedStrings #-}
module Test.Language.UntypedLambda where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.UntypedLambda
import           Language.Utils.Pretty

import Data.Either

test_ul :: TestTree
test_ul = testGroup "UntypedLambda"
  [ testCase "pretty" $ do
      prettyText (TmVar "x") @?= "x"
      prettyText (TmLam "x" (TmVar "x")) @?= "λx. x"
      prettyText (TmApp (TmVar "x") (TmVar "y")) @?= "x y"
  , testCase "parser" $ do
      runUlParser "x" @?= Right (TmVar "x")
      runUlParser "x1y" @?= Right (TmVar "x1y")
      isLeft (runUlParser "Xyz") @?= True
      isLeft (runUlParser "123x") @?= True
      runUlParser "λx. t" @?= Right (TmLam "x" (TmVar "t"))
      isLeft (runUlParser "λ. Ab") @?= True
  ]
