{-# LANGUAGE OverloadedStrings #-}
module Test.Language.UntypedLambda where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.UntypedLambda
import qualified Language.UntypedLambda.Examples as UL
import           Language.Utils.Pretty

import           Data.Either

test_ul :: TestTree
test_ul = testGroup "UntypedLambda"
  [ testCase "pretty" $ do
      prettyText (TmVar "x") @?= "x"
      prettyText (TmLam "x" "x") @?= "λx. x"
      prettyText (TmApp "x" "y") @?= "x y"
  , testCase "parser" $ do
      runUlParser "x" @?= Right "x"
      runUlParser "x1y" @?= Right "x1y"
      isLeft (runUlParser "Xyz") @?= True
      isLeft (runUlParser "123x") @?= True
      runUlParser "λx. t" @?= Right (TmLam "x" "t")
      isLeft (runUlParser "λ. Ab") @?= True
      runUlParser "s t u" @?= Right UL.example1
      runUlParser "s t u" @?= runUlParser "(s t) u"
      runUlParser "λx. λy. x y x" @?= Right UL.example2
      runUlParser "λx. λy. x y x" @?= runUlParser "λx. (λy. ((x y) x))"
      runUlParser "(λx.x) ((λx.x) (λz.(λx.x) z))" @?= Right UL.example3
  , testCase "isClosed" $ do
      isClosed UL.example1 @?= False
      isClosed UL.example2 @?= True
      isClosed UL.example3 @?= True
      isClosed UL.example4 @?= False
      isClosed UL.example5 @?= True
  , testCase "β-reduction" $ do
      reduceBeta (TmApp (TmLam "x" "x") "y") @?= TmVar "y"
      reduceBeta UL.example6 @?= TmApp (TmApp "u" "r") (TmLam "x" "x")
      reduceBeta (TmApp (TmLam "x" (TmLam "y" (TmApp "x" "y"))) "z") @?= TmLam "y" (TmApp "z" "y")
  ]
