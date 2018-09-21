{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Language.SimpleLambda where

import           RIO

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.SimpleLambda

test_sl :: TestTree
test_sl = testGroup "SimpleLambda"
  [ testCase "pretty" $ do
      prettySimpleText ["x"] (TmVar 0) @?= "x"
      prettySimpleText [] (TmLam "x" TyBool (TmVar 0)) @?= "λx:Bool. x"
      prettySimpleText ["b"] (TmApp (TmLam "b" TyBool (TmVar 0)) TmTrue) @?= "(λb:Bool. b) true"
      prettySimpleText [] (TmIf TmTrue TmFalse TmFalse) @?= "if true then false else false"
  ]
