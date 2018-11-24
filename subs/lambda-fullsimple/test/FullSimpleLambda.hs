{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module FullSimpleLambda where

import           RIO

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.FullSimpleLambda

test_sl :: TestTree
test_sl = testGroup "FullSimpleLambda"
  [ testCase "pretty" $ do
      prettyFullSimpleText "x" (TmVar 0) @?= "x"
      prettyFullSimpleText mempty (TmLam "x" TyBool (TmVar 0)) @?= "λx:Bool. x"
      prettyFullSimpleText "b" (TmApp (TmLam "b" TyBool (TmVar 0)) TmTrue) @?= "(λb:Bool. b) true"
      prettyFullSimpleText mempty (TmIf TmTrue TmFalse TmFalse) @?= "if true then false else false"
  , testCase "parser" $ do
      runFullSimpleLambdaParser "x" "x" @?= Right (TmVar 0)
      runFullSimpleLambdaParser mempty "λx:Bool. x" @?= Right (TmLam "x" TyBool (TmVar 0))
      runFullSimpleLambdaParser "b" "(λb:Bool. b) true" @?= Right (TmApp (TmLam "b" TyBool (TmVar 0)) TmTrue)
      runFullSimpleLambdaParser mempty "if true then false else false" @?= Right (TmIf TmTrue TmFalse TmFalse)
      runFullSimpleLambdaParser "not" "if true then (λx:Bool. x) else (λx:Bool. not x)" @?= Right (TmIf TmTrue (TmLam "x" TyBool (TmVar 0)) (TmLam "x" TyBool (TmApp (TmVar 1) (TmVar 0))))
      runFullSimpleLambdaParser "f" "f (if false then true else false)" @?= Right (TmApp (TmVar 0) (TmIf TmFalse TmTrue TmFalse))
      runFullSimpleLambdaParser "f" "λx:Bool. f (if (f x) then false else x)" @?= Right (TmLam "x" TyBool (TmApp (TmVar 1) (TmIf (TmApp (TmVar 1) (TmVar 0)) TmFalse (TmVar 0))))
      runFullSimpleLambdaParser mempty "λx:Bool. λy:Bool. λz:Bool. x y z" @?= Right (TmLam "x" TyBool (TmLam "y" TyBool (TmLam "z" TyBool (TmApp (TmApp (TmVar 2) (TmVar 1)) (TmVar 0)))))
      runFullSimpleLambdaParser mempty "λx:Bool. λy:Bool. λz:Bool. x (y z)" @?= Right (TmLam "x" TyBool (TmLam "y" TyBool (TmLam "z" TyBool (TmApp (TmVar 2) (TmApp (TmVar 1) (TmVar 0))))))
  -- , testCase "parser (そのうち直す" $ do
  --     runFullSimpleLambdaParser "f" "λx:Bool. f (if f x then false else x)" @?= Left
  , testCase "unit" $
      typeof "" TmUnit @?= TyUnit
  ]
