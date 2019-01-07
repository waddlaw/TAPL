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
  , testCase "pair" $ do
      let n4 = TmSucc (TmSucc (TmSucc (TmSucc TmZero)))
          tl = TmPred n4
          tr = TmIf TmTrue TmFalse TmFalse
          tp = TmPair tl tr
          t  = TmPairFst tp
      eval t @?= TmPairFst (TmPair (TmSucc (TmSucc (TmSucc TmZero))) tr)
      eval (eval t) @?= TmPairFst (TmPair (TmSucc (TmSucc (TmSucc TmZero))) TmFalse)
      eval (eval (eval t)) @?= TmSucc (TmSucc (TmSucc TmZero))

      -- case2
      let n3   =TmSucc (TmSucc (TmSucc TmZero))
          n5   = TmSucc n4
          ty   = TyProd TyNat TyNat
          tlam = TmLam "x" ty (TmPairSnd (TmVar 0))
          tp2  = TmPair (TmPred n4) (TmPred n5)
          t2   = TmApp tlam tp2
      eval t2 @?= TmApp tlam (TmPair n3 (TmPred n5))
      eval (eval t2) @?= TmApp tlam (TmPair n3 n4)
      -- eval (eval (eval t2)) @?= TmPairSnd (TmPair n3 n4) -- subst が必要
      -- eval (eval (eval (eval t2))) @?= n4 -- subst が必要
  ]
