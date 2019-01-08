{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module FullSimpleLambda where

import           RIO

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.FullSimpleLambda

mkNat :: Int -> Term
mkNat = foldr ($) TmZero . flip replicate TmSucc

evalN :: Int -> Term -> Term
evalN n term = foldr ($) term $ replicate n eval

test_sl :: TestTree
test_sl = testGroup "Basic Type"
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
  ]

test_unit :: TestTree
test_unit = testGroup "unit"
  [ testGroup "typecheck" $
    [ testCase "unit:Unit" $
        typeof mempty TmUnit @?= TyUnit
    ]
  ]

test_pair :: TestTree
test_pair = testGroup "pair"
  [ testGroup "eval" $
    [ testCase "{pred 4, if true then false else false}.1" $ do
        let n4 = mkNat 4
            tl = TmPred n4
            tr = TmIf TmTrue TmFalse TmFalse
            tp = TmPair tl tr
            t  = TmPairFst tp
        evalN 1 t @?= TmPairFst (TmPair (mkNat 3) tr)
        evalN 2 t @?= TmPairFst (TmPair (mkNat 3) TmFalse)
        evalN 3 t @?= mkNat 3
    , testCase "(λx:Nat*Nat. x.2) {pred 4, pred5}" $ do
        let n3   = mkNat 3
            n4   = mkNat 4
            n5   = mkNat 5
            ty   = TyProd TyNat TyNat
            tlam = TmLam "x" ty (TmPairSnd (TmVar 0))
            tp2  = TmPair (TmPred n4) (TmPred n5)
            t    = TmApp tlam tp2
        evalN 1 t @?= TmApp tlam (TmPair n3 (TmPred n5))
        evalN 2 t @?= TmApp tlam (TmPair n3 n4)
        evalN 3 t @?= TmPairSnd (TmPair n3 n4)
        evalN 4 t @?= n4
    ]
  ]

test_record :: TestTree
test_record = testGroup "record"
  [ testGroup "typecheck" $
    [ testCase "{x=5}:Nat" $ do
        let t = TmRecord [("x", mkNat 5)]
        typeof mempty t @?= TyRecord [("x", TyNat)]
    , testCase "{partno=5524,cost=true}" $ do
        let t = TmRecord [("partno", mkNat 5524), ("cost", TmTrue)]
        typeof mempty t @?= TyRecord [("partno", TyNat), ("cost", TyBool)]
    ]
  ]
