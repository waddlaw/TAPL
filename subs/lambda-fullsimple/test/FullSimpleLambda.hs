{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module FullSimpleLambda where

import           RIO

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.FullSimpleLambda
import           Language.FullSimpleLambda.Internal

mkNat :: Int -> Term
mkNat = foldr ($) TmZero . flip replicate TmSucc

evalN :: Int -> Term -> Term
evalN n term = foldr ($) term $ replicate n eval

test_type :: TestTree
test_type = testGroup "Types"
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

test_function :: TestTree
test_function = testGroup "Functions"
  [ testCase "isValue" $ do
      isValue (TmRecord []) @?= True
      isValue (TmRecord [("a", TmUnit)]) @?= True
      isValue (TmRecord [("a", TmUnit), ("b", TmUnit)]) @?= True
      isValue (TmRecord [("a", TmUnit), ("b", TmIf TmTrue TmFalse TmFalse)]) @?= False
  ]

test_unit :: TestTree
test_unit = testGroup "unit"
  [ testGroup "typecheck"
    [ testCase "unit:Unit" $
        typeof mempty TmUnit @?= TyUnit
    ]
  ]

test_pair :: TestTree
test_pair = testGroup "pair"
  [ testGroup "eval"
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
  [ testGroup "pretty"
    [ testCase "{partno=0,cost=true}" $ do
        let t = TmRecord [("partno", TmZero), ("cost", TmTrue)]
        prettyFullSimpleText mempty t @?= "{partno=0,cost=true}"
    ]
  , testGroup "eval"
    [ testCase "フィールドの全てが値" $ do
        let t = TmRecord [("partno", TmUnit), ("cost", TmUnit)]
        eval t @?= TmRecord [("partno", TmUnit),("cost", TmUnit)]
    , testCase "フィールドの1つ目が簡約可能" $ do
        let redex = TmIf TmTrue TmFalse TmFalse
            t = TmRecord [("partno", redex), ("cost", TmUnit)]
        eval t @?= TmRecord [("partno", TmFalse),("cost", TmUnit)]
    , testCase "フィールドの1つ目と2つ目が簡約可能" $ do
      let redex = TmIf TmTrue TmFalse TmFalse
          t = TmRecord [("partno", redex), ("cost", redex)]
      eval t @?= TmRecord [("partno", TmFalse),("cost", redex)]
    , testCase "フィールドの2つ目が簡約可能" $ do
      let redex = TmIf TmTrue TmFalse TmFalse
          t = TmRecord [("partno", TmUnit), ("cost", redex)]
      eval t @?= TmRecord [("partno", TmUnit),("cost", TmFalse)]
    ]
  , testGroup "typecheck"
    [ testCase "{x=5}:Nat" $ do
        let t = TmRecord [("x", mkNat 5)]
        typeof mempty t @?= TyRecord [("x", TyNat)]
    , testCase "{partno=5524,cost=true}" $ do
        let t = TmRecord [("partno", mkNat 5524), ("cost", TmTrue)]
        typeof mempty t @?= TyRecord [("partno", TyNat), ("cost", TyBool)]
    ]
  ]

test_pattern :: TestTree
test_pattern = do
  let p  = PtRecord [("partno", PtVar "x" 0), ("cost", PtVar "y" 1)]
      t1 = TmRecord [("partno", mkNat 1), ("cost", TmTrue)]
      t2 = TmVar 0
      t  = TmPattern p t1 t2
      ctx = mconcat ["x","y"]
  testGroup "pattern"
    [ testGroup "pretty"
      [ testCase "let x=() in ()" $ do
          let t' = TmPattern (PtVar "x" 0) TmUnit TmUnit
          prettyFullSimpleText "x" t' @?= "let x=() in ()"
      , testCase "let {partno=x,cost=y}={partno=1,cost=true} in x" $
          prettyFullSimpleText ctx t @?= "let {partno=x,cost=y}={partno=succ 0,cost=true} in x"
      ]
    , testGroup "eval"
      [ testCase "let {partno=x,cost=y}={partno=1,cost=true} in x" $
          eval t @?= mkNat 1
      ]
    , testGroup "typecheck"
      [ testCase "let {partno=x,cost=y}={partno=1,cost=true} in x" $
          typeof mempty t @?= TyNat
      ]
    ]
