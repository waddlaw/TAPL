{-# LANGUAGE OverloadedStrings #-}

module SystemF where

import Language.SystemF
import Language.SystemF.Internal
import RIO hiding (evaluate)
import Test.Tasty
import Test.Tasty.HUnit

test_eval :: TestTree
test_eval =
  testGroup "eval"
    [ testCase "book examples (p.271)" $ do
        -- id [Nat]
        let exp1 = TmTypeApp identity TyNat
            res1 = TmLam "x" TyNat $ TmVar "x" 0
        evaluate exp1 @?= res1

        -- id [Nat] 0
        let exp2 = TmApp exp1 TmZero
            res2 = TmZero
        evaluate exp2 @?= res2

        evaluate doubleNat @?= (TmLam "f" (TyArr TyNat TyNat) . TmLam "a" TyNat $ TmApp (TmVar "f" 1) (TmApp (TmVar "f" 1) (TmVar "a" 0)))

        evaluate doubleNatArrowNat @?= (TmLam "f" (TyArr (TyArr TyNat TyNat) (TyArr TyNat TyNat)) . TmLam "a" (TyArr TyNat TyNat) $ TmApp (TmVar "f" 1) (TmApp (TmVar "f" 1) (TmVar "a" 0)))

        -- double [Nat] (λx:Nat . succ(succ(succ(x)))) 3
        let exp5_1 = TmApp doubleNat (TmLam "x" TyNat $ TmSucc $ TmSucc (TmVar "x" 0))
            exp5 = TmApp exp5_1 $ mkN 3
        evaluate exp5 @?= mkN 7

        let sub = (TmLam "x" TyNat $ TmSucc $ TmSucc (TmVar "x" 0))
            exp8 = TmApp (TmApp (TmTypeApp quadruple TyNat) sub) $ mkN 2
        evaluate exp8 @?= mkN 10

    ]

test_pretty :: TestTree
test_pretty =
  testGroup "pretty"
    [ testCase "term" $ do
        let ctx1 = toContext $ TermVarBind "x" TyBool
        prettySystemFText ctx1 (TmVar "x" 0) @?= "x"
        prettySystemFText mempty (TmLam "x" TyBool (TmVar "x" 0)) @?= "λx:Bool. x"

        let ctx2 = toContext $ TermVarBind "x" TyBool
        prettySystemFText ctx2 (TmApp (TmLam "b" TyBool (TmVar "b" 0)) TmTrue) @?= "(λb:Bool. b) true"
        prettySystemFText mempty (TmIf TmTrue TmFalse TmFalse) @?= "if true then false else false",

      testCase "book examples (p.271)" $ do
        prettySystemFText mempty identity @?= "λX. λx:X. x"

        -- id [Nat]
        let exp2 = TmTypeApp identity TyNat
        prettySystemFText mempty exp2 @?= "(λX. λx:X. x) [Nat]"

        -- id [Nat] 0
        let exp3 = TmApp exp2 TmZero
        prettySystemFText mempty exp3 @?= "((λX. λx:X. x) [Nat]) 0"
        prettySystemFText mempty double @?= "λX. λf:X->X. λa:X. f (f a)"
        prettySystemFText mempty doubleNat @?= "(λX. λf:X->X. λa:X. f (f a)) [Nat]"
        prettySystemFText mempty doubleNatArrowNat @?= "(λX. λf:X->X. λa:X. f (f a)) [Nat->Nat]"

        -- double [Nat] (λx:Nat. succ(succ(x))) 3;
        let sub = TmLam "x" TyNat $ TmSucc $ TmSucc (TmVar "x" 0)
            exp7 = TmApp (TmApp doubleNat sub) $ mkN 3
        prettySystemFText mempty exp7 @?= "(((λX. λf:X->X. λa:X. f (f a)) [Nat]) (λx:Nat. succ (succ (x)))) (succ (succ (succ (0))))"

        prettySystemFText mempty selfApp @?= "λx:∀X.X->X. (x [∀X.X->X]) x"
        prettySystemFText mempty quadruple @?= "λX. ((λX. λf:X->X. λa:X. f (f a)) [X->X]) ((λX. λf:X->X. λa:X. f (f a)) [X])"
    ]