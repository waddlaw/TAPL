{-# LANGUAGE OverloadedStrings #-}

module SystemF where

import Language.SystemF
import Language.SystemF.Internal
import RIO hiding (evaluate)
import Test.Tasty
import Test.Tasty.HUnit

test_eval :: TestTree
test_eval =
  testGroup
    "eval"
    [ testCase "p.271: id [Nat]" $ do
        let expr = TmTypeApp identity TyNat
            expected = TmLam "x" TyNat $ TmVar "x" 0
        evaluate expr @?= expected,
      testCase "p.271: id [Nat] 0" $ do
        let expr = TmApp (TmTypeApp identity TyNat) TmZero
            expected = TmZero
        evaluate expr @?= expected,
      testCase "p.271: doubleNat = double [Nat]" $ do
        let expected =
              TmLam "f" (TyArr TyNat TyNat)
                . TmLam "a" TyNat
                $ TmApp (TmVar "f" 1) (TmApp (TmVar "f" 1) (TmVar "a" 0))
        evaluate doubleNat @?= expected,
      testCase "p.271: doubleNat = double [Nat -> Nat]" $ do
        let expected =
              TmLam "f" (TyArr (TyArr TyNat TyNat) (TyArr TyNat TyNat))
                . TmLam "a" (TyArr TyNat TyNat)
                $ TmApp (TmVar "f" 1) (TmApp (TmVar "f" 1) (TmVar "a" 0))
        evaluate doubleNatArrowNat @?= expected,
      testCase "p.271: double [Nat] (λx:Nat . succ(succ(x))) 3" $ do
        let expr1 = TmApp doubleNat (TmLam "x" TyNat $ TmSucc $ TmSucc (TmVar "x" 0))
            expr2 = TmApp expr1 $ mkN 3
        evaluate expr2 @?= mkN 7,
      testCase "p.271: quadruple [Nat] (λx:Nat . succ(succ(x))) 2" $ do
        let sub = TmLam "x" TyNat $ TmSucc $ TmSucc (TmVar "x" 0)
            expr = TmApp (TmApp (TmTypeApp quadruple TyNat) sub) $ mkN 2
        evaluate expr @?= mkN 10,
      testCase "[E-HEADCONS] head [Nat] (cons [Nat] 0 nil)" $ do
        let sub = TmApp (TmApp (TmTypeApp TmCons TyNat) TmZero) TmNil
            expr = TmApp (TmTypeApp TmHead TyNat) sub
        evaluate expr @?= TmZero
    ]

test_typeof :: TestTree
test_typeof =
  testGroup
    "typeof"
    [ testCase "p.272: l = cons [Nat] 4 (cons [Nat] 3 (cons [Nat] 2 (nil [Nat])))" $ do
        let sub1 = TmTypeApp TmNil TyNat
            sub2 = TmApp (TmApp (TmTypeApp TmCons TyNat) (mkN 2)) sub1
            sub3 = TmApp (TmApp (TmTypeApp TmCons TyNat) (mkN 3)) sub2
            expr = TmApp (TmApp (TmTypeApp TmCons TyNat) (mkN 4)) sub3
        typeof mempty expr @?= TyList TyNat

      -- , testCase "p.272: map'" $ do
      --     let expected = TyForAll "X" $ TyForAll "Y" $ TyArr (TyArr (TyVar "X" 1) (TyVar "Y" 0)) (TyArr (TyList (TyVar "X" 1)) (TyList (TyVar "Y" 0)))
      --     typeof mempty map' @?= expected
    ]

test_pretty :: TestTree
test_pretty =
  testGroup
    "pretty"
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
