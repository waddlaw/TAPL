{-# LANGUAGE OverloadedStrings #-}

module SystemF where

import Language.SystemF
import RIO
import Test.Tasty
import Test.Tasty.HUnit

-- test_sl :: TestTree
-- test_sl =
--   testGroup "SystemF"
--     [ testCase "pretty" $ do
--         prettySimpleText "x" (TmVar 0) @?= "x"
--         prettySimpleText mempty (TmLam "x" TyBool (TmVar 0)) @?= "λx:Bool. x"
--         prettySimpleText "b" (TmApp (TmLam "b" TyBool (TmVar 0)) TmTrue) @?= "(λb:Bool. b) true"
--         prettySimpleText mempty (TmIf TmTrue TmFalse TmFalse) @?= "if true then false else false",
--       testCase "parser" $ do
--         runSystemFParser "x" "x" @?= Right (TmVar 0)
--         runSystemFParser mempty "λx:Bool. x" @?= Right (TmLam "x" TyBool (TmVar 0))
--         runSystemFParser "b" "(λb:Bool. b) true" @?= Right (TmApp (TmLam "b" TyBool (TmVar 0)) TmTrue)
--         runSystemFParser mempty "if true then false else false" @?= Right (TmIf TmTrue TmFalse TmFalse)
--         runSystemFParser "not" "if true then (λx:Bool. x) else (λx:Bool. not x)" @?= Right (TmIf TmTrue (TmLam "x" TyBool (TmVar 0)) (TmLam "x" TyBool (TmApp (TmVar 1) (TmVar 0))))
--         runSystemFParser "f" "f (if false then true else false)" @?= Right (TmApp (TmVar 0) (TmIf TmFalse TmTrue TmFalse))
--         runSystemFParser "f" "λx:Bool. f (if (f x) then false else x)" @?= Right (TmLam "x" TyBool (TmApp (TmVar 1) (TmIf (TmApp (TmVar 1) (TmVar 0)) TmFalse (TmVar 0))))
--         runSystemFParser mempty "λx:Bool. λy:Bool. λz:Bool. x y z" @?= Right (TmLam "x" TyBool (TmLam "y" TyBool (TmLam "z" TyBool (TmApp (TmApp (TmVar 2) (TmVar 1)) (TmVar 0)))))
--         runSystemFParser mempty "λx:Bool. λy:Bool. λz:Bool. x (y z)" @?= Right (TmLam "x" TyBool (TmLam "y" TyBool (TmLam "z" TyBool (TmApp (TmVar 2) (TmApp (TmVar 1) (TmVar 0))))))
--       ]
-- , testCase "parser (そのうち直す" $ do
--     runSystemFParser "f" "λx:Bool. f (if f x then false else x)" @?= Left

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
        -- id = λX. λx:X. x
        let exp1 = TmTypeLam "X" . TmLam "x" (TyVar "X" 1) $ TmVar "x" 0
        prettySystemFText mempty exp1 @?= "λX. λx:X. x"

        -- id [Nat]
        let exp2 = TmTypeApp exp1 TyNat
        prettySystemFText mempty exp2 @?= "(λX. λx:X. x) [Nat]"

        -- id [Nat] 0
        let exp3 = TmApp exp2 TmZero
        prettySystemFText mempty exp3 @?= "((λX. λx:X. x) [Nat]) 0"

        -- double = λX. λf:X->X. λa:X. f (f a)
        let exp4 = TmTypeLam "X" . TmLam "f" (TyArr (TyVar "X" 1) (TyVar "X" 1)) . TmLam "a" (TyVar "X" 2) $ TmApp (TmVar "f" 1) (TmApp (TmVar "f" 1) (TmVar "a" 0))
        prettySystemFText mempty exp4 @?= "λX. λf:X->X. λa:X. f (f a)"

        -- doubleNat = double [Nat]
        let exp5 = TmTypeApp exp4 TyNat
        prettySystemFText mempty exp5 @?= "(λX. λf:X->X. λa:X. f (f a)) [Nat]"

        -- doubleNat = double [Nat->Nat]
        let exp6 = TmTypeApp exp4 (TyArr TyNat TyNat)
        prettySystemFText mempty exp6 @?= "(λX. λf:X->X. λa:X. f (f a)) [Nat->Nat]"

        -- double [Nat] (λx:Nat. succ(succ(x))) 3;
        let sub = TmLam "x" TyNat $ TmSucc $ TmSucc $ TmSucc (TmVar "x" 0)
            exp7 = TmApp (TmApp exp5 sub) (TmSucc (TmSucc (TmSucc TmZero)))
        prettySystemFText mempty exp7 @?= "(((λX. λf:X->X. λa:X. f (f a)) [Nat]) (λx:Nat. succ (succ (succ (x))))) (succ (succ (succ (0))))"

        -- selfApp = λx:∀X.X->X. x [∀X.X->X] x
        let exp8 = TmLam "x" (TyForAll "X" (TyArr (TyVar "X" 0) (TyVar "X" 0))) $ TmApp (TmTypeApp (TmVar "x" 0) (TyForAll "X" (TyArr (TyVar "X" 0) (TyVar "X" 0)))) (TmVar "x" 0)
        prettySystemFText mempty exp8 @?= "λx:∀X.X->X. (x [∀X.X->X]) x"

        -- quadruple = λX. double [X->X] (double [X])
        let exp9 = TmTypeLam "X" $ TmApp (TmTypeApp exp4 (TyArr (TyVar "X" 0) (TyVar "X" 0))) (TmTypeApp exp4 (TyVar "X" 0))
        prettySystemFText mempty exp9 @?= "λX. ((λX. λf:X->X. λa:X. f (f a)) [X->X]) ((λX. λf:X->X. λa:X. f (f a)) [X])"
    ]