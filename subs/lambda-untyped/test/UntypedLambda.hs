{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module UntypedLambda where

import Data.Either
import Language.Core.Pretty
import Language.Core.Types
import Language.UntypedLambda
import qualified Language.UntypedLambda.Examples as UL
import Language.UntypedLambda.Lib.Base
import Language.UntypedLambda.Lib.Bool
import Language.UntypedLambda.Lib.Church
import Language.UntypedLambda.Lib.List
import Language.UntypedLambda.Lib.Pair
import Test.Tasty
import Test.Tasty.HUnit
import Prelude hiding (and, fst, head, id, not, or, snd, tail)

test_ul :: TestTree
test_ul =
  testGroup "UntypedLambda"
    [ testCase "pretty" $ do
        prettyText @UntypedLambda (TmVar "x") @?= "x"
        prettyText @UntypedLambda (TmLam "x" "x") @?= "λx. x"
        prettyText @UntypedLambda (TmApp "x" "y") @?= "x y",
      testCase "parser" $ do
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
        -- Bool
        runUlParser "tru" @?= Right tru
        runUlParser "fls" @?= Right fls
        runUlParser "test" @?= Right test
        runUlParser "and" @?= Right and
        runUlParser "or" @?= Right or
        runUlParser "not" @?= Right not
        -- pair
        runUlParser "pair" @?= Right pair
        runUlParser "fst" @?= Right fst
        runUlParser "snd" @?= Right snd
        -- church
        runUlParser "c10" @?= Right (c 10)
        runUlParser "c0" @?= Right (c 0)
        runUlParser "c123" @?= Right (c 123)
        runUlParser "scc" @?= Right scc
        runUlParser "plus" @?= Right plus
        runUlParser "times" @?= Right times
        runUlParser "iszro" @?= Right iszro
        runUlParser "prd" @?= Right prd
        runUlParser "subtract" @?= Right subtract1
        runUlParser "equal" @?= Right equal
        -- list
        runUlParser "nil" @?= Right nil
        runUlParser "head" @?= Right head
        runUlParser "isnil" @?= Right isnil
        runUlParser "cons" @?= Right cons
        runUlParser "tail" @?= Right tail,
      testCase "isClosed" $ do
        isClosed UL.example1 @?= False
        isClosed UL.example2 @?= True
        isClosed UL.example3 @?= True
        isClosed UL.example4 @?= False
        isClosed UL.example5 @?= True,
      testCase "evaluate (NormalOrder)" $ do
        reduceNormalOrder (TmApp (TmLam "x" "x") "y") @?= TmVar "y"
        reduceNormalOrder UL.example6 @?= TmApp (TmApp "u" "r") (TmLam "x" "x")
        reduceNormalOrder (TmApp (TmLam "x" (TmLam "y" (TmApp "x" "y"))) "z") @?= TmLam "y" (TmApp "z" "y")
        -- 評価戦略の共通の例
        reduceNormalOrder UL.example3 @?= TmApp id (TmLam "z" (TmApp id "z"))
        reduceNormalOrder (TmApp id (TmLam "z" (TmApp id "z"))) @?= TmLam "z" (TmApp id "z")
        reduceNormalOrder (TmLam "z" (TmApp id "z")) @?= TmLam "z" "z"
        reduceNormalOrder (TmLam "z" "z") @?= TmLam "z" "z",
      testCase "evaluate (CallByName)" $ do
        reduceCallByName UL.example3 @?= TmApp id (TmLam "z" (TmApp id "z"))
        reduceCallByName (TmApp id (TmLam "z" (TmApp id "z"))) @?= TmLam "z" (TmApp id "z")
        reduceCallByName (TmLam "z" (TmApp id "z")) @?= TmLam "z" (TmApp id "z"),
      testCase "evaluate (CallByValue)" $ do
        reduceCallByValue UL.example3 @?= TmApp id (TmLam "z" (TmApp id "z"))
        reduceCallByValue (TmApp id (TmLam "z" (TmApp id "z"))) @?= TmLam "z" (TmApp id "z")
        reduceCallByValue (TmLam "z" (TmApp id "z")) @?= TmLam "z" (TmApp id "z"),
      testCase "evaluate" $ do
        eval NormalOrder UL.example3 @?= TmLam "z" "z"
        eval CallByName UL.example3 @?= TmLam "z" (TmApp id "z")
        eval CallByValue UL.example3 @?= TmLam "z" (TmApp id "z"),
      testCase "subst" $ do
        subst "x" (TmLam "z" (TmApp "z" "w")) (TmLam "y" "x") @?= TmLam "y" (TmLam "z" (TmApp "z" "w"))
        subst "x" "y" (TmLam "x" "x") @?= TmLam "x" "x"
        subst "x" "z" (TmLam "z" "x") @?= TmLam "z" "x",
      testCase "size" $ do
        size "x" @?= 1
        size (TmApp "x" "x") @?= 2,
      testCase "removenames" $ do
        -- 演習6.1.1
        removenames [] (c 0) @?= NlTmLam (NlTmLam "0")
        removenames [] (c 2) @?= NlTmLam (NlTmLam (NlTmApp "1" (NlTmApp "1" "0")))
        removenames [] plus @?= NlTmLam (NlTmLam (NlTmLam (NlTmLam (NlTmApp (NlTmApp "3" "1") (NlTmApp (NlTmApp "2" "1") "0")))))
        let t = NlTmLam $ NlTmApp "1" $ NlTmLam $ NlTmApp (NlTmApp "1" "1") "0"
        removenames [] fix @?= NlTmLam (NlTmApp t t)
        let foo = TmApp (TmLam "x" $ TmLam "x" "x") (TmLam "x" "x")
        removenames [] foo @?= NlTmApp (NlTmLam $ NlTmLam "0") (NlTmLam "0"),
      testCase "restorenames" $ do
        restorenames [] (NlTmLam (NlTmLam "0")) @?= TmLam "a0" (TmLam "a1" "a1")
        restorenames [] (NlTmLam (NlTmLam (NlTmApp "1" (NlTmApp "1" "0")))) @?= TmLam "a0" (TmLam "a1" (TmApp "a0" (TmApp "a0" "a1")))
        restorenames [] (NlTmLam (NlTmLam (NlTmLam (NlTmLam (NlTmApp (NlTmApp "3" "1") (NlTmApp (NlTmApp "2" "1") "0")))))) @?= TmLam "a0" (TmLam "a1" (TmLam "a2" (TmLam "a3" $ TmApp (TmApp "a0" "a2") (TmApp (TmApp "a1" "a2") "a3"))))
        let t = NlTmLam $ NlTmApp "1" $ NlTmLam $ NlTmApp (NlTmApp "1" "1") "0"
            t' = TmLam "a1" $ TmApp "a0" $ TmLam "a2" $ TmApp (TmApp "a1" "a1") "a2"
        restorenames [] (NlTmLam (NlTmApp t t)) @?= TmLam "a0" (TmApp t' t')
        restorenames [] (NlTmApp (NlTmLam $ NlTmLam "0") (NlTmLam "0")) @?= TmApp (TmLam "a0" $ TmLam "a1" "a1") (TmLam "a0" "a0"),
      testCase "shift" $ do
        shift 0 2 (NlTmLam $ NlTmLam $ NlTmApp "1" (NlTmApp "0" "2")) @?= NlTmLam (NlTmLam $ NlTmApp "1" (NlTmApp "0" "4"))
        shift 0 2 (NlTmLam $ NlTmApp (NlTmApp "0" "1") (NlTmLam $ NlTmApp (NlTmApp "0" "1") "2")) @?= NlTmLam (NlTmApp (NlTmApp "0" "3") (NlTmLam $ NlTmApp (NlTmApp "0" "1") "4")),
      testCase "namelessSubst" $ do
        -- 演習6.2.5
        let g = ["b", "a"]
        let t1 = TmApp "b" (TmLam "x" $ TmLam "y" "b")
            k1 = getNlTermVar $ removenames g "b"
            s1 = removenames g "a"
            nt1 = removenames g t1
        namelessSubst k1 s1 nt1 @?= NlTmApp "1" (NlTmLam $ NlTmLam "3")
        let t2 = TmApp "b" (TmLam "x" "b")
            k2 = getNlTermVar $ removenames g "b"
            s2 = removenames g $ TmApp "a" (TmLam "z" "a")
            nt2 = removenames g t2
        namelessSubst k2 s2 nt2 @?= NlTmApp (NlTmApp "1" (NlTmLam "2")) (NlTmLam $ NlTmApp "2" (NlTmLam "3"))
        let t3 = TmLam "b" (TmApp "b" "a")
            k3 = getNlTermVar $ removenames g "b"
            s3 = removenames g "a"
            nt3 = removenames g t3
        namelessSubst k3 s3 nt3 @?= NlTmLam (NlTmApp "0" "2")
        let t4 = TmLam "a" (TmApp "b" "a")
            k4 = getNlTermVar $ removenames g "b"
            s4 = removenames g "a"
            nt4 = removenames g t4
        namelessSubst k4 s4 nt4 @?= NlTmLam (NlTmApp "2" "0"),
      testCase "reduceNameless"
        $ reduceNameless (NlTmApp (NlTmLam $ NlTmApp (NlTmApp "1" "0") "2") (NlTmLam "0"))
          @?= NlTmApp (NlTmApp "0" (NlTmLam "0")) "1"
      ]
