{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Test.Language.UntypedLambda where

import           Prelude                           hiding (and, fst, head, id,
                                                    not, or, snd, tail)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.UntypedLambda
import qualified Language.UntypedLambda.Examples   as UL
import           Language.UntypedLambda.Lib.Base
import           Language.UntypedLambda.Lib.Bool
import           Language.UntypedLambda.Lib.Church
import           Language.UntypedLambda.Lib.List
import           Language.UntypedLambda.Lib.Pair
import           Language.Utils.Pretty

import           Data.Either

test_ul :: TestTree
test_ul = testGroup "UntypedLambda"
  [ testCase "pretty" $ do
      prettyText @UntypedLambda (TmVar "x") @?= "x"
      prettyText @UntypedLambda (TmLam "x" "x") @?= "λx. x"
      prettyText @UntypedLambda (TmApp "x" "y") @?= "x y"
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

      -- Bool
      runUlParser "tru"  @?= Right tru
      runUlParser "fls"  @?= Right fls
      runUlParser "test" @?= Right test
      runUlParser "and"  @?= Right and
      runUlParser "or"   @?= Right or
      runUlParser "not"  @?= Right not

      -- pair
      runUlParser "pair" @?= Right pair
      runUlParser "fst"  @?= Right fst
      runUlParser "snd"  @?= Right snd

      -- church
      runUlParser "c10"      @?= Right (c 10)
      runUlParser "c0"       @?= Right (c 0)
      runUlParser "c123"     @?= Right (c 123)
      runUlParser "scc"      @?= Right scc
      runUlParser "plus"     @?= Right plus
      runUlParser "times"    @?= Right times
      runUlParser "iszro"    @?= Right iszro
      runUlParser "prd"      @?= Right prd
      runUlParser "subtract" @?= Right subtract1
      runUlParser "equal"    @?= Right equal

      -- list
      runUlParser "nil"   @?= Right nil
      runUlParser "head"  @?= Right head
      runUlParser "isnil" @?= Right isnil
      runUlParser "cons"  @?= Right cons
      runUlParser "tail"  @?= Right tail
  , testCase "isClosed" $ do
      isClosed UL.example1 @?= False
      isClosed UL.example2 @?= True
      isClosed UL.example3 @?= True
      isClosed UL.example4 @?= False
      isClosed UL.example5 @?= True
  , testCase "evaluate (NormalOrder)" $ do
      reduceNormalOrder (TmApp (TmLam "x" "x") "y") @?= TmVar "y"
      reduceNormalOrder UL.example6 @?= TmApp (TmApp "u" "r") (TmLam "x" "x")
      reduceNormalOrder (TmApp (TmLam "x" (TmLam "y" (TmApp "x" "y"))) "z") @?= TmLam "y" (TmApp "z" "y")

      -- 評価戦略の共通の例
      reduceNormalOrder UL.example3 @?= TmApp id (TmLam "z" (TmApp id "z"))
      reduceNormalOrder (TmApp id (TmLam "z" (TmApp id "z"))) @?= TmLam "z" (TmApp id "z")
      reduceNormalOrder (TmLam "z" (TmApp id "z")) @?= TmLam "z" "z"
      reduceNormalOrder (TmLam "z" "z") @?= TmLam "z" "z"
  , testCase "evaluate (CallByName)" $ do
      reduceCallByName UL.example3 @?= TmApp id (TmLam "z" (TmApp id "z"))
      reduceCallByName (TmApp id (TmLam "z" (TmApp id "z"))) @?= TmLam "z" (TmApp id "z")
      reduceCallByName (TmLam "z" (TmApp id "z")) @?= TmLam "z" (TmApp id "z")
  , testCase "evaluate (CallByValue)" $ do
      reduceCallByValue UL.example3 @?= TmApp id (TmLam "z" (TmApp id "z"))
      reduceCallByValue (TmApp id (TmLam "z" (TmApp id "z"))) @?= TmLam "z" (TmApp id "z")
      reduceCallByValue (TmLam "z" (TmApp id "z")) @?= TmLam "z" (TmApp id "z")
  , testCase "evaluate" $ do
      eval NormalOrder UL.example3 @?= TmLam "z" "z"
      eval CallByName  UL.example3 @?= TmLam "z" (TmApp id "z")
      eval CallByValue UL.example3 @?= TmLam "z" (TmApp id "z")
  , testCase "subst" $ do
      subst "x" (TmLam "z" (TmApp "z" "w")) (TmLam "y" "x") @?= TmLam "y" (TmLam "z" (TmApp "z" "w"))
      subst "x" "y" (TmLam "x" "x") @?= TmLam "x" "x"
      subst "x" "z" (TmLam "z" "x") @?= TmLam "z" "x"
  , testCase "size" $ do
      size "x" @?= 1
      size (TmApp "x" "x") @?= 2
  ]
