{-# LANGUAGE OverloadedStrings #-}
module Test.Language.UntypedLambda where

import           Prelude                         hiding (id, not, or)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.UntypedLambda
import qualified Language.UntypedLambda.Examples as UL
import           Language.UntypedLambda.Prelude
import           Language.Utils.Pretty

import           Data.Either

test_ul :: TestTree
test_ul = testGroup "UntypedLambda"
  [ testCase "pretty" $ do
      prettyText (TmVar "x") @?= "x"
      prettyText (TmLam "x" "x") @?= "\\x. x"
      prettyText (TmApp "x" "y") @?= "x y"
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
  , testCase "Church ブール値" $ do
      eval NormalOrder UL.example7 @?= tru
      eval CallByName  UL.example7 @?= tru
      eval CallByValue UL.example7 @?= tru

      -- and
      eval NormalOrder UL.example8 @?= tru
      eval CallByName  UL.example8 @?= tru
      eval CallByValue UL.example8 @?= tru
      eval NormalOrder UL.example9 @?= fls
      eval CallByName  UL.example9 @?= fls
      eval CallByValue UL.example9 @?= fls

      -- or
      eval NormalOrder (TmApp (TmApp or tru) fls) @?= tru
      eval CallByName  (TmApp (TmApp or tru) fls) @?= tru
      eval CallByValue (TmApp (TmApp or tru) fls) @?= tru
      eval NormalOrder (TmApp (TmApp or fls) fls) @?= fls
      eval CallByName  (TmApp (TmApp or fls) fls) @?= fls
      eval CallByValue (TmApp (TmApp or fls) fls) @?= fls

      -- not
      eval NormalOrder (TmApp not fls) @?= tru
      eval CallByName  (TmApp not fls) @?= tru
      eval CallByValue (TmApp not fls) @?= tru
      eval NormalOrder (TmApp not tru) @?= fls
      eval CallByName  (TmApp not tru) @?= fls
      eval CallByValue (TmApp not tru) @?= fls
  , testCase "二つ組" $ do
      eval NormalOrder UL.example10 @?= TmVar "v"
      eval CallByName  UL.example10 @?= TmVar "v"
      eval CallByValue UL.example10 @?= TmVar "v"
  , testCase "Church数" $ do
      c 0 @?= TmLam "s" (TmLam "z" "z")
      c 1 @?= TmLam "s" (TmLam "z" (TmApp "s" "z"))
      c 2 @?= TmLam "s" (TmLam "z" (TmApp "s" (TmApp "s" "z")))
      c 3 @?= TmLam "s" (TmLam "z" (TmApp "s" (TmApp "s" (TmApp "s" "z"))))

      -- scc
      eval NormalOrder (TmApp scc (c 0)) @?= c 1
      -- 抽象の本体の適用は許可されないため
      eval CallByName  (TmApp scc (c 0)) @?= TmLam "s" (TmLam "z" (TmApp "s" (TmApp (TmApp (c 0) "s") "z")))
      eval CallByValue (TmApp scc (c 0)) @?= TmLam "s" (TmLam "z" (TmApp "s" (TmApp (TmApp (c 0) "s") "z")))
      eval NormalOrder (TmApp scc (c 0)) @?= eval NormalOrder (TmApp scc2 (c 0))
      eval NormalOrder (TmApp scc (c 1)) @?= eval NormalOrder (TmApp scc2 (c 1))
      eval NormalOrder (TmApp scc (c 2)) @?= eval NormalOrder (TmApp scc2 (c 2))

      -- plus
      eval NormalOrder (TmApp (TmApp plus (c 5)) (c 10))    @?= c 15
      eval NormalOrder (TmApp (TmApp plus (c 100)) (c 200)) @?= c 300

      -- times
      eval NormalOrder (TmApp (TmApp times (c 5)) (c 10))    @?= c 50
      eval NormalOrder (TmApp (TmApp times (c 100)) (c 200)) @?= c 20000
      eval NormalOrder (TmApp (TmApp times (c 5)) (c 10))    @?= eval NormalOrder (TmApp (TmApp times2 (c 5)) (c 10))
      eval NormalOrder (TmApp (TmApp times (c 100)) (c 200)) @?= eval NormalOrder (TmApp (TmApp times2 (c 100)) (c 200))
      eval NormalOrder (TmApp (TmApp times (c 5)) (c 10))    @?= eval NormalOrder (TmApp (TmApp times3 (c 5)) (c 10))
      eval NormalOrder (TmApp (TmApp times (c 100)) (c 200)) @?= eval NormalOrder (TmApp (TmApp times3 (c 100)) (c 200))

      -- power
      eval NormalOrder (TmApp (TmApp power1 (c 2)) (c 10)) @?= c 1024
      eval NormalOrder (TmApp (TmApp power1 (c 2)) (c 0))  @?= c 1

      -- TODO https://github.com/waddlaw/TAPL/issues/13
      -- eval NormalOrder (TmApp (TmApp power2 (c 2)) (c 3))  @?= c 9
      -- eval NormalOrder (TmApp (TmApp power2 (c 0)) (c 2))  @?= c 1
  ]
