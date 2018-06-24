{-# LANGUAGE OverloadedStrings #-}
module Test.Extensible.UntypedLambda where

import           Prelude                                   hiding (and, fst,
                                                            head, id, not, or,
                                                            snd, tail)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Extensible.Language.UntypedLambda
import           Extensible.Language.UntypedLambda.Prelude
import           Language.Utils.Pretty

import Data.Either

test_extensible_untyped_lambda :: TestTree
test_extensible_untyped_lambda = testGroup "Extensible UntypedLambda" $
  [ testCase "pretty" $ do
      prettyText (var "x") @?= "x"
      prettyText (lambda "x" "x") @?= "λx. x"
      prettyText (app "x" "y") @?= "x y"
  , testCase "parser" $ do
      runExUlParser "x" @?= Right "x"
      runExUlParser "x1y" @?= Right "x1y"
      isLeft (runExUlParser "Xyz") @?= True
      isLeft (runExUlParser "123x") @?= True
      runExUlParser "λx. t" @?= Right (lambda "x" "t")
      isLeft (runExUlParser "λ. Ab") @?= True
      runExUlParser "s t u" @?= runExUlParser "(s t) u"
      runExUlParser "λx. λy. x y x" @?= runExUlParser "λx. (λy. ((x y) x))"

      -- Bool
      runExUlParser "tru"  @?= Right tru
      runExUlParser "fls"  @?= Right fls
      runExUlParser "test" @?= Right test
      runExUlParser "and"  @?= Right and
      runExUlParser "or"   @?= Right or
      runExUlParser "not"  @?= Right not

      -- pair
      runExUlParser "pair" @?= Right pair
      runExUlParser "fst"  @?= Right fst
      runExUlParser "snd"  @?= Right snd

      -- church
      runExUlParser "c10"      @?= Right (c 10)
      runExUlParser "c0"       @?= Right (c 0)
      runExUlParser "c123"     @?= Right (c 123)
      runExUlParser "scc"      @?= Right scc
      runExUlParser "plus"     @?= Right plus
      runExUlParser "times"    @?= Right times
      runExUlParser "iszro"    @?= Right iszro
      runExUlParser "prd"      @?= Right prd
      runExUlParser "subtract" @?= Right subtract1
      runExUlParser "equal"    @?= Right equal

      -- list
      runExUlParser "nil"   @?= Right nil
      runExUlParser "head"  @?= Right head
      runExUlParser "isnil" @?= Right isnil
      runExUlParser "cons"  @?= Right cons
      runExUlParser "tail"  @?= Right tail
  ]
