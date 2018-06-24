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
  ]
