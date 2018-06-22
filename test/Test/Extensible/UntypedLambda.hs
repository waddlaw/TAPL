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

import Control.Lens    (( # ))

test_extensible_untyped_lambda :: TestTree
test_extensible_untyped_lambda = testGroup "Extensible UntypedLambda" $
  [ testCase "pretty" $ do
      let termVar = "x" :: Term
          termLam = Term $ #lambda # ("x", "x")
          termApp = Term $ #app    # ("x", "y")

      prettyText termVar @?= "x"
      prettyText termLam @?= "λx. x"
      prettyText termApp @?= "x y"
  ]
