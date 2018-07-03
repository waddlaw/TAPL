{-# LANGUAGE OverloadedStrings #-}
module Test.Language.UntypedLambda.Lib.NB where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.UntypedLambda
import           Language.UntypedLambda.Lib.NB
import           Language.UntypedLambda.Prelude

test_ul :: TestTree
test_ul = testGroup "UntypedLambda.Lib.NB"
  [ testCase "realbool" $ do
      eval CallByValue (realbool @@ fls) @?= "false"
      eval CallByValue (realbool @@ tru) @?= "true"
  , testCase "realeq" $ do
      eval CallByValue (realeq @@ c 0 @@ c 0) @?= "true"
      eval CallByValue (realeq @@ c 0 @@ c 1) @?= "false"
  , testCase "realnat" $ do
      eval NormalOrder (realnat @@ c 2) @?= "succ" @@ ("succ" @@ "0")
      eval NormalOrder (realnat @@ (times @@ c 2 @@ c 2)) @?= "succ" @@ ("succ" @@ ("succ" @@ ("succ" @@ "0")))
  ]
