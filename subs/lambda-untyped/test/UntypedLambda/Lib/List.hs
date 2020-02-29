{-# LANGUAGE OverloadedStrings #-}

module UntypedLambda.Lib.List where

import Language.Core.Types
import Language.UntypedLambda
import Language.UntypedLambda.Lib.Bool
import Language.UntypedLambda.Lib.Church
import Language.UntypedLambda.Lib.List

import RIO
import Test.Tasty
import Test.Tasty.HUnit

test_ul :: TestTree
test_ul =
  testGroup "UntypedLambda.Lib.List"
    [ testCase "cons"
        $ eval NormalOrder (cons @@ "x" @@ nil)
          @?= λ "c" (λ "n" $ "c" @@ "x" @@ "n"), -- TODO: 振る舞い等価
      testCase "isnil" $ do
        eval CallByValue (isnil @@ nil) @?= tru
        eval CallByValue (isnil @@ λ "c" (λ "n" ("c" @@ "x" @@ "n"))) @?= fls
        eval CallByValue (isnil @@ λ "c" (λ "n" ("c" @@ "x1" @@ ("c" @@ "x2" @@ "n")))) @?= fls,
      testCase "head" $ do
        eval CallByValue (head @@ nil) @?= nil
        eval CallByValue (head @@ λ "c" (λ "n" ("c" @@ "x" @@ "n"))) @?= "x",
      testCase "tail" $ do
        eval CallByValue (tail @@ nil) @?= nil
        eval CallByValue (tail @@ λ "c" (λ "n" ("c" @@ "x" @@ "n"))) @?= nil
        eval NormalOrder (tail @@ λ "c" (λ "n" ("c" @@ "x" @@ ("c" @@ "y" @@ "n")))) @?= λ "c" (λ "n" $ "c" @@ "y" @@ "n"), -- TODO: 振る舞い等価
      testCase "sumlist (Ex.5.2.11)" $ do
        let l = cons @@ c 2 @@ (cons @@ c 3 @@ (cons @@ c 4 @@ nil))
        eval CallByValue (equal @@ (sumlist @@ l) @@ c 9) @?= tru
        eval CallByValue (equal @@ (sumlist' @@ l) @@ c 9) @?= tru
      ]
