{-# LANGUAGE OverloadedStrings #-}
module UntypedLambda.Lib.Int where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.UntypedLambda
import           Language.UntypedLambda.Lib.Bool
import           Language.UntypedLambda.Lib.Church
import           Language.UntypedLambda.Lib.Int
import           Language.UntypedLambda.Lib.Pair

test_ul :: TestTree
test_ul = testGroup "UntypedLambda.Lib.Int"
  [ testCase "int" $ do
      eval CallByValue (int 0)    @?= eval CallByValue (λ "b" $ c 0)
      eval CallByValue (int (-3)) @?= eval CallByValue (mkPair fls (c 3))
      eval CallByValue (int 3)    @?= eval CallByValue (mkPair tru (c 3))
  , testCase "succI" $ do
      eval CallByValue (succI @@ int 0) @?= eval CallByValue (int 1)
      eval NormalOrder (succI @@ int (-1)) @?= eval NormalOrder (int 0) -- TODO: 振る舞い等価
      -- TODO: 振る舞い等価?
      -- eval NormalOrder (succI @@ (int 1)) @?= eval NormalOrder (int 2)
  , testCase "succNI" $
      eval CallByValue (succNI @@ mkPair (c 1) (int 0)) @?= eval CallByValue (int 1)
      -- TODO: 振る舞い等価?
      -- eval NormalOrder (plusI @@ int 1 @@ int 1) @?= eval NormalOrder (int 2)
  , testCase "isZeroI" $ do
      eval CallByValue (isZeroI @@ int (-1)) @?= fls
      eval CallByValue (isZeroI @@ int 0) @?= tru
      eval CallByValue (isZeroI @@ int 1) @?= fls
  , testCase "isAbsOneI" $ do
      eval CallByValue (isAbsOneI @@ int (-2)) @?= fls
      eval CallByValue (isAbsOneI @@ int (-1)) @?= tru
      eval CallByValue (isAbsOneI @@ int 0) @?= fls
      eval CallByValue (isAbsOneI @@ int 1) @?= tru
      eval CallByValue (isAbsOneI @@ int 2) @?= fls
  ]
