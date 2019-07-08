{-# LANGUAGE OverloadedStrings #-}

module UntypedLambda.Lib.Bool where

import Language.UntypedLambda.Lib.Bool
import Test.Tasty
import Test.Tasty.HUnit
import Utils

test_ul :: TestTree
test_ul =
  testGroup "UntypedLambda.Lib.Base"
    [ testCase "test" $ do
        evalAllStrategy (mkTest tru tru fls) tru
        evalAllStrategy (mkTest fls tru fls) fls
    , testCase "and" $ do
      evalAllStrategy (mkAnd tru tru) tru
      evalAllStrategy (mkAnd tru fls) fls
      evalAllStrategy (mkAnd fls fls) fls
      evalAllStrategy (mkAnd fls fls) fls
    , testCase "or" $ do
      evalAllStrategy (mkOr tru tru) tru
      evalAllStrategy (mkOr tru fls) tru
      evalAllStrategy (mkOr fls tru) tru
      evalAllStrategy (mkOr fls fls) fls
    , testCase "not" $ do
      evalAllStrategy (mkNot fls) tru
      evalAllStrategy (mkNot tru) fls
    ]
