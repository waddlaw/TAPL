{-# LANGUAGE OverloadedStrings #-}
module Test.Language.UntypedLambda.Lib.Bool where

import           Prelude                         hiding (and, not, or)

import           Test.Tasty
import           Test.Tasty.HUnit

import           Language.UntypedLambda
import           Language.UntypedLambda.Lib.Bool

evalAllStrategy :: UntypedLambda -> UntypedLambda  -> IO ()
evalAllStrategy term expected = do
  eval NormalOrder term @?= expected
  eval CallByName  term @?= expected
  eval CallByValue term @?= expected

test_ul :: TestTree
test_ul = testGroup "UntypedLambda.Lib.Base"
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
