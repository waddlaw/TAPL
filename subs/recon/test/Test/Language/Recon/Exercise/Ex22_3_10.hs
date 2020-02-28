{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Language.Recon.Exercise.Ex22_3_10 where

import qualified RIO.Set  as Set
import Test.Tasty
import Test.Tasty.HUnit

import Language.Recon.Exercise.Ex22_3_10

test_runTypingC :: TestTree
test_runTypingC = testGroup "ex22.3.10"
  [ testCase "runTypingC" do
      let expr = TmApp TmZero TmTrue
          expected = ( TyVar "?X_1", Set.fromList [( TyNat, TyArr TyBool ( TyVar "?X_1" ))])
      runTypingC expr @?= expected
  ]