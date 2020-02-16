{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Language.Recon.Exercise.Ex22_4_6 where

import Test.Tasty
import Test.Tasty.HUnit
import qualified RIO.Set as Set

import Language.Recon.Exercise.Ex22_4_6

test_unify :: TestTree
test_unify = testGroup "ex22.3.10" $
  [ testGroup "unify" $
      [ testCase "ex22.4.3-1" do
          let cs = Set.fromList [(TyVar "X", TyNat), (TyVar "Y", TyArr (TyVar "X") (TyVar "X"))]
              expected = Just $ Set.fromList [(TyVar "X",TyNat),(TyVar "Y",TyArr TyNat TyNat)]
          unify cs @?= expected

      , testCase "ex22.4.3-2" do
          let cs = Set.fromList [ (TyArr TyNat TyNat, TyArr (TyVar "X") (TyVar "Y")) ]
              expected = Just $ Set.fromList [(TyVar "X",TyNat),(TyVar "Y",TyNat)]
          unify cs @?= expected

      , testCase "ex22.4.3-3" do
          let cs = Set.fromList
                [ (TyArr (TyVar "X") (TyVar "Y"), TyArr (TyVar "Y") (TyVar "Z"))
                , (TyVar "Z", TyArr (TyVar "U") (TyVar "W"))
                ]
              expected = Just $ Set.fromList
                [ (TyVar "Z",TyArr (TyVar "U") (TyVar "W"))
                , (TyVar "Y",TyArr (TyVar "U") (TyVar "W"))
                , (TyVar "X",TyArr (TyVar "U") (TyVar "W"))
                ]
          unify cs @?= expected

      , testCase "ex22.4.3-4" do
          let cs = Set.fromList [ (TyNat, TyArr TyNat (TyVar "Y")) ]
              expected = Nothing
          unify cs @?= expected

      , testCase "ex22.4.3-5" do
          let cs = Set.fromList [ (TyVar "Y", TyArr TyNat (TyVar "Y")) ]
              expected = Nothing
          unify cs @?= expected

      , testCase "ex22.4.3-6" do
          let cs = Set.fromList []
              expected = Just $ Set.fromList []
          unify cs @?= expected
      ]
  ]