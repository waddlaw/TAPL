{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Language.Recon.Exercise.Ex22_4_6 where

import Test.Tasty
import Test.Tasty.HUnit
import qualified RIO.Set as Set

import Language.Recon.Exercise.Ex22_4_6

test_unify :: TestTree
test_unify = testGroup "ex22.3.10"
  [ testGroup "unify"
      [ testCase "ex22.4.3-1" do
          let expected = Just $ Set.fromList [(TyVar "X",TyNat),(TyVar "Y",TyArr TyNat TyNat)]
          unify ex22_4_3_1 @?= expected

      , testCase "ex22.4.3-2" do
          let expected = Just $ Set.fromList [(TyVar "X",TyNat),(TyVar "Y",TyNat)]
          unify ex22_4_3_2 @?= expected

      , testCase "ex22.4.3-3" do
          let expected = Just $ Set.fromList
                [ (TyVar "Z",TyArr (TyVar "U") (TyVar "W"))
                , (TyVar "Y",TyArr (TyVar "U") (TyVar "W"))
                , (TyVar "X",TyArr (TyVar "U") (TyVar "W"))
                ]
          unify ex22_4_3_3 @?= expected

      , testCase "ex22.4.3-4" do
          let expected = Nothing
          unify ex22_4_3_4 @?= expected

      , testCase "ex22.4.3-5" do
          let expected = Nothing
          unify ex22_4_3_5 @?= expected

      , testCase "ex22.4.3-6" do
          let expected = Just $ Set.fromList []
          unify ex22_4_3_6 @?= expected

      -- FIXME
      -- , testCase "ex22.5.2" do
      --     let expected = Just $ Set.fromList
      --           [ (TyVar "X"    , TyArr (TyVar "Z")    (TyVar "?X_1"))
      --           , (TyVar "Y"    , TyArr (TyVar "Z")    (TyVar "?X_2"))
      --           , (TyVar "?X_1" , TyArr (TyVar "?X_2") (TyVar "?X_3"))
      --           ]
      --     unify ex22_5_2 @?= expected
      ]
  ]