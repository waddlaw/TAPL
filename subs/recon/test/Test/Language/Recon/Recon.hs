{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Language.Recon.Recon where

import Language.Recon.Sigma
import Language.Recon.TySubst
import Language.Recon.Type
import RIO
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Test.Tasty
import Test.Tasty.HUnit

test_sigma :: [TestTree]
test_sigma =
  [ testCase "dom" $ dom example @?= Set.fromList ["X", "Y", "Z"],
    testCase "range" $ range example @?= Set.fromList [TyArr (TyVar "X") (TyVar "X"), TyBool]
  ]

test_tySubst :: [TestTree]
test_tySubst =
  [ testCase "[X|->Bool]. σ(X->X)" $ do
      let sigma = Map.fromList [("X", TyBool)]
          expr = TyArr (TyVar "X") (TyVar "X")
          expected = TyArr TyBool TyBool
      tySubst @Ty sigma expr @?= expected,
    testCase "[X|->Bool, Y|->X->X]. σ(X->X)" $ do
      let sigma = Map.fromList [("X", TyBool), ("Y", TyArr (TyVar "X") (TyVar "X"))]
          expr = TyVar "Y"
          expected = TyArr (TyVar "X") (TyVar "X")
      tySubst @Ty sigma expr @?= expected,
    testCase "context" $ do
      let sigma = Map.fromList [("X", TyBool)]
          ctx = [("x", TyVar "X"), ("y", TyVar "X"), ("x", TyVar "Y")]
          expected = [("x", TyBool), ("y", TyBool), ("x", TyVar "Y")]
      tySubst @Context sigma ctx @?= expected,
    testCase "term" $ do
      let sigma = Map.fromList [("X", TyBool)]
          term = TmLam "x" (TyVar "X") (TmVar "x")
          expected = TmLam "x" TyBool (TmVar "x")
      tySubst @Term sigma term @?= expected
  ]

-- test1 :: TestTree
-- test1 = testCase "runTypingC" $ runTypingC expr @?= expected
--   where
--     expr = TmApp TmZero TmTrue
--     expected = ( TyVar "?X_1", Set.fromList [( TyNat, TyArr TyBool ( TyVar "?X_1" ))])
