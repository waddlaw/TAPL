{-# LANGUAGE OverloadedStrings #-}

module NB.Eval where

import Language.NB.Eval
import Language.NB.Type
import RIO
import Test.Tasty.Hspec

spec_eval :: Spec
spec_eval =
  describe "p41" $ do
    it "pred 0" $ do
      let input = TmPred TmZero
      eval' input `shouldBe` TmZero

    it "succ (pred 0)" $ do
      let input = TmSucc (TmPred TmZero)
      eval' input `shouldBe` TmSucc TmZero

    it "pred (succ (pred 0))" $ do
      let input = TmPred $ TmSucc $ TmPred TmZero
      eval' input `shouldBe` TmPred (TmSucc TmZero)
