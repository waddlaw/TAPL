{-# LANGUAGE OverloadedStrings #-}

module B.Eval where

import Language.B.Eval
import Language.B.Example
import Language.B.Type
import RIO
import Test.Tasty.Hspec

spec_eval :: Spec
spec_eval =
  describe "p25-33: 3.5 Evaluation" $ do
    it "definition 3.5.1" $ do
      let input = TmIf TmTrue TmTrue (TmIf TmFalse TmFalse TmFalse)
      eval input `shouldBe` TmTrue

    it "definition 3.5.3" $ do
      let input = TmIf t TmFalse TmFalse
      eval' input `shouldBe` TmIf u TmFalse TmFalse
