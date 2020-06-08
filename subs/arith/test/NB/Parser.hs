{-# LANGUAGE OverloadedStrings #-}

module NB.Parser where

import Language.NB
import RIO
import Test.Hspec.Megaparsec
import Test.Tasty.Hspec
import Text.Megaparsec

spec_parser :: Spec
spec_parser = do
  describe "values" $ do
    it "true" $ parse pTrue "" "true" `shouldParse` TmTrue
    it "false" $ parse pFalse "" "false" `shouldParse` TmFalse
    it "0" $ parse pZero "" "0" `shouldParse` TmZero
    it "succ 0" $ parse pSucc "" "succ 0" `shouldParse` TmSucc TmZero
