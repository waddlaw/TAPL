{-# LANGUAGE OverloadedStrings #-}

module B.Parser where

import Language.B.Parser
import Language.B.Type
import RIO
import Test.Hspec.Megaparsec
import Test.Tasty.Hspec
import Text.Megaparsec

spec_parser :: Spec
spec_parser = do
  describe "values" $ do
    it "true" $ parse pTrue "" "true" `shouldParse` TmTrue
    it "false" $ parse pFalse "" "false" `shouldParse` TmFalse

  describe "p25-33: 3.5 Evaluation" $ do
    it "p.26" $ do
      parse pTerm "" "if true then (if false then false else false) else true"
        `shouldParse` TmIf TmTrue (TmIf TmFalse TmFalse TmFalse) TmTrue

      parse pTerm "" "if true then false else true"
        `shouldParse` TmIf TmTrue TmFalse TmTrue

    it "definition 3.5.1" $
      parse pTerm "" "if true then true else (if false then false else false)"
        `shouldParse` TmIf TmTrue TmTrue (TmIf TmFalse TmFalse TmFalse)

    it "definition 3.5.3" $ do
      let s = "if true then false else false"
          expectedS = TmIf TmTrue TmFalse TmFalse
      parse pTerm "" s `shouldParse` expectedS

      let t = "if " <> s <> " then true else true"
          expectedT = TmIf expectedS TmTrue TmTrue
      parse pTerm "" t `shouldParse` expectedT

      let u = "if false then true else true"
          expectedU = TmIf TmFalse TmTrue TmTrue
      parse pTerm "" u `shouldParse` expectedU
