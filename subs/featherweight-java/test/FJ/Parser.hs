{-# LANGUAGE OverloadedStrings #-}

module FJ.Parser where

import Language.FJ.Parser
import Language.FJ.Type
import RIO
import Test.Hspec.Megaparsec
import Test.Tasty.Hspec
import Text.Megaparsec

testP :: (Show a, Eq a) => Parser a -> FilePath -> a -> Expectation
testP p fp expected = do
  input <- readFileUtf8 fp
  parse p "" input `shouldParse` expected

spec_parser :: Spec
spec_parser = do
  describe "pClassDef" $ do
    let test = testP pClassDef
    it "A" $ do
      let expected = CL (mkClass "A") (mkClass "Object") [] (K (mkClass "A") [] [] []) []
      test "test/resource/test1.fj" expected

    it "B" $ do
      let expected = CL (mkClass "B") (mkClass "Object") [] (K (mkClass "B") [] [] []) []
      test "test/resource/test2.fj" expected

    it "Piar" $ do
      let fields = [(mkClass "Object", mkField "fst"), (mkClass "Object", mkField "snd")]
          constr = K (mkClass "Pair") [(mkClass "Object", mkField "fst"), (mkClass "Object", mkField "snd")] [] [(mkField "fst", mkField "fst"), (mkField "snd", mkField "snd")]
          t = TmNew (mkClass "Pair") [TmVar $ mkVar "newfst", TmFieldRef (TmVar $ mkVar "this") (mkField "snd")]
          ms = [M (mkClass "Pair") (mkMethod "setfst") [(mkClass "Object", mkVar "newfst")] t]
          expected = CL (mkClass "Pair") (mkClass "Object") fields constr ms
      test "test/resource/test3.fj" expected

  describe "pTerm " $ do
    it "new A()" $ do
      let input = "new A()"
          expected = TmNew (mkClass "A") []
      parse pTerm "" input `shouldParse` expected

    it "new Pair(new A(), new B())" $ do
      let input = "new Pair(new A(), new B())"
          expected = TmNew (mkClass "Pair") [TmNew (mkClass "A") [], TmNew (mkClass "B") []]
      parse pTerm "" input `shouldParse` expected

    it "new Pair(new Pair(new A(), new B()), new A())" $ do
      let input = "new Pair(new Pair(new A(), new B()), new A())"
          t1 = TmNew (mkClass "Pair") [TmNew (mkClass "A") [], TmNew (mkClass "B") []]
          expected = TmNew (mkClass "Pair") [t1, TmNew (mkClass "A") []]
      parse pTerm "" input `shouldParse` expected

    it "new Pair(new Pair(new A(), new B()), new A()).fst" $ do
      let input = "new Pair(new Pair(new A(), new B()), new A()).fst"
          t1 = TmNew (mkClass "Pair") [TmNew (mkClass "A") [], TmNew (mkClass "B") []]
          t2 = TmNew (mkClass "Pair") [t1, TmNew (mkClass "A") []]
          expected = TmFieldRef t2 (mkField "fst")
      parse pTerm "" input `shouldParse` expected

    it "(Pair)new Pair(new Pair(new A(), new B()), new A()).fst" $ do
      let input = "(Pair)new Pair(new Pair(new A(), new B()), new A()).fst"
          t1 = TmNew (mkClass "Pair") [TmNew (mkClass "A") [], TmNew (mkClass "B") []]
          t2 = TmNew (mkClass "Pair") [t1, TmNew (mkClass "A") []]
          t3 = TmFieldRef t2 (mkField "fst")
          expected = TmCast (mkClass "Pair") t3
      parse pTerm "" input `shouldParse` expected

    it "((Pair)new Pair(new Pair(new A(), new B()), new A()).fst).snd" $ do
      let input = "((Pair)new Pair(new Pair(new A(), new B()), new A()).fst).snd"
          t1 = TmNew (mkClass "Pair") [TmNew (mkClass "A") [], TmNew (mkClass "B") []]
          t2 = TmNew (mkClass "Pair") [t1, TmNew (mkClass "A") []]
          t3 = TmFieldRef t2 (mkField "fst")
          t4 = TmCast (mkClass "Pair") t3
          expected = TmFieldRef t4 (mkField "snd")
      parse pTerm "" input `shouldParse` expected
