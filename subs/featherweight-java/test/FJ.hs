{-# LANGUAGE OverloadedStrings #-}
module FJ where

import Language.FJ.Eval
import Language.FJ.Example
import Language.FJ.Parser
import Language.FJ.Pretty
import Language.FJ.Type

import RIO

import Text.Megaparsec
import Test.Hspec.Megaparsec
import Test.Tasty.Hspec

testP :: (Show a, Eq a) => Parser a -> FilePath -> a -> Expectation
testP p fp expected = do
  input <- readFileUtf8 fp
  parse p "" input `shouldParse` expected

spec_parser :: Spec
spec_parser =
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
          constr = K (mkClass "Pair") [(mkClass "Object", mkField "fst"),(mkClass "Object", mkField "snd")] [] [(mkField "fst", mkField "fst"), (mkField "snd", mkField "snd")]
          t = TmNew (mkClass "Pair") [TmVar $ mkVar "newfst", TmFieldRef (TmVar $ mkVar "this") (mkField "snd")]
          ms = [ M (mkClass "Pair") (mkMethod "setfst") [(mkClass "Object", mkVar "newfst")] t]
          expected = CL (mkClass "Pair") (mkClass "Object") fields constr ms
      test "test/resource/test3.fj" expected

spec_evaluation :: Spec
spec_evaluation = do
  describe "eval" $ do
    it "E-INVKNEW" $ do
      let expected = TmNew (mkClass "Pair") [b, b]
          b = TmNew (mkClass "B") []
      eval exCT exMain1 `shouldBe` expected

    it "cast" $ do
      let expected = TmNew (mkClass "B") []
      eval exCT exMain2 `shouldBe` expected

    it "E-PROJNEW" $ do
      let expected = TmNew (mkClass "B") []
      eval exCT exMain3 `shouldBe` expected

    it "E-CASTNEW" $ do
      let expected =TmNew (mkClass "Pair") [a, b]
          a = TmNew (mkClass "A") []
          b = TmNew (mkClass "B") []
      eval exCT exMain4 `shouldBe` expected

  describe "evalTrace" $ do
    it "((Pair)(new Pair(new Pair(new A(), new B()), new A()).fst)).snd" $ do
      let expected = [exMain2, step1, step2, step3]
          step1 = TmFieldRef (TmCast (mkClass "Pair") (TmNew (mkClass "Pair") [a, b])) (mkField "snd")
          a = TmNew (mkClass "A") []
          b = TmNew (mkClass "B") []
          step2 = TmFieldRef (TmNew (mkClass "Pair") [a, b]) (mkField "snd")
          step3 = b
      evalTrace exCT exMain2 `shouldBe` expected

spec_isValue :: Spec
spec_isValue = do
  it "new A()" $ do
    let actual = TmNew (mkClass "A") []
    isValue actual `shouldBe` True

  it "new Pair(new A(), new A())" $ do
    let a = TmNew (mkClass "A") []
        actual = TmNew (mkClass "Pair") [a, a]
    isValue actual `shouldBe` True
  
  it "new Pair(new Pair(new A(), new A()), new A())" $ do
    let a = TmNew (mkClass "A") []
        p = TmNew (mkClass "Pair") [a, a]
        actual = TmNew (mkClass "Pair") [p, a]
    isValue actual `shouldBe` True

  it "(new Pair(new Pair(new A(), new A()), new A())).fst" $ do
    let a = TmNew (mkClass "A") []
        p = TmNew (mkClass "Pair") [a, a]
        c = TmNew (mkClass "Pair") [p, a]
        actual = TmFieldRef c (mkField "fst")
    isValue actual `shouldBe` False

  it "(Pair)((new Pair(new Pair(new A(), new A()), new A())).fst)" $ do
    let a = TmNew (mkClass "A") []
        p = TmNew (mkClass "Pair") [a, a]
        c = TmNew (mkClass "Pair") [p, a]
        f = TmFieldRef c (mkField "fst")
        actual = TmCast (mkClass "Pair") f
    isValue actual `shouldBe` False

evalPrettyN :: Int -> Term -> Text
evalPrettyN 0 t = pretty t
evalPrettyN n t = evalPrettyN (n-1) $ eval' exCT t

spec_pretty :: Spec
spec_pretty = do
  describe "exMain1" $ do
    it "eval 0" $ evalPrettyN 0 exMain1 `shouldBe` "new Pair(new A(), new B()).setfst(new B())"
    it "eval 1" $ evalPrettyN 1 exMain1 `shouldBe` "new Pair(new B(), new Pair(new A(), new B()).snd)"
    it "eval 2" $ evalPrettyN 2 exMain1 `shouldBe` "new Pair(new B(), new B())"

  describe "exMain2" $ do
    it "eval 0" $ evalPrettyN 0 exMain2 `shouldBe` "((Pair)(new Pair(new Pair(new A(), new B()), new A()).fst)).snd"
    it "eval 1" $ evalPrettyN 1 exMain2 `shouldBe` "((Pair)new Pair(new A(), new B())).snd"
    it "eval 2" $ evalPrettyN 2 exMain2 `shouldBe` "new Pair(new A(), new B()).snd"
    it "eval 3" $ evalPrettyN 3 exMain2 `shouldBe` "new B()"

  describe "exMain3" $ do
    it "eval 0" $ evalPrettyN 0 exMain3 `shouldBe` "new Pair(new A(), new B()).snd"
    it "eval 1" $ evalPrettyN 1 exMain3 `shouldBe` "new B()"

  describe "exMain4" $ do
    it "eval 0" $ evalPrettyN 0 exMain4 `shouldBe` "(Pair)new Pair(new A(), new B())"
    it "eval 1" $ evalPrettyN 1 exMain4 `shouldBe` "new Pair(new A(), new B())"