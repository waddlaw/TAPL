{-# LANGUAGE OverloadedStrings #-}

module FJ.Eval where

import Language.FJ.Eval
import Language.FJ.Example
import Language.FJ.Type
import RIO
import Test.Tasty.Hspec

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
      let expected = TmNew (mkClass "Pair") [a, b]
          a = TmNew (mkClass "A") []
          b = TmNew (mkClass "B") []
      eval exCT exMain4 `shouldBe` expected

  describe "evalTrace" $
    it "((Pair)(new Pair(new Pair(new A(), new B()), new A()).fst)).snd" $
      do
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
