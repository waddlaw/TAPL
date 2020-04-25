{-# LANGUAGE OverloadedStrings #-}

module FJ.Pretty where

import Language.FJ.Eval
import Language.FJ.Example
import Language.FJ.Pretty
import Language.FJ.Type
import RIO
import Test.Tasty.Hspec

evalPrettyN :: Int -> Term -> Text
evalPrettyN 0 t = renderFJ t
evalPrettyN n t = evalPrettyN (n -1) $ eval' exCT t

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
