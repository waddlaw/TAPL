{-# LANGUAGE OverloadedStrings #-}

module FJ.Auxiliary where

import Language.FJ.Auxiliary
import Language.FJ.Example
import Language.FJ.Type
import RIO
import Test.Tasty.Hspec

spec_auxiliary :: Spec
spec_auxiliary = do
  describe "fields"
    $ it "fields exCT (mkClass Pair)"
    $ do
      let expected = [(mkClass "Object", mkField "fst"), (mkClass "Object", mkField "snd")]
      fields exCT (mkClass "Pair") `shouldBe` expected

  describe "mtype"
    $ it "mtype exCT (mkMethod setfst) (mkClass Pair)"
    $ do
      let expected = Just ([mkClass "Object"], mkClass "Pair")
      mtype exCT (mkMethod "setfst") (mkClass "Pair") `shouldBe` expected
