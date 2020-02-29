{-# LANGUAGE OverloadedStrings #-}

module UntypedLambda.Lib.Pair where

import Language.UntypedLambda
import Language.UntypedLambda.Lib.Pair

import RIO hiding (fst, snd)
import Test.Tasty
import Test.Tasty.HUnit
import Utils

test_ul :: TestTree
test_ul =
  testGroup "UntypedLambda.Lib.Pair"
    [ testCase "pair"
        $ evalAllStrategy (mkPair "v" "w") (λ "b" $ "b" @@ "v" @@ "w"),
      testCase "fst"
        $ evalAllStrategy (fst @@ mkPair "v" "w") "v",
      testCase "snd"
        $ evalAllStrategy (snd @@ mkPair "v" "w") "w"
      ]
