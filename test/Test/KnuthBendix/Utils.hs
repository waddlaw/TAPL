module Test.KnuthBendix.Utils where

import           Test.Tasty
import           Test.Tasty.HUnit

import           KnuthBendix.Types
import           KnuthBendix.Utils

test_renameVars :: TestTree
test_renameVars = testGroup "KnuthBendix.Utils"
  [ testCase "renameVars" $ do
      let t1 = Var "a"
          t2 = Var "b"
      renameVars t1 @?= Var "v0"
      renameVars (Func "f" [t1, t1]) @?= Func "f" [Var "v0", Var "v0"]
      renameVars (Func "f" [t1, Func "g" [t1, t1]]) @?= Func "f" [Var "v0", Func "g" [Var "v0", Var "v0"]]
      renameVars (Func "f" [t1, t2]) @?= Func "f" [Var "v1", Var "v0"]
      renameVars (Func "f" [t1, Func "g" [t2, t1]]) @?= Func "f" [Var "v0", Func "g" [Var "v1", Var "v0"]]
  ]
