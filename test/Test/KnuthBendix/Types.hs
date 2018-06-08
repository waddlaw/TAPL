module Test.KnuthBendix.Types where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           KnuthBendix.Types
import           KnuthBendix.Axiom
import qualified KnuthBendix.Original as O

test_types :: TestTree
test_types = testGroup "KnuthBendix.Types"
  [ testCase "maxLength" $ do
      maxLength (groupAxioms !! 0) @?= 1
      maxLength (groupAxioms !! 1) @?= 1
      maxLength (groupAxioms !! 2) @?= 3
  ]

prop_maxLength :: Axiom -> Property
prop_maxLength axiom = maxLength axiom === O.maxLength axiom

prop_getLength :: Term -> Property
prop_getLength term = getLength term === O.getLength term

prop_findVarsInTerm  :: Term -> Property
prop_findVarsInTerm term = findVarsInTerm term === O.findVarsInTerm term