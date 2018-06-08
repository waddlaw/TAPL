-- https://github.com/mfejzer/knuth-bendix-completion/blob/master/KnuthBendixCompletion/Datatypes.hs
module KnuthBendix.Types
  ( maxLength
  , getLength
  , findVarsInTerm
  ) where

import           Data.Function
import           Data.List
import           Data.Text.Prettyprint.Doc
import           Test.QuickCheck

data Term
  = Func String [Term]
  | Var String
  deriving (Eq, Show)

instance Ord Term where
  compare termA termB =
    if result == EQ
      then checkVarCount termA termB
      else result
    where
    result = order termA termB
    order :: Term -> Term -> Ordering
    order (Func name args) (Var v) = GT
    order (Var v) (Func name args) = LT
    order (Var a) (Var b) = EQ
    order (Func nameA (a:argsA)) (Func nameB (b:argsB)) =
        if result == EQ
          then order (Func nameA argsA) (Func nameB argsB)
          else result
        where result = order a b
    order (Func nameA []) (Func nameB []) = EQ
    order (Func nameA (a:args)) (Func nameB []) = GT
    order (Func nameA []) (Func nameB (b:args)) = LT
    checkVarCount :: Term -> Term -> Ordering
    checkVarCount termA termB =
       if (length.findVarsInTerm) termA > (length.findVarsInTerm) termB
         then GT
         else
           if (length.findVarsInTerm) termA < (length.findVarsInTerm) termB
             then LT
             else EQ

instance Arbitrary Term where
  arbitrary = oneof
    [ Var  <$> vectorOf 5 (elements ['a'..'z'])
    , Func <$> vectorOf 5 (elements ['a'..'z']) <*> vectorOf 2 arbitrary
    ]

instance Pretty Term where
  pretty (Var name)  = pretty name
  pretty (Func f ts) = pretty f <+> pretty ":" <+> pretty ts

data ReductionRule = ReductionRule
  { rule   :: Term
  , result :: Term
  } deriving (Eq, Show)

instance Pretty ReductionRule where
  pretty (ReductionRule rule result) = pretty rule <+> pretty "->" <+> pretty result

data AlgorithmStatus
  = Finished
    { finalRules :: ReductionRules }
  | CanProceed
    { axioms :: Axioms
    , rules  :: ReductionRules
    }
  | FailedOn
    { lastAxiom        :: Axiom
    , incompleteAxioms :: Axioms
    , incompleteRules  :: ReductionRules
    }
  deriving (Eq, Show)

data Axiom = Axiom
  { lhs :: Term
  , rhs :: Term
  } deriving (Eq, Show)

instance Arbitrary Axiom where
  arbitrary = Axiom <$> arbitrary <*> arbitrary

instance Pretty Axiom where
  pretty (Axiom l r) = pretty "Axiom:" <+> pretty l <+> pretty "<->" <+> pretty r

instance Ord Axiom where
  (<)  = (<)  `on` maxLength
  (<=) = (<=) `on` maxLength
  (>)  = (>)  `on` maxLength
  (>=) = (>=) `on` maxLength

maxLength :: Axiom -> Int
maxLength axiom = max ln rn
  where
    ln = getLength . lhs $ axiom
    rn = getLength . rhs $ axiom

getLength :: Term -> Int
getLength = length . findVarsInTerm

type ReductionRules = [ReductionRule]
type Axioms = [Axiom]

type Vars = [Term]
type Var = Term

findVarsInTerm :: Term -> Vars
findVarsInTerm = nub . findVars []

findVars :: Vars -> Term -> Vars
findVars acc var@(Var _)     = var:acc
findVars acc (Func _ [])     = acc
findVars acc (Func f (t:ts)) = findVars (findVars acc t) (Func f ts)
