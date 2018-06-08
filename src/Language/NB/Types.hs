module Language.NB.Types
  ( Term(..)
  , TmError(..)
  ) where

import           Data.Text.Prettyprint.Doc

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show)

instance Pretty Term where
  pretty TmTrue  = pretty "true"
  pretty TmFalse = pretty "false"
  pretty (TmIf t1 t2 t3) =  pretty "if"   <+> pretty t1
                        <+> pretty "then" <+> pretty t2
                        <+> pretty "else" <+> pretty t3
  pretty TmZero       = pretty "0"
  pretty (TmSucc t)   = pretty "succ" <+> pretty t
  pretty (TmPred t)   = pretty "pred" <+> pretty t
  pretty (TmIsZero t) = pretty "isZero" <+> pretty t

data TmError
  = NoRuleApplies
  deriving (Eq, Show)
