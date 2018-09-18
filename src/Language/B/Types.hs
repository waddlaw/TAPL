{-# LANGUAGE NoImplicitPrelude #-}
module Language.B.Types
  ( Term (..)
  , Rule (..)
  , EvalRelation (..)
  , Premise
  , Conclusion
  ) where

import           RIO

import           Data.Text.Prettyprint.Doc

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  deriving (Eq, Show)

data Rule
  = E_IFTRUE
  | E_IFFALSE
  | E_IF
  deriving (Enum, Bounded, Show)

instance Pretty Rule where
  pretty E_IFTRUE  = pretty "E-IFTRUE"
  pretty E_IFFALSE = pretty "E-IFFALSE"
  pretty E_IF      = pretty "E-IF"

newtype EvalRelation = EvalRelation { unwrap :: (Term, Term) }
  deriving (Eq, Show)

type Premise         = EvalRelation
type Conclusion      = EvalRelation

instance Pretty Term where
  pretty TmTrue  = pretty "true"
  pretty TmFalse = pretty "false"
  pretty (TmIf t1 t2 t3) =  pretty "if"   <+> pretty t1
                       <+> pretty "then" <+> pretty t2
                       <+> pretty "else" <+> pretty t3

instance Pretty EvalRelation where
  pretty (EvalRelation (t, t')) = pretty t <+> pretty "->" <+> pretty t'
