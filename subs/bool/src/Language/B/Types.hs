{-# LANGUAGE OverloadedStrings #-}

module Language.B.Types
  ( Term (..),
    Rule (..),
    EvalRelation (..),
    Premise,
    Conclusion,
  )
where

import Data.Text.Prettyprint.Doc
import RIO

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  deriving stock (Eq, Show)

data Rule
  = E_IFTRUE
  | E_IFFALSE
  | E_IF
  deriving stock (Enum, Bounded, Show)

instance Pretty Rule where
  pretty = \case
    E_IFTRUE -> "E-IFTRUE"
    E_IFFALSE -> "E-IFFALSE"
    E_IF -> "E-IF"

newtype EvalRelation = EvalRelation {unwrap :: (Term, Term)}
  deriving stock (Eq, Show)

type Premise = EvalRelation

type Conclusion = EvalRelation

instance Pretty Term where
  pretty = \case
    TmTrue -> "true"
    TmFalse -> "false"
    TmIf t1 t2 t3 ->
      "if" <+> pretty t1
        <+> "then"
        <+> pretty t2
        <+> "else"
        <+> pretty t3

instance Pretty EvalRelation where
  pretty (EvalRelation (t, t')) = pretty t <+> "->" <+> pretty t'
