{-# LANGUAGE OverloadedStrings #-}

module Language.NB.Type (Term (..)) where

import Data.Text.Prettyprint.Doc
import RIO

data Term
  = TmTrue -- constant true
  | TmFalse -- constant false
  | TmIf Term Term Term -- conditional
  | TmZero -- constant zero
  | TmSucc Term -- successor
  | TmPred Term -- predecessor
  | TmIsZero Term -- zero test
  deriving stock (Eq, Show)

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
    TmZero -> "0"
    TmSucc t -> "succ" <+> pretty t
    TmPred t -> "pred" <+> pretty t
    TmIsZero t -> "iszero" <+> pretty t
