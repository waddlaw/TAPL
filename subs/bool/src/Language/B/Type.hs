{-# LANGUAGE OverloadedStrings #-}

module Language.B.Type (Term (..)) where

import Data.Text.Prettyprint.Doc
import RIO

data Term
  = TmTrue -- constant true
  | TmFalse -- constant false
  | TmIf Term Term Term -- conditional
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
