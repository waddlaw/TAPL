{-# LANGUAGE OverloadedStrings #-}
module Language.NB.Types
  ( Term (..)
  , TmError (..)
  )
where

import RIO
import Data.Text.Prettyprint.Doc

data Term
  = TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving stock (Eq, Show)

instance Pretty Term where
  pretty = \case
    TmTrue        -> "true"
    TmFalse       -> "false"
    TmIf t1 t2 t3 ->
      "if"   <+> pretty t1 <+>
      "then" <+> pretty t2 <+>
      "else" <+> pretty t3
    TmZero        -> "0"
    TmSucc   t    -> "succ"   <+> pretty t
    TmPred   t    -> "pred"   <+> pretty t
    TmIsZero t    -> "iszero" <+> pretty t

data TmError = NoRuleApplies
  deriving stock (Eq, Show)
