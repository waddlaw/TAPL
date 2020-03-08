{-# LANGUAGE OverloadedStrings #-}
module Language.FJ.Pretty (pretty) where

import Language.FJ.Type

import RIO
import qualified RIO.Text as Text

pretty :: Term -> Text
pretty = \case
  TmVar var -> getVarName var

  TmFieldRef t@TmCast{} field -> mconcat ["(", pretty t, ").", getFieldName field]
  TmFieldRef t field -> mconcat [pretty t, ".", getFieldName field]

  TmMethodInv t method args ->
    mconcat [pretty t, ".", getMethodName method, "(", Text.intercalate ", " (map pretty args), ")"]

  TmNew cls args ->
    mconcat ["new ", getClassName cls, "(", Text.intercalate ", " (map pretty args), ")"]

  TmCast cls t@TmFieldRef{} -> mconcat ["(", getClassName cls, ")", "(", pretty t, ")"]
  TmCast cls t -> mconcat ["(", getClassName cls, ")", pretty t]
