{-# LANGUAGE NoImplicitPrelude #-}
module Language.FullSimpleLambda.Pretty
  ( prettyFullSimpleText
  ) where

import           RIO

import           Language.FullSimpleLambda.Types

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

prettyFullSimpleText :: Context -> FullSimpleTypedLambda -> Text
prettyFullSimpleText ctx = renderStrict . layoutCompact . pprFullSimple ctx
