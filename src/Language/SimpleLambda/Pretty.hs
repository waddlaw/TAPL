{-# LANGUAGE NoImplicitPrelude #-}
module Language.SimpleLambda.Pretty
  ( prettySimpleText
  ) where

import           RIO

import           Language.SimpleLambda.Types

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

prettySimpleText :: Context -> SimpleTypedLambda -> Text
prettySimpleText ctx = renderStrict . layoutCompact . pprSimple ctx
