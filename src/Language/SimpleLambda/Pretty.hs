{-# LANGUAGE NoImplicitPrelude #-}
module Language.SimpleLambda.Pretty
  ( prettySimpleText
  ) where

import           RIO

import           Language.SimpleLambda.Types

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

prettySimpleText :: [Text] -> SimpleTypedLambda -> Text
prettySimpleText fvs = renderStrict . layoutCompact . pprSimple fvs
