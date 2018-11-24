{-# LANGUAGE NoImplicitPrelude #-}
module Language.FullSimpleLambda.Pretty
  ( prettyFullSimpleText
  ) where

import           RIO
import qualified RIO.List.Partial                      as L.Partial

import           Language.FullSimpleLambda.TypeCheck
import           Language.FullSimpleLambda.Types

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

prettyFullSimpleText :: Context -> FullSimpleTypedLambda -> Text
prettyFullSimpleText ctx = renderStrict . layoutCompact . pprFullSimple ctx
