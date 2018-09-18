{-# LANGUAGE NoImplicitPrelude #-}
module Language.Utils.Pretty
  ( prettyText
  ) where

import RIO

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

prettyText :: Pretty a => a -> Text
prettyText = renderStrict . layoutCompact . pretty
