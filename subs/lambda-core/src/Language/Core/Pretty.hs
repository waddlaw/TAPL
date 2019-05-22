module Language.Core.Pretty
  ( prettyText
  ) where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

prettyText :: Pretty a => a -> Text
prettyText = renderStrict . layoutCompact . pretty