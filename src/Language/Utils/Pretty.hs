module Language.Utils.Pretty
 ( prettyText
 ) where

import           Data.Text                             (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

prettyText :: Pretty a => a -> Text
prettyText = renderStrict . layoutCompact . pretty
