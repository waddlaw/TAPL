module Language.SystemF.Pretty (prettySystemFText) where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.SystemF.Types
import RIO

prettySystemFText :: Context -> Term -> Text
prettySystemFText ctx = renderStrict . layoutCompact . pprTerm ctx
