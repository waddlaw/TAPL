module Language.FJ.Pretty (renderFJ) where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import Language.FJ.Type
import RIO

renderFJ :: Term -> Text
renderFJ = renderStrict . layoutPretty defaultLayoutOptions . pretty
