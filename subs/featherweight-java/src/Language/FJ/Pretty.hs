{-# LANGUAGE OverloadedStrings #-}
module Language.FJ.Pretty (renderFJ) where

import Language.FJ.Type

import RIO
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

renderFJ :: Term -> Text
renderFJ = renderStrict . layoutPretty defaultLayoutOptions . pretty
