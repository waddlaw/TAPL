module Language.Utils
  ( trim
  , render
  ) where

import           Data.Char
import           Data.List
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

render :: Pretty a => a -> String
render = renderString . layoutPretty defaultLayoutOptions . pretty
