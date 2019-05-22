module Language.Core
  ( module Language.Core.Types
  , trim
  , render
  , renderPrelude
  , displayRender
  , strategies
  ) where

import Language.Core.Types

import qualified RIO.Char as C
import qualified RIO.List as L
import qualified RIO.Map  as Map
import qualified RIO.Text as Text

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

trim :: String -> String
trim = L.dropWhileEnd C.isSpace . L.dropWhile C.isSpace

render :: Pretty a => a -> String
render = renderString . layoutPretty defaultLayoutOptions . pretty

displayRender :: Pretty a => a -> Utf8Builder
displayRender = display . Text.pack . render

renderPrelude :: Pretty a => Map Text a -> String
renderPrelude = L.foldr glue "" . Map.toList
  where
    glue (key, func) acc  = mconcat [Text.unpack key, ": ", render func, addNewline acc]
    addNewline ""  = ""
    addNewline acc = "\n" ++ acc

strategies :: [Strategy]
strategies = [minBound .. maxBound]