module Language.Core
  ( module Language.Core.Types,
    trim,
    render,
    renderPrelude,
    displayRender,
    strategies,
  )
where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String
import Language.Core.Types
import RIO
import qualified RIO.Char as Char
import qualified RIO.List as List
import qualified RIO.Map as Map
import qualified RIO.Text as Text

trim :: String -> String
trim = List.dropWhileEnd Char.isSpace . List.dropWhile Char.isSpace

render :: Pretty a => a -> String
render = renderString . layoutPretty defaultLayoutOptions . pretty

displayRender :: Pretty a => a -> Utf8Builder
displayRender = display . Text.pack . render

renderPrelude :: Pretty a => Map Text a -> String
renderPrelude = foldr glue "" . Map.toList
  where
    glue (key, func) acc = mconcat [Text.unpack key, ": ", render func, addNewline acc]
    addNewline "" = ""
    addNewline acc = "\n" ++ acc

strategies :: [Strategy]
strategies = [minBound .. maxBound]
