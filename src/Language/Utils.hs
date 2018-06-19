module Language.Utils
  ( trim
  , render
  , renderPrelude
  ) where

import           Data.Char
import           Data.List
import           Data.Map                                (Map)
import qualified Data.Map                                as Map
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

render :: Pretty a => a -> String
render = renderString . layoutPretty defaultLayoutOptions . pretty

renderPrelude :: Pretty a => Map Text a -> String
renderPrelude = foldr glue "" . Map.toList
  where
    glue (key, func) acc  = concat [T.unpack key, ": ", render func, addNewline acc]
    addNewline "" = ""
    addNewline acc = "\n" ++ acc
