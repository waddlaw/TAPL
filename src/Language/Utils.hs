{-# LANGUAGE NoImplicitPrelude #-}
module Language.Utils
  ( trim
  , render
  , renderPrelude
  ) where

import RIO
import qualified RIO.Text as Text
import qualified RIO.List as L
import qualified RIO.Char as C

import           Data.Map                                (Map)
import qualified Data.Map                                as Map
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String

trim :: String -> String
trim = L.dropWhileEnd C.isSpace . L.dropWhile C.isSpace

render :: Pretty a => a -> String
render = renderString . layoutPretty defaultLayoutOptions . pretty

renderPrelude :: Pretty a => Map Text a -> String
renderPrelude = L.foldr glue "" . Map.toList
  where
    glue (key, func) acc  = mconcat [Text.unpack key, ": ", render func, addNewline acc]
    addNewline ""  = ""
    addNewline acc = "\n" ++ acc
