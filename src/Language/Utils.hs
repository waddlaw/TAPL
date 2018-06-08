module Language.Utils
  ( trim
  ) where

import Data.List
import Data.Char

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace