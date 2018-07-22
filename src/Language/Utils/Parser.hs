module Language.Utils.Parser
  ( runParserString
  ) where

import           Text.Trifecta

runParserString :: Parser a -> String -> Either String a
runParserString p input =
  case parseString p mempty input of
    Failure xs -> Left $ "parse failure: " ++ show xs
    Success a  -> Right a
