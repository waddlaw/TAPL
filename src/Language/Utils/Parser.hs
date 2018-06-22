module Language.Utils.Parser
  ( runParserString
  , identP
  ) where

import           Text.Trifecta
import           Text.Parser.Token.Highlight

import           Data.Text                      (Text)

runParserString :: Parser a -> String -> Either String a
runParserString p input =
  case parseString p mempty input of
    Failure xs -> Left $ "parse failure: " ++ show xs
    Success a  -> Right a

identP :: Parser Text
identP = ident defaultIdentStyle

defaultIdentStyle :: IdentifierStyle Parser
defaultIdentStyle = IdentifierStyle
  { _styleName              = "UntypedLambda"
  , _styleStart             = oneOf ['a'..'z']
  , _styleLetter            = alphaNum
  , _styleReserved          = mempty
  , _styleHighlight         = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }