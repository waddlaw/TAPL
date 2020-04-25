{-# LANGUAGE OverloadedStrings #-}

module Language.Core.Parser
  ( Parser,
    runParserString,
    runLangParser,
    pKeyword,
    lexeme,
    symbol,
  )
where

import RIO
import qualified RIO.Text as Text
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.Trifecta as Trifecta

-- TOOD: remove
runParserString :: Trifecta.Parser a -> String -> Either String a
runParserString p input =
  case Trifecta.parseString p mempty input of
    Trifecta.Failure xs -> Left $ "parse failure: " ++ show xs
    Trifecta.Success a -> Right a

-- utils
type Parser = Parsec Void Text

runLangParser :: Parser a -> String -> Either String a
runLangParser p input =
  case runParser p "" (Text.pack input) of
    Left bundle -> Left (errorBundlePretty bundle)
    Right x -> Right x

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")
