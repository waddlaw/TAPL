module Language.UntypedLambda.Parser
  ( runUlParser
  )
where

import Language.Core.Parser
import Language.UntypedLambda.Lib.Church
import Language.UntypedLambda.Prelude
import Language.UntypedLambda.Types
import qualified RIO.List.Partial as L.Partial
import qualified RIO.Map as Map
import qualified RIO.Text as Text
import Text.Parser.Token.Highlight
import Text.Trifecta

runUlParser :: String -> Either String UntypedLambda
runUlParser = runParserString exprP

exprP :: Parser UntypedLambda
exprP = lefty <$> factorP <*> termsP
  where
    lefty x xs = L.Partial.foldl1 TmApp (x : xs)
    termsP = many (space *> factorP)

factorP :: Parser UntypedLambda
factorP = (char '(' *> (exprP <* char ')')) <|> try numP <|> varP <|> lambdaP

lambdaP :: Parser UntypedLambda
lambdaP =
  TmLam <$ symbol "λ" <*>
    identP <*
    dot <*>
    token exprP

-- FIXME
numP :: Parser UntypedLambda
numP =
  c . fromMaybe 0 . readMaybe <$ char 'c' <*>
    some digit

varP :: Parser UntypedLambda
varP = toTerm <$> oneOf ['a'.. 'z'] <*> many alphaNum
  where
    toTerm x xs = lifty $ Text.pack (x : xs)
    lifty var = Map.findWithDefault (TmVar var) var prelude

identP :: Parser Text
identP = ident defaultIdentStyle

defaultIdentStyle :: IdentifierStyle Parser
defaultIdentStyle = IdentifierStyle
  { _styleName = "UntypedLambda"
  , _styleStart = oneOf ['a'.. 'z']
  , _styleLetter = alphaNum
  , _styleReserved = mempty
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }
