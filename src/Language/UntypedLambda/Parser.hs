module Language.UntypedLambda.Parser
  ( runUlParser
  ) where

import           Language.UntypedLambda.Lib.Base (c)
import           Language.UntypedLambda.Prelude  (prelude)
import           Language.UntypedLambda.Types
import           Language.Utils.Parser

import           Control.Applicative
import qualified Data.Map                        as Map
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Text.Parser.Token.Highlight
import           Text.Trifecta

runUlParser :: String -> Either String UntypedLambda
runUlParser = runParserString exprP

exprP :: Parser UntypedLambda
exprP = lefty <$> factorP <*> termsP
  where
    lefty x xs = foldl1 TmApp (x:xs)
    termsP = many (space *> factorP)

factorP :: Parser UntypedLambda
factorP = (char '(' *> (exprP <* char ')')) <|> try numP <|> varP <|> lambdaP

lambdaP :: Parser UntypedLambda
lambdaP = TmLam <$  symbol "λ"
                <*> identP
                <*  dot
                <*> token exprP

numP :: Parser UntypedLambda
numP = c . read <$  char 'c'
                <*> some digit

varP :: Parser UntypedLambda
varP = toTerm <$> oneOf ['a'..'z'] <*> many alphaNum
  where
    toTerm x xs = lifty $ T.pack (x:xs)
    lifty var = Map.findWithDefault (TmVar var) var prelude

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
