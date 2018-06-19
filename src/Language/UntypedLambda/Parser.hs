module Language.UntypedLambda.Parser
  ( runUlParser
  ) where

import           Language.UntypedLambda.Prelude (prelude, c)
import           Language.UntypedLambda.Types
import           Language.Utils.Parser

import           Control.Applicative
import qualified Data.Map                       as Map
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Text.Parser.Token.Highlight
import           Text.Trifecta

runUlParser :: String -> Either String Term
runUlParser = runParserString exprP

exprP :: Parser Term
exprP = lefty <$> factorP <*> termsP
  where
    lefty x xs = foldl1 TmApp (x:xs)
    termsP = many (space *> factorP)

factorP :: Parser Term
factorP = (char '(' *> (exprP <* char ')')) <|> varP <|> lambdaP

lambdaP :: Parser Term
lambdaP = TmLam <$  symbol "λ"
                <*> identP
                <*  dot
                <*> token exprP

varP :: Parser Term
varP = toTerm <$> oneOf ['a'..'z'] <*> many alphaNum
  where
    -- TODO
    toTerm 'c' xs = c (read xs ::Int)
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
