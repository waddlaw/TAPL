module Language.UntypedLambda.Parser
  ( runUlParser
  ) where

import           Language.UntypedLambda.Types
import           Language.Utils.Parser

import           Control.Applicative
import           Data.Text                    (Text)
import           Text.Parser.Token.Highlight
import           Text.Trifecta

runUlParser :: String -> Either String Term
runUlParser = runParserString termP

termP :: Parser Term
termP =  varP
     <|> lambdaP
     <|> applyP

lambdaP :: Parser Term
lambdaP = TmLam <$  symbol "λ"
                <*> identP
                <*  dot
                <*> (parens termP <|> token termP)

applyP :: Parser Term
applyP = TmApp <$> (parens termP <|> token termP)
               <*  space
               <*> (parens termP <|> token termP)

varP :: Parser Term
varP = (TmVar <$> identP) <|> failP

failP :: Parser Term
failP = do
  _ <- upper <|> digit
  unexpected "unexpected Upper or Digit"

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
