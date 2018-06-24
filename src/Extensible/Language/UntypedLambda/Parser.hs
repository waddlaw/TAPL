module Extensible.Language.UntypedLambda.Parser
  ( runExUlParser
  ) where

import           Extensible.Language.UntypedLambda.Prelude (c, prelude)
import           Extensible.Language.UntypedLambda.Types
import           Language.Utils.Parser

import           Text.Trifecta

import           Control.Applicative
import qualified Data.Map                                  as Map
import qualified Data.Text                                 as T

runExUlParser :: String -> Either String Term
runExUlParser = runParserString exprP

exprP :: Parser Term
exprP = lefty <$> factorP <*> termsP
  where
    lefty x xs = foldl1 app (x:xs)
    termsP = many (space *> factorP)

factorP :: Parser Term
factorP = (char '(' *> (exprP <* char ')')) <|> try numP <|> varP <|> lambdaP

lambdaP :: Parser Term
lambdaP = lambda <$  symbol "λ"
                <*> identP
                <*  dot
                <*> token exprP

numP :: Parser Term
numP = c . read <$  char 'c'
                <*> some digit

varP :: Parser Term
varP = toTerm <$> oneOf ['a'..'z'] <*> many alphaNum
  where
    toTerm x xs = lifty $ T.pack (x:xs)
    lifty x = Map.findWithDefault (var x) x prelude
