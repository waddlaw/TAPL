module Extensible.Language.UntypedLambda.Parser
  ( runExUlParser
  ) where

import           Extensible.Language.UntypedLambda.Types
import           Extensible.Language.UntypedLambda.Prelude (c)
import Language.Utils.Parser

import Control.Lens          ((#))

import           Text.Trifecta

import           Control.Applicative
import qualified Data.Text                      as T

runExUlParser :: String -> Either String Term
runExUlParser = runParserString exprP

exprP :: Parser Term
exprP = lefty <$> factorP <*> termsP
  where
    lefty x xs = foldl1 go (x:xs)
    go acc x = Term $ #app # (acc, x)
    termsP = many (space *> factorP)

factorP :: Parser Term
factorP = (char '(' *> (exprP <* char ')')) <|> try numP <|> varP <|> lambdaP

lambdaP :: Parser Term
lambdaP = toLam <$  symbol "λ"
                <*> identP
                <*  dot
                <*> token exprP
  where
    toLam v t = Term $ #lambda # (v, t)

numP :: Parser Term
numP = c . read <$  char 'c'
                <*> some digit

varP :: Parser Term
varP = toTerm <$> oneOf ['a'..'z'] <*> many alphaNum
  where
    toTerm x xs = Term $ #var # T.pack (x:xs) -- lifty $ T.pack (x:xs)
    -- lifty var = Map.findWithDefault (TmVar var) var prelude