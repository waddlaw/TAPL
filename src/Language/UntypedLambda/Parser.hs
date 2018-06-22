module Language.UntypedLambda.Parser
  ( runUlParser
  ) where

import           Language.UntypedLambda.Prelude (c, prelude)
import           Language.UntypedLambda.Types
import           Language.Utils.Parser

import           Control.Applicative
import qualified Data.Map                       as Map
import qualified Data.Text                      as T
import           Text.Trifecta

runUlParser :: String -> Either String Term
runUlParser = runParserString exprP

exprP :: Parser Term
exprP = lefty <$> factorP <*> termsP
  where
    lefty x xs = foldl1 TmApp (x:xs)
    termsP = many (space *> factorP)

factorP :: Parser Term
factorP = (char '(' *> (exprP <* char ')')) <|> try numP <|> varP <|> lambdaP

lambdaP :: Parser Term
lambdaP = TmLam <$  symbol "λ"
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
    lifty var = Map.findWithDefault (TmVar var) var prelude
