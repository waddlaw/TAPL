{-# LANGUAGE OverloadedStrings #-}

module Language.B.Parser
  ( Parser,
    runLangParser,
    pTerm,
    pTrue,
    pFalse,
    pIf,
  )
where

import Language.B.Type
import Language.Core.Parser
import RIO hiding (many, some, try)
import Text.Megaparsec

pTerm :: Parser Term
pTerm = try (between (symbol "(") (symbol ")") pTerm') <|> pTerm'

pTerm' :: Parser Term
pTerm' = asum [pTrue, pFalse, pIf]

pTrue :: Parser Term
pTrue = TmTrue <$ pKeyword "true"

pFalse :: Parser Term
pFalse = TmFalse <$ pKeyword "false"

pIf :: Parser Term
pIf =
  TmIf
    <$ pKeyword "if" <*> pTerm
    <* pKeyword "then" <*> pTerm
    <* pKeyword "else" <*> pTerm
