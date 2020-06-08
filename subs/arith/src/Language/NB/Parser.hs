{-# LANGUAGE OverloadedStrings #-}

module Language.NB.Parser
  ( Parser,
    runLangParser,
    pTerm,
    pTrue,
    pFalse,
    pIf,
    pZero,
    pSucc,
    pPred,
  )
where

import Language.Core.Parser
import Language.NB.Type
import RIO hiding (many, some, try)
import Text.Megaparsec

pTerm :: Parser Term
pTerm =
  pTrue
    <|> pFalse
    <|> pIf
    <|> pZero
    <|> pSucc
    <|> pPred
    <|> pIsZero
    <|> between (symbol "(") (symbol ")") pTerm

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

pZero :: Parser Term
pZero = TmZero <$ pKeyword "0"

pSucc :: Parser Term
pSucc = TmSucc <$ pKeyword "succ" <*> pTerm

pPred :: Parser Term
pPred = TmPred <$ pKeyword "pred" <*> pTerm

pIsZero :: Parser Term
pIsZero = TmIsZero <$ pKeyword "iszero" <*> pTerm
