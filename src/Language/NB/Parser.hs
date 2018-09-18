{-# LANGUAGE NoImplicitPrelude #-}
module Language.NB.Parser
  ( runNbParser
  ) where

import RIO

import           Language.NB.Types
import           Language.Utils.Parser

import           Text.Trifecta

runNbParser :: String -> Either String Term
runNbParser = runParserString termP

termP :: Parser Term
termP =  trueP
     <|> falseP
     <|> ifP
     <|> zeroP
     <|> succP
     <|> predP
     <|> iszeroP

iszeroP :: Parser Term
iszeroP = TmIsZero <$  symbol "iszero"
                   <*> (parens termP <|> token termP)

predP :: Parser Term
predP = TmPred <$  symbol "pred"
               <*> (parens termP <|> token termP)

succP :: Parser Term
succP = TmSucc <$  symbol "succ"
               <*> (parens termP <|> token termP)

zeroP :: Parser Term
zeroP = TmZero <$ symbol "0"

trueP :: Parser Term
trueP = TmTrue <$ symbol "true"

falseP :: Parser Term
falseP = TmFalse <$ symbol "false"

ifP :: Parser Term
ifP = TmIf <$  symbol "if"
           <*> (parens termP <|> token termP)
           <*  symbol "then"
           <*> (parens termP <|> token termP)
           <*  symbol "else"
           <*> (parens termP <|> token termP)
