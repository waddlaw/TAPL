module Language.B.Parser
  ( bparser
  , stepCmdParser
  )
where

import Language.B.Types
import Language.Core.Parser
import RIO
import Text.Trifecta

bparser :: String -> Either String EvalRelation
bparser = runParserString elP

stepCmdParser :: String -> Either String Int
stepCmdParser = runParserString p
  where
    p =
      fromIntegral <$ symbol ":step" <*>
        natural

elP :: Parser EvalRelation
elP = EvalRelation <$> elP'
  where
    elP' =
      (,) <$> token termP <*
        symbol "->" <*>
        token termP

termP :: Parser Term
termP =
  trueP <|>
    falseP <|>
    ifP

trueP :: Parser Term
trueP = TmTrue <$ symbol "true"

falseP :: Parser Term
falseP = TmFalse <$ symbol "false"

ifP :: Parser Term
ifP =
  TmIf <$ symbol "if" <*>
    (parens termP <|> token termP) <*
    symbol "then" <*>
    (parens termP <|> token termP) <*
    symbol "else" <*>
    (parens termP <|> token termP)
