{-# LANGUAGE NoImplicitPrelude #-}
module Language.SimpleLambda.Parser
  ( runSimpleLambdaParser
  ) where

-- λs:Bool.λz:Bool.s (s z)
-- λf:Bool.(λx:Bool.f (λy:Bool. (x x) y)) (λx:Bool. f (λy:Bool. (x x) y))

import           RIO                         hiding (try)
import qualified RIO.List                    as L
import qualified RIO.List.Partial            as L.Partial
-- import qualified RIO.Map                     as Map
import qualified RIO.Text                    as Text

import           Language.SimpleLambda.Types
import           Language.Utils.Parser

import           Text.Parser.Token.Highlight
import           Text.Trifecta

import           Control.Monad.Trans.State

runSimpleLambdaParser :: Context -> String -> Either String Term
runSimpleLambdaParser ctx = runParserString (evalStateT exprP ctx)

exprP :: StateT Context Parser Term
exprP = do
  ctx <- get
  r1 <- lift $ evalStateT factorP ctx
  r2 <- lift $ evalStateT termsP ctx
  pure $ lefty r1 r2
  -- lefty <$> evalStateT factorP env <*> evalStateT termsP env
  where
    lefty x xs = L.Partial.foldl1 TmApp (x:xs)
    termsP = many (space *> factorP)

factorP :: StateT Context Parser Term
-- factorP = (char '(' *> (exprP <* char ')')) <|> try numP <|> varP <|> lambdaP
factorP = (char '(' *> (exprP <* char ')')) <|> varP <|> lambdaP

lambdaP :: StateT Context Parser Term
lambdaP = TmLam <$  lift (symbol "λ")
                <*> identP
                <*  lift (symbol ":")
                <*> typeP
                <*  dot
                <*> token exprP

typeP :: StateT Context Parser Ty
typeP = lefty <$> typeFactorP <*> termsP
  where
    lefty x xs = L.Partial.foldl1 TyArr (x:xs)
    termsP = many (spaces *> string "->" *> spaces *> typeFactorP)

typeFactorP :: StateT Context Parser Ty
typeFactorP = (char '(' *> (typeP <* char ')')) <|> typeBoolP

typeBoolP :: StateT Context Parser Ty
typeBoolP = TyBool <$ string "Bool"

-- FIXME
-- numP :: Parser Term
-- numP = c . fromMaybe 0 . readMaybe <$  char 'c'
--                 <*> some digit

varP :: StateT Context Parser Term
varP = do
    ctx <- get
    var <- lift $ toTerm <$> oneOf ['a'..'z'] <*> many alphaNum
    pure $ TmVar $ fromMaybe (error $ Text.unpack var <> " is not found in Contexts") $ L.findIndex ((==var) . fst) $ unCtx ctx
  where
    toTerm x xs =  Text.pack (x:xs)
    -- toTerm x xs = lifty 0
    -- lifty var = Map.findWithDefault (TmVar var) var (Map.fromList []) -- FIXME: prelude

identP :: StateT Context Parser Text
identP = do
  v <- lift $ ident defaultIdentStyle
  modify (addContext (v,NameBind))
  return v

defaultIdentStyle :: IdentifierStyle Parser
defaultIdentStyle = IdentifierStyle
  { _styleName              = "SimpleLambda"
  , _styleStart             = oneOf ['a'..'z']
  , _styleLetter            = alphaNum
  , _styleReserved          = mempty
  , _styleHighlight         = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }
