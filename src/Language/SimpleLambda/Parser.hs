{-# LANGUAGE NoImplicitPrelude #-}
module Language.SimpleLambda.Parser
  ( runSimpleLambdaParser
  ) where

-- λs:Bool.λz:Bool.s (s z)
-- λf:Bool.(λx:Bool.f (λy:Bool. (x x) y)) (λx:Bool. f (λy:Bool. (x x) y))

import           RIO                               hiding (try)
import qualified RIO.List.Partial                  as L.Partial
import qualified RIO.Map                           as Map
import qualified RIO.Text                          as Text
import qualified RIO.List as L

import           Language.Utils.Parser
import Language.SimpleLambda.Types

import           Text.Parser.Token.Highlight
import           Text.Trifecta

import Control.Monad.Trans.State

type Env = [Text]

runSimpleLambdaParser :: String -> Either String Term
runSimpleLambdaParser = runParserString (evalStateT exprP [])

exprP :: StateT Env Parser Term
exprP = do
  env <- get
  r1 <- lift $ evalStateT factorP env
  r2 <- lift $ evalStateT termsP env
  pure $ lefty r1 r2
  -- lefty <$> evalStateT factorP env <*> evalStateT termsP env
  where
    lefty x xs = L.Partial.foldl1 TmApp (x:xs)
    termsP = many (space *> factorP)

factorP :: StateT Env Parser Term
-- factorP = (char '(' *> (exprP <* char ')')) <|> try numP <|> varP <|> lambdaP
factorP = (char '(' *> (exprP <* char ')')) <|> varP <|> lambdaP

lambdaP :: StateT Env Parser Term
lambdaP = TmLam <$  lift (symbol "λ")
                <*> identP
                <*  lift (symbol ":")
                <*> typeP
                <*  dot
                <*> token exprP

typeP :: StateT Env Parser Ty
typeP = lefty <$> typeFactorP <*> termsP
  where
    lefty x xs = L.Partial.foldl1 TyArr (x:xs)
    termsP = many (spaces *> string "->" *> spaces *> typeFactorP)

typeFactorP :: StateT Env Parser Ty
typeFactorP = (char '(' *> (typeP <* char ')')) <|> typeBoolP

typeBoolP :: StateT Env Parser Ty
typeBoolP = TyBool <$ string "Bool"

-- FIXME
-- numP :: Parser Term
-- numP = c . fromMaybe 0 . readMaybe <$  char 'c'
--                 <*> some digit

varP :: StateT Env Parser Term
varP = do
    table <- get
    var <- lift $ toTerm <$> oneOf ['a'..'z'] <*> many alphaNum
    pure $ TmVar $ fromMaybe (error "not foound in table") $ L.elemIndex var table
  where
    toTerm x xs =  Text.pack (x:xs)
    -- toTerm x xs = lifty 0
    -- lifty var = Map.findWithDefault (TmVar var) var (Map.fromList []) -- FIXME: prelude

identP :: StateT Env Parser Text
identP = do
  v <- lift $ ident defaultIdentStyle
  modify (v:)
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
