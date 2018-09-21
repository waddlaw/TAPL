{-# LANGUAGE NoImplicitPrelude #-}
module Language.SimpleLambda
  ( module Language.SimpleLambda.Types
  , module Language.SimpleLambda.Parser
  , module Language.SimpleLambda.Pretty
  , typeof
  ) where

import           RIO
import qualified RIO.List.Partial             as L.Partial

import           Language.SimpleLambda.Parser
import           Language.SimpleLambda.Pretty
import           Language.SimpleLambda.Types

import           Language.UntypedLambda.Types (UntypedLambda)
import qualified Language.UntypedLambda.Types as Untyped

typeof :: Context -> Term -> Ty
typeof ctx (TmVar i) = getTypeFromContext ctx i
typeof ctx (TmLam x tyT1 t2) = TyArr tyT1 tyT2
  where
    tyT2 = typeof ctx' t2
    ctx' = addBinding ctx x (VarBind tyT1)
typeof ctx (TmApp t1 t2) =
  case tyT1 of
    TyArr tyT11 tyT12 -> if tyT2 == tyT11
                         then tyT12
                         else error "parameter type mismatch"
    _ -> error "arrow type expected"
  where
    tyT1 = typeof ctx t1
    tyT2 = typeof ctx t2
typeof _ TmTrue = TyBool
typeof _ TmFalse = TyBool
typeof ctx (TmIf t1 t2 t3) =
    if typeof ctx t1 == TyBool
    then if tyT2 == typeof ctx t3
        then tyT2
        else error "arms of conditional have different types"
    else error "guard of conditional not a boolean"
  where
    tyT2 = typeof ctx t2

addBinding :: Context -> Text -> Binding -> Context
addBinding ctx x bind = addContext (x, bind) ctx

getTypeFromContext :: Context -> Int -> Ty
getTypeFromContext ctx i =
  case getBinding ctx i of
    (VarBind tyT) -> tyT
    _             -> error "getTypeFromContext"

getBinding :: Context -> Int -> Binding
getBinding ctx i = snd $ ctx' L.Partial.!! i
  where ctx' = unCtx ctx

-- FIXME
-- erase :: TypedLambda -> UntypedLambda
-- erase (TmVar x) = Untyped.TmVar x
-- erase (TmAbs x _tyT1 t2) = Untyped.TmLam x (erase t2)
-- erase (TmApp t1 t2) = Untyped.TmApp (erase t1) (erase t2)
