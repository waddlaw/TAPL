{-# LANGUAGE NoImplicitPrelude #-}
module Language.FullSimpleLambda
  ( module Language.FullSimpleLambda.Types
  , module Language.FullSimpleLambda.Parser
  , module Language.FullSimpleLambda.Pretty
  , typeof
  , isValue
  ) where

import           RIO
import qualified RIO.List.Partial                 as L.Partial

import           Language.FullSimpleLambda.Parser
import           Language.FullSimpleLambda.Pretty
import           Language.FullSimpleLambda.Types

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
typeof _ TmUnit = TyUnit  -- ^ 11.2 Unit型

----------------------
-- helper functions --
----------------------

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

-- | 与えられた項が値かどうか判定する述語
isValue :: FullSimpleTypedLambda -> Bool
isValue TmVar{} = True
isValue TmLam{} = True
isValue TmUnit  = True  -- ^ 11.2 Unit型
isValue _       = False
