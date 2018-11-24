{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Language.FullSimpleLambda
  ( module Language.FullSimpleLambda.Types
  , module Language.FullSimpleLambda.Parser
  , module Language.FullSimpleLambda.Pretty
  , typeof
  , desugar
  ) where

import           RIO
import qualified RIO.List.Partial                 as L.Partial

import           Language.FullSimpleLambda.Parser
import           Language.FullSimpleLambda.Pretty
import           Language.FullSimpleLambda.Types

eval :: Term -> Term
eval (TmIf TmTrue t2 t3)                             = t2                                    -- E-IFTRUE
eval (TmIf TmFalse t2 t3)                            = t3                                   -- E-IFFALSE
eval (TmIf t1@(isValue -> False) t2 t3)              = TmIf (eval t1) t2 t3   -- E-IF
eval (TmApp (TmLam x _ t1) v2@(isValue -> True))     = subst x v2 t1 -- E-APPABS
eval (TmApp (TmWildcard _ t12) t2@(isValue -> True)) = t12       -- E-WILDCARD
eval (TmApp t1@(isValue -> False) t2)                = TmApp (eval t1) t2       -- E-APP1
eval (TmApp v1@(isValue -> True) t2)                 = TmApp v1 (eval t2)        -- E-APP2
eval (TmSeq t1@(isValue -> False) t2)                = TmSeq (eval t1) t2       -- E-SEQ
eval (TmSeq t1@(isValue -> True) t2)                 = t2                        -- E-SEQNEXT

subst :: Text -> Value -> Term -> Term
subst = undefined

typeof :: Context -> Term -> Ty
typeof ctx (TmVar i) = getTypeFromContext ctx i -- T-VAR
typeof ctx (TmLam x tyT1 t2) = TyArr tyT1 tyT2  -- T-ABS
  where
    tyT2 = typeof ctx' t2
    ctx' = addBinding ctx x (VarBind tyT1)
typeof ctx (TmApp t1 t2) =  -- T-APP
  case tyT1 of
    TyArr tyT11 tyT12 -> if tyT2 == tyT11
                         then tyT12
                         else error "parameter type mismatch"
    _ -> error "arrow type expected"
  where
    tyT1 = typeof ctx t1
    tyT2 = typeof ctx t2
typeof _ TmTrue = TyBool      -- T-TRUE
typeof _ TmFalse = TyBool     -- T-FALSE
typeof ctx (TmIf t1 t2 t3) =  -- T-IF
    if typeof ctx t1 == TyBool
    then if tyT2 == typeof ctx t3
        then tyT2
        else error "arms of conditional have different types"
    else error "guard of conditional not a boolean"
  where
    tyT2 = typeof ctx t2
typeof _ TmUnit = TyUnit  -- T-UNIT
typeof ctx (TmSeq TmUnit tyT2) = typeof ctx tyT2 -- T-SEQ
typeof ctx (TmWildcard tyT1 t2) = TyArr tyT1 (typeof ctx t2)  -- T-WILDCARD

-- | 対象の構文
--
-- TmSeq
--
-- TmWildcard
desugar :: Term -> Term
desugar (TmSeq t1 t2)     = TmApp (TmLam "x" TyUnit t2) t1 -- FIXME x notin FV(t2)
desugar (TmWildcard ty t) = TmLam "x" ty t -- FIXME x notin FV(t)
desugar term              = term

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
isValue :: Term -> Bool
isValue TmVar{} = True
isValue TmLam{} = True
isValue TmUnit  = True  -- ^ 11.2 Unit型
isValue _       = False
