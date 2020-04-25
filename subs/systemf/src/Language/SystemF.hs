{-# LANGUAGE OverloadedStrings #-}

module Language.SystemF
  ( module Language.SystemF.Eval,
    module Language.SystemF.Types,
    module Language.SystemF.Parser,
    module Language.SystemF.Pretty,
    module Language.SystemF.Example,
    typeof,
  )
where

import Language.SystemF.Eval
import Language.SystemF.Example
import Language.SystemF.Parser
import Language.SystemF.Pretty
import Language.SystemF.Types
import RIO
import qualified RIO.List.Partial as L.Partial

typeof :: Context -> Term -> Ty
typeof ctx = \case
  -- 8-1. T-TRUE
  TmTrue -> TyBool
  -- 8-1. T-FALSE
  TmFalse -> TyBool
  -- 8-1. T-IF
  TmIf t1 t2 t3 ->
    if typeof ctx t1 == TyBool
      then
        let tyT2 = typeof ctx t2
         in if eqType tyT2 (typeof ctx t3)
              then tyT2
              else error "arms of conditional have different types"
      else error "guard of conditional not a boolean"
  -- 8-2. T-ZERO
  TmZero -> TyNat
  -- 8-2. T-SUCC
  TmSucc t ->
    case typeof ctx t of
      TyNat -> TyNat
      _ -> error "nat type expected"
  -- 8-2. T-PRED
  TmPred t ->
    case typeof ctx t of
      TyNat -> TyNat
      _ -> error "nat type expected"
  -- 8-2. T-ISZERO
  TmIsZero t ->
    case typeof ctx t of
      TyNat -> TyBool
      _ -> error "nat type expected"
  -- 9-1. T-VAR
  TmVar _varName i -> getTypeFromContext ctx i
  -- 9-1. T-ABS
  TmLam x tyT1 t2 ->
    let ctx' = addContext (TermVarBind x tyT1) ctx
        tyT2 = typeof ctx' t2
     in TyArr tyT1 tyT2
  -- 9-1. T-APP
  TmApp t1 t2 ->
    let tyT1 = typeof ctx t1
        tyT2 = typeof ctx t2
     in case tyT1 of
          TyArr tyT11 tyT12
            | eqType tyT2 tyT11 -> tyT12
            | otherwise -> error $ "T-APP: parameter type mismatch: " <> show tyT11
          _ -> error "arrow type expected"
  -- 11-12. T-FIX
  TmFix t1 ->
    case typeof ctx t1 of
      TyArr ty1 ty2 -> if eqType ty1 ty2 then ty1 else error $ "T-FIX: " <> show ty1
      e -> error $ "T-FIX: " <> show e
  -- 11-13. T-NIL
  TmNil -> TyForAll "X" . TyList $ TyVar "X" 0
  -- 11-13. T-CONS
  TmCons ->
    TyForAll "X" . TyArr (TyVar "X" 0) $ TyArr (TyList $ TyVar "X" 0) (TyList $ TyVar "X" 0)
  -- 11-13. T-ISNIL
  TmIsNil -> TyForAll "X" $ TyArr (TyList $ TyVar "X" 0) TyBool
  -- 11-13. T-HEAD
  TmHead -> TyForAll "X" $ TyArr (TyList $ TyVar "X" 0) (TyVar "X" 0)
  -- 11-13. T-TAIL
  TmTail -> TyForAll "X" $ TyArr (TyList $ TyVar "X" 0) (TyList $ TyVar "X" 0)
  -- 23-1. T-TABS
  TmTypeLam tyVar t2 ->
    let ctx' = addContext (TypeVarBind tyVar) ctx
        ty2 = typeof ctx' t2
     in TyForAll tyVar ty2
  -- 23-1. T-TAPP
  TmTypeApp t1 ty2 ->
    case typeof ctx t1 of
      TyForAll _tyVar ty12 -> substT 0 (Left ty2) ty12
      _ -> error "T-TAPP"

getTypeFromContext :: Context -> Int -> Ty
getTypeFromContext ctx i = case getBinding ctx i of
  (TermVarBind _ tyT) -> tyT
  _ -> error "getTypeFromContext"

getBinding :: Context -> Int -> Binding
getBinding ctx i = ctx' L.Partial.!! i
  where
    ctx' = unCtx ctx

-- FIXME
-- erase :: TypedLambda -> UntypedLambda
-- erase (TmVar x) = Untyped.TmVar x
-- erase (TmAbs x _tyT1 t2) = Untyped.TmLam x (erase t2)
-- erase (TmApp t1 t2) = Untyped.TmApp (erase t1) (erase t2)

eqType :: Ty -> Ty -> Bool
eqType (TyVar x1 _) (TyVar x2 _) = x1 == x2
eqType (TyArr ty11 ty12) (TyArr ty21 ty22) = eqType ty11 ty21 && eqType ty12 ty22
eqType (TyList ty1) (TyList ty2) = eqType ty1 ty2
eqType (TyForAll x1 ty1) (TyForAll x2 ty2) = x1 == x2 && eqType ty1 ty2
eqType ty1 ty2 = ty1 == ty2
