{-# LANGUAGE OverloadedStrings #-}

module Language.FullSimpleLambda.TypeCheck
  ( typeof
  )
where

import Language.FullSimpleLambda.Types

import RIO
import qualified RIO.List.Partial as List.Partial

typeof :: Context -> Term -> Ty
typeof ctx (TmVar i) = getTypeFromContext ctx i -- T-VAR
typeof ctx (TmLam x tyT1 t2) = TyArr tyT1 tyT2 -- T-ABS
  where
    tyT2 = typeof ctx' t2
    ctx' = addBinding ctx x (VarBind tyT1)
typeof ctx (TmApp t1 t2) =
  -- T-APP
  case tyT1 of
    TyArr tyT11 tyT12 ->
      if tyT2 == tyT11
        then tyT12
        else
          error
            $ unlines
                [ "parameter type mismatch (T-APP): ",
                  "tyT2: " <> show tyT2,
                  "tyT11: " <> show tyT11
                  ]
    _ -> error "arrow type expected (T-APP)"
  where
    tyT1 = typeof ctx t1
    tyT2 = typeof ctx t2
typeof _ TmTrue = TyBool -- T-TRUE
typeof _ TmFalse = TyBool -- T-FALSE
typeof ctx (TmIf t1 t2 t3) =
  -- T-IF
  if typeof ctx t1 == TyBool
    then
      if tyT2 == typeof ctx t3
        then tyT2
        else error "arms of conditional have different types (T-IF)"
    else error "guard of conditional not a boolean (T-IF)"
  where
    tyT2 = typeof ctx t2
typeof _ TmZero = TyNat -- T-ZERO
typeof ctx (TmSucc t) -- T-SUCC
  | typeof ctx t == TyNat = TyNat
  | otherwise = error "type mismatch (T-SUCC)"
typeof ctx (TmPred t) -- T-PRED
  | typeof ctx t == TyNat = TyNat
  | otherwise = error "type mismatch (T-PRED)"
typeof ctx (TmIsZero t) -- T-ISZERO
  | typeof ctx t == TyNat = TyBool
  | otherwise = error "type mismatch (T-ISZERO)"
typeof _ TmUnit = TyUnit -- T-UNIT
typeof ctx (TmSeq t1 t2) -- T-SEQ
  | typeof ctx t1 == TyUnit = typeof ctx t2
  | otherwise = error "type mismatch (T-SEQ)"
typeof ctx (TmWildcard tyT1 t2) = TyArr tyT1 (typeof ctx t2) -- T-WILDCARD
typeof ctx (TmAscribe t1 tyT) -- T-ASCRIBE
  | tyT == typeof ctx t1 = tyT
  | otherwise = error "ascribe type mismatch error (T-ASCRIBE)"
typeof ctx (TmLet var t1 t2) = typeof ctx' t2 -- T-LET
  where
    tyT1 = typeof ctx t1
    ctx' = addBinding ctx var (VarBind tyT1)
typeof ctx (TmPair t1 t2) = TyProd (typeof ctx t1) (typeof ctx t2) -- T-PAIR
typeof ctx (TmPairFst t) =
  -- T-PORJ1
  case typeof ctx t of
    TyProd t11 _t12 -> t11
    _ -> error "type mismatch (T-PORJ1)"
typeof ctx (TmPairSnd t) =
  -- T-PROJ2
  case typeof ctx t of
    TyProd _t11 t12 -> t12
    _ -> error "type mismatch (T-PROJ2)"
typeof ctx (TmTuple ts) = TyTuple $ map (typeof ctx) ts -- T-TUPLE
typeof ctx (TmTupleProj j t) =
  -- T-PROJ
  case typeof ctx t of
    TyTuple tys -> tys List.Partial.!! j
    _ -> error "type mismatch (T-PROJ)"
typeof ctx (TmRecord fields) = TyRecord $ map (second (typeof ctx)) fields -- T-RCD
typeof ctx (TmRecordProj label t) =
  -- T-RECORDPROJ
  case typeof ctx t of
    TyRecord fields -> fromMaybe (error "field label not found (T-RECORDPROJ)") $ lookup label fields
    _ -> error "type mismatch (T-RECORDPROJ)"
typeof ctx (TmPattern p t1 t2) = typeof ctx' t2 -- T-LET (Pattern)
  where
    ty1 = typeof ctx t1
    ctx' = ctx <> delta p ty1
typeof ctx (TmInL t ty@(TySum tyL _tyR)) -- T-INL (Sum)
  | typeof ctx t == tyL = ty
  | otherwise = error "type mismatch (T-INL)"
typeof _ (TmInL _ _) = error "type mismatch (T-INL)"
typeof ctx (TmInR t ty@(TySum _tyL tyR)) -- T-INR (Sum)
  | tyR == typeof ctx t = ty
  | otherwise = error "type mismatch (T-INR)"
typeof _ (TmInR _ _) = error "type mismatch (T-INR)"
typeof ctx (TmCase t0 (TmVar x1, t1) (TmVar x2, t2)) =
  -- T-CASE
  if tyT1 == tyT2
    then tyT1
    else error "type mismatch (T-CASE)"
  where
    (ty1, ty2) = case typeof ctx t0 of
      (TySum ty1' ty2') -> (ty1', ty2')
      _ -> error "type mismatch (T-CASE)"
    ctx1 = addBinding ctx ("FV" <> tshow x1) (VarBind ty1)
    ctx2 = addBinding ctx ("FV" <> tshow x2) (VarBind ty2)
    tyT1 = typeof ctx1 t1
    tyT2 = typeof ctx2 t2
typeof _ TmCase {} = error "Only variables can appear on the left side of the body of the Case"

delta :: Pattern -> Ty -> Context
delta (PtVar varName _) ty = addContext (VarContext varName, VarBind ty) mempty
delta (PtRecord pfs) ty =
  case ty of
    TyRecord tfs -> foldMap (\(p, ty') -> addContext (PatternContext p, PatternBind ty') mempty) $ zip (map snd pfs) (map snd tfs)
    _ -> error "delta: expected Record term"

----------------------
-- helper functions --
----------------------

-- VarContext only
addBinding :: Context -> Text -> Binding -> Context
addBinding ctx x bind = addContext (VarContext x, bind) ctx

getTypeFromContext :: Context -> Int -> Ty
getTypeFromContext ctx i =
  case getBinding ctx i of
    VarBind tyT     -> tyT
    PatternBind tyT -> tyT
    _               -> error "getTypeFromContext"

getBinding :: Context -> Int -> Binding
getBinding ctx i = snd $ ctx' List.Partial.!! i
  where
    ctx' = unCtx ctx
