{-# LANGUAGE NoImplicitPrelude #-}
module Language.FullSimpleLambda.TypeCheck
  ( typeof
  ) where

import           RIO
import qualified RIO.List.Partial                as L.Partial

import           Language.FullSimpleLambda.Types

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
                         else error "parameter type mismatch (T-APP)"
    _ -> error "arrow type expected (T-APP)"
  where
    tyT1 = typeof ctx t1
    tyT2 = typeof ctx t2
typeof _ TmTrue = TyBool      -- T-TRUE
typeof _ TmFalse = TyBool     -- T-FALSE
typeof ctx (TmIf t1 t2 t3) =  -- T-IF
    if typeof ctx t1 == TyBool
    then if tyT2 == typeof ctx t3
        then tyT2
        else error "arms of conditional have different types (T-IF)"
    else error "guard of conditional not a boolean (T-IF)"
  where
    tyT2 = typeof ctx t2
typeof _ TmZero = TyNat  -- T-ZERO
typeof ctx (TmSucc t)    -- T-SUCC
  | typeof ctx t == TyNat = TyNat
  | otherwise = error "type mismatch (T-SUCC)"
typeof ctx (TmPred t)    -- T-PRED
  | typeof ctx t == TyNat = TyNat
  | otherwise = error "type mismatch (T-PRED)"
typeof ctx (TmIsZero t)  -- T-ISZERO
  | typeof ctx t == TyNat = TyBool
  | otherwise = error "type mismatch (T-ISZERO)"
typeof _ TmUnit = TyUnit -- T-UNIT
typeof ctx (TmSeq t1 t2) -- T-SEQ
  | typeof ctx t1 == TyUnit = typeof ctx t2
  | otherwise = error "type mismatch (T-SEQ)"
typeof ctx (TmWildcard tyT1 t2) = TyArr tyT1 (typeof ctx t2)  -- T-WILDCARD
typeof ctx (TmAscribe t1 tyT) -- T-ASCRIBE
  | tyT == typeof ctx t1 = tyT
  | otherwise = error "ascribe type mismatch error (T-ASCRIBE)"
typeof ctx (TmLet var t1 t2) = typeof ctx' t2 -- T-LET
  where
    tyT1 = typeof ctx t1
    ctx' = addBinding ctx var (VarBind tyT1)
typeof ctx (TmPair t1 t2) = TyProd (typeof ctx t1) (typeof ctx t2) -- T-PAIR
typeof ctx (TmPairFst t) = -- T-PORJ1
  case typeof ctx t of
    TyProd t11 _t12 -> t11
    _               -> error "type mismatch (T-PORJ1)"
typeof ctx (TmPairSnd t) = -- T-PROJ2
  case typeof ctx t of
    TyProd _t11 t12 -> t12
    _               -> error "type mismatch (T-PROJ2)"
typeof ctx (TmTuple ts) = TyTuple $ map (typeof ctx) ts -- T-TUPLE
typeof ctx (TmTupleProj j t) = -- T-PROJ
  case typeof ctx t of
    TyTuple tys -> tys L.Partial.!! j
    _           -> error "type mismatch (T-PROJ)"

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
