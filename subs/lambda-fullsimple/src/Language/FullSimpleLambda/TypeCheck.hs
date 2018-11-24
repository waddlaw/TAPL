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
typeof _ TmZero = TyNat -- T-ZERO
typeof ctx (TmSucc t) = if typeof ctx t == TyNat then TyNat else error "type mismatch" -- T-SUCC
typeof ctx (TmPred t) = if typeof ctx t == TyNat then TyNat else error "type mismatch" -- T-PRED
typeof ctx (TmIsZero t) = if typeof ctx t == TyNat then TyBool else error "type mismatch" -- T-ISZERO
typeof _ TmUnit = TyUnit  -- T-UNIT
typeof ctx (TmSeq t1 t2) = if typeof ctx t1 == TyUnit then typeof ctx t2 else error "type mismatch" -- T-SEQ
typeof ctx (TmWildcard tyT1 t2) = TyArr tyT1 (typeof ctx t2)  -- T-WILDCARD
typeof ctx (TmAscribe t1 tyT) -- T-ASCRIBE
  | tyT == typeof ctx t1 = tyT
  | otherwise = error "ascribe type mismatch error"
typeof ctx (TmLet var t1 t2) = typeof ctx' t2 -- T-LET
  where
    tyT1 = typeof ctx t1
    ctx' = addBinding ctx var (VarBind tyT1)
typeof ctx (TmPair t1 t2) = TyProd (typeof ctx t1) (typeof ctx t2) -- T-PAIR
typeof ctx (TmPairFst t) = typeof ctx t -- T-PORJ1
typeof ctx (TmPairSnd t) = typeof ctx t -- T-PROJ2

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
