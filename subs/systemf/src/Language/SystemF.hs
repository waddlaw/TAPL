module Language.SystemF
  ( module Language.SystemF.Eval,
    module Language.SystemF.Types,
    module Language.SystemF.Parser,
    module Language.SystemF.Pretty,
    module Language.SystemF.Example,
    typeof
    )
where

import Language.SystemF.Example
import Language.SystemF.Eval
import Language.SystemF.Parser
import Language.SystemF.Pretty
import Language.SystemF.Types
import RIO
import qualified RIO.List.Partial as L.Partial

typeof :: Context -> Term -> Ty
typeof ctx (TmVar varName i) = getTypeFromContext ctx i
typeof ctx (TmLam x tyT1 t2) = TyArr tyT1 tyT2
  where
    tyT2 = typeof ctx' t2
    ctx' = addContext (TermVarBind x tyT1) ctx
typeof ctx (TmApp t1 t2) = case tyT1 of
  TyArr tyT11 tyT12 ->
    if tyT2 == tyT11
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
    then
      if tyT2 == typeof ctx t3
        then tyT2
        else error "arms of conditional have different types"
    else error "guard of conditional not a boolean"
  where
    tyT2 = typeof ctx t2

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
