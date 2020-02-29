module Language.SystemF.Eval
  ( eval,
    evaluate,
    substT,
  ) where

import Data.Either
import Data.Function
import Language.SystemF.Internal
import Language.SystemF.Types
import RIO hiding (evaluate)

evaluate :: Term -> Value
evaluate t
  | isValue t = t
  | otherwise = evaluate (eval t)

eval :: Term -> Term
eval = \case
  -- 3-1. E-IFTRUE
  TmIf TmTrue t2 _ -> t2
  -- 3-1. E-IFFALSE
  TmIf TmFalse _t2 t3 -> t3
  -- 3-1. E-IF
  TmIf t1 t2 t3 -> TmIf (eval t1) t2 t3

  -- 3-2. E-SUCC
  TmSucc t -> TmSucc (eval t)
  -- 3-2. E-PREDZERO
  TmPred TmZero -> TmZero
  -- 3-2. E-PREDSUCC
  TmPred (TmSucc nv@(isNumericValue -> True)) -> nv
  -- 3-2. E-PRED
  TmPred t1 -> TmPred (eval t1)
  -- 3-2. E-ISZEROZERO
  TmIsZero TmZero -> TmTrue
  -- 3-2. E-ISZEROSUCC
  TmIsZero (TmSucc (isNumericValue -> True)) -> TmFalse
  -- 3-2. E-ISZERO
  TmIsZero t -> TmIsZero (eval t)

  -- 11-12. E-FIXBETA
  t@(TmFix (TmLam _x _tyT1 t2)) -> subst 0 (Right t) t2
  -- 11-12. E-FIX
  TmFix t1 ->
    let t1' = eval t1
     in TmFix t1'

  -- 11-13. E-CONS2
  TmApp (TmApp (TmTypeApp TmCons tyT) v1@(isValue -> True)) t2 ->
    let t2' = eval t2
     in TmApp (TmApp (TmTypeApp TmCons tyT) v1) t2'
  -- 11-13. E-CONS1
  TmApp (TmApp (TmTypeApp TmCons tyT) t1) t2 ->
    let t1' = eval t1
     in TmApp (TmApp (TmTypeApp TmCons tyT) t1') t2
  -- 11-13. E-ISNILNIL
  TmApp (TmTypeApp TmIsNil _tyS) (TmTypeApp TmNil _tyT) -> TmTrue
  -- 11-13. E-ISNILCONS
  TmApp (TmTypeApp TmIsNil _tyS) (TmApp (TmApp (TmTypeApp TmCons _tyT) _v1@(isValue -> True)) _v2@(isValue -> True)) -> TmFalse
  -- 11-13. E-ISNIL
  TmApp (TmTypeApp TmIsNil tyT) t1 ->
    let t1' = eval t1
     in TmApp (TmTypeApp TmIsNil tyT) t1'
  -- 11-13. E-HEADCONS
  TmApp (TmTypeApp TmHead _tyS) (TmApp (TmApp (TmTypeApp TmCons _tyT) v1@(isValue -> True)) _v2@(isValue -> True)) -> v1
  -- 11-13. E-HEAD
  TmApp (TmTypeApp TmHead tyT) t1 ->
    let t1' = eval t1
     in TmApp (TmTypeApp TmHead tyT) t1'
  -- 11-13. E-TAILCONS
  TmApp (TmTypeApp TmTail _tyS) (TmApp (TmApp (TmTypeApp TmCons _tyT) _v1@(isValue -> True)) v2@(isValue -> True)) -> v2
  -- 11-13. E-TAIL
  TmApp (TmTypeApp TmTail tyT) t1 ->
    let t1' = eval t1
     in TmApp (TmTypeApp TmTail tyT) t1'

  -- 23-1. E-TAPPABS
  TmTypeApp (TmTypeLam _ t12) ty2 -> shift 0 (-1) $ subst 0 (Left $ shift 0 1 ty2) t12
  -- 23-1. E-TAPP
  TmTypeApp t1 ty2 -> TmTypeApp (eval t1) ty2

  -- 9-1. E-APPABS
  TmApp (TmLam _ _ t12) v2@(isValue -> True) -> shift 0 (-1) $ subst 0 (Right $ shift 0 1 v2) t12
  -- 9-1. E-APP2
  TmApp v1@(isValue -> True) t2 -> TmApp v1 (eval t2)
  -- 9-1. E-APP1
  TmApp t1 t2 -> TmApp (eval t1) t2

  -- debug log
  x -> error $ show x

subst :: Int -> Either Ty Term -> Term -> Term
subst j s = \case
  t@(TmVar _ k)
    | k == j -> fromRight undefined s
    | otherwise -> t
  TmLam x ty t ->
    let ty' = substT (j + 1) (shift 0 1 s) ty
        t'  = subst  (j + 1) (shift 0 1 s) t
    in TmLam x ty' t'
  TmApp t1 t2 -> (TmApp `on` subst j s) t1 t2
  TmTypeLam x t -> TmTypeLam x $ subst (j + 1) (shift 0 1 s) t
  TmTypeApp t ty ->
    let ty' = substT j s ty
        t'  = subst  j s t
    in TmTypeApp t' ty'
  TmTrue -> TmTrue
  TmFalse -> TmFalse
  TmIf t1 t2 t3 -> (TmIf `on` subst j s) t1 t2 t3
  TmZero -> TmZero
  TmSucc t -> TmSucc $ subst j s t
  TmPred t -> TmPred $ subst j s t
  TmIsZero t -> TmIsZero $ subst j s t

  -- 11-12. General recursion
  TmFix t -> TmFix $ subst j s t
  
  TmNil   -> TmNil
  TmCons  -> TmCons
  TmIsNil -> TmIsNil
  TmHead  -> TmHead
  TmTail  -> TmTail

substT :: Int -> Either Ty Term -> Ty -> Ty
substT j s = \case
  TyBool -> TyBool
  TyNat -> TyNat
  TyArr ty1 ty2 -> (TyArr `on` substT j s) ty1 ty2
  TyList ty -> TyList (substT j s ty)
  ty@(TyVar _ k)
    | k == j -> typeShift j $ fromLeft undefined s
    | otherwise -> ty
  TyForAll tyVarName ty ->
    let ty' = substT (j + 1) (shift 0 1 s) ty
    in TyForAll tyVarName ty'

class DeBruijn a where
  shift :: Int -> Int -> a -> a

instance (DeBruijn a, DeBruijn b) => DeBruijn (Either a b) where
  shift c d (Left  a) = Left  (shift c d a)
  shift c d (Right b) = Right (shift c d b)

instance DeBruijn Term where
  shift c d = \case
    TmVar varName k
      | k < c     -> TmVar varName k
      | otherwise -> TmVar varName (k + d)

    TmLam x ty t ->
      let ty' = shift (c + 1) d ty
          t'  = shift (c + 1) d t
      in TmLam x ty' t'

    TmApp t1 t2 -> (TmApp `on` shift c d) t1 t2

    -- 8-1.
    TmTrue -> TmTrue
    TmFalse -> TmFalse
    TmIf t1 t2 t3 -> (TmIf `on` shift c d) t1 t2 t3

    -- 8-2. 
    TmZero     -> TmZero
    TmSucc t   -> TmSucc $ shift c d t
    TmPred t   -> TmPred $ shift c d t
    TmIsZero t -> TmIsZero $ shift c d t

    -- 11-12. General recursion
    TmFix t -> TmFix $ shift c d t

    -- 11-13. List
    TmNil   -> TmNil
    TmCons  -> TmCons
    TmIsNil -> TmIsNil
    TmHead  -> TmHead
    TmTail  -> TmTail

    TmTypeLam tyVarName t -> TmTypeLam tyVarName $ shift (c + 1) d t
    TmTypeApp t ty ->
      let ty' = shift c d ty
          t'  = shift c d t
       in TmTypeApp t' ty'

instance DeBruijn Ty where
  shift c d = \case
    TyBool                -> TyBool
    TyNat                 -> TyNat
    TyArr ty1 ty2         -> (TyArr `on` shift c d) ty1 ty2
    TyList ty             -> TyList (shift c d ty)
    TyVar tyVarName k
      | k < c             -> TyVar tyVarName k
      | otherwise         -> TyVar tyVarName (k + d)
    TyForAll tyVarName ty -> TyForAll tyVarName $ shift (c + 1) d ty

typeShift :: DeBruijn a => Int -> a -> a
typeShift = shift 0