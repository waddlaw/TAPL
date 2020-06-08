module Language.NB.Eval (eval, eval', evalTrace) where

import Language.NB.Type
import RIO

eval :: Term -> Term
eval t
  | isValue t = t
  | otherwise = eval (eval' t)

evalTrace :: Term -> [Term]
evalTrace t
  | isValue t = [t]
  | otherwise = t : evalTrace (eval' t)

eval' :: Term -> Term
eval' = \case
  -- E-IFTRUE
  TmIf TmTrue t2 _t3 -> t2
  -- E-IFFALSE
  TmIf TmFalse _t2 t3 -> t3
  -- E-IF
  TmIf t1 t2 t3 -> TmIf (eval' t1) t2 t3
  -- E-SUCC
  TmSucc t1 -> TmSucc $ eval' t1
  -- E-PREDZERO
  TmPred TmZero -> TmZero
  -- E-PREDSUCC
  (TmPred (TmSucc nv1@(isNumericalVal -> True))) -> nv1
  -- E-PRED
  TmPred t1 -> TmPred $ eval' t1
  -- E-ISZEROZERO
  TmIsZero TmZero -> TmTrue
  -- E-ISZEROSUCC
  TmIsZero (TmSucc (isNumericalVal -> True)) -> TmFalse
  -- E-ISZERO
  TmIsZero t1 -> TmIsZero $ eval' t1
  _ -> error "fail: eval'"

isValue :: Term -> Bool
isValue = \case
  -- true value
  TmTrue -> True
  -- false value
  TmFalse -> True
  t -> isNumericalVal t

isNumericalVal :: Term -> Bool
isNumericalVal = \case
  -- zero value
  TmZero -> True
  -- successor value
  TmSucc t1 -> isNumericalVal t1
  _ -> False
