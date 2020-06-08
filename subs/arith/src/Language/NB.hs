module Language.NB
  ( module Language.NB.Parser,
    module Language.NB.Type,
    isVal,
    isNumericalVal,
    eval,
  )
where

import Language.NB.Parser
import Language.NB.Type
import RIO

eval :: Term -> Term
eval t = case eval1 t of
  Left _ -> t
  Right t' -> eval t'

eval1 :: Term -> Either TmError Term
eval1 = \case
  TmIf TmTrue t2 _t3 -> Right t2
  TmIf TmFalse _t2 t3 -> Right t3
  TmIf t1 t2 t3 -> TmIf <$> eval1 t1 <*> pure t2 <*> pure t3
  TmSucc t1 -> TmSucc <$> eval1 t1
  TmPred TmZero -> Right TmZero
  (TmPred (TmSucc nv1@(isNumericalVal -> True))) -> Right nv1
  TmPred t1 -> TmPred <$> eval1 t1
  TmIsZero TmZero -> Right TmTrue
  TmIsZero (TmSucc (isNumericalVal -> True)) -> Right TmFalse
  TmIsZero t1 -> TmIsZero <$> eval1 t1
  _ -> Left NoRuleApplies

isVal :: Term -> Bool
isVal = \case
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
