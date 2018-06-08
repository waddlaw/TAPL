{-# LANGUAGE ViewPatterns #-}
module Language.NB
  ( module Language.NB.Types
  , isNumericalVal
  , isVal
  , eval
  ) where

import           Language.NB.Types

eval :: Term -> Term
eval t =
  case eval1 t of
    Left _   -> t
    Right t' -> eval t'

eval1 :: Term -> Either TmError Term
eval1 (TmIf TmTrue  t2 _t3)                          = Right t2
eval1 (TmIf TmFalse _t2 t3)                          = Right t3
eval1 (TmIf t1 t2 t3)                                = TmIf   <$> eval1 t1 <*> pure t2 <*> pure t3
eval1 (TmSucc t1)                                    = TmSucc <$> eval1 t1
eval1 (TmPred TmZero)                                = Right TmZero
eval1 (TmPred (TmSucc nv1@(isNumericalVal -> True))) = Right nv1
eval1 (TmPred t1)                                    = TmPred <$> eval1 t1
eval1 (TmIsZero TmZero)                              = Right TmTrue
eval1 (TmIsZero (TmSucc (isNumericalVal -> True)))   = Right TmFalse
eval1 (TmIsZero t1)                                  = TmIsZero <$> eval1 t1
eval1 _                                              = Left NoRuleApplies

isNumericalVal :: Term -> Bool
isNumericalVal TmZero      = True
isNumericalVal (TmSucc t1) = isNumericalVal t1
isNumericalVal _           = False

isVal :: Term -> Bool
isVal TmTrue                   = True
isVal TmFalse                  = True
isVal (isNumericalVal -> True) = True
isVal _                        = False