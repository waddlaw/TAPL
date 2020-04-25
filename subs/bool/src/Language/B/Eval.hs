module Language.B.Eval (eval, eval', evalTrace) where

import Language.B.Type
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
  TmIf TmTrue t2 _ -> t2
  -- E-IFFALSE
  TmIf TmFalse _ t3 -> t3
  -- E-IF
  TmIf t1 t2 t3 -> TmIf (eval' t1) t2 t3
  t ->
    if  | isValue t -> t
        | otherwise -> error "fail: eval'"

isValue :: Term -> Bool
isValue = \case
  -- true value
  TmTrue -> True
  -- false value
  TmFalse -> True
  _ -> False
