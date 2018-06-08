{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.B
  ( deduce
  , step
  , reduction
  , true
  , false
  ) where

import           Language.B.Types

import           Data.Maybe

-- | 項の評価関係
--
-- 評価関係は t -> t' と書かれ、「tが1ステップでt'に評価される」と読む
reduction :: Maybe Premise -> Term -> Term
reduction Nothing (TmIf TmTrue  t2 t3) = t2                 -- E-IFTRUE
reduction Nothing (TmIf TmFalse t2 t3) = t3                 -- E-IFFALSE
reduction (Just premise) (TmIf t1 t2 t3) = TmIf t1' t2 t3   -- E-IF
  where
    t1' = fromMaybe (error msg) $ lookup t1 [unwrap premise]
    msg = show t1 ++ " -> ???"

deduce :: Rule -> Conclusion -> Maybe Premise
deduce E_IFTRUE (EvalRelation (t, t')) =
  let TmIf TmTrue t2 t3 = t
  in  if t2 == t' then Nothing else error "unify mismatch"
deduce E_IFFALSE (EvalRelation (t, t')) =
  let TmIf TmFalse t2 t3 = t
  in  if t3 == t' then Nothing else error "unify mismatch"
deduce E_IF (EvalRelation (t, t')) =
  let TmIf t1 t2 t3 = t
      TmIf t1' t2' t3' = t'
  in  if t2 == t2' && t3 == t3' then Just (EvalRelation (t1, t1')) else error "unify mismatch"

step :: Rule -> Maybe Premise -> Maybe Premise
step rule = maybe Nothing (deduce rule)

true, false :: Term
true  = TmTrue
false = TmFalse
