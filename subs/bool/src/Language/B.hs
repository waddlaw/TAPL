{-# LANGUAGE FlexibleInstances #-}

module Language.B
  ( module Language.B.Parser,
    module Language.B.Types,
    deduce,
    step,
    reduction
    )
where

import Language.B.Parser
import Language.B.Types
import RIO

-- | 項の評価関係
--
-- 評価関係 t -> t' は「tが1ステップでt'に評価される」と読む
reduction :: Maybe Premise -> Term -> Term
reduction Nothing (TmIf TmTrue t2 _t3) = t2 -- E-IFTRUE
reduction Nothing (TmIf TmFalse _t2 t3) = t3 -- E-IFFALSE
reduction (Just premise) (TmIf t1 t2 t3) = TmIf t1' t2 t3 -- E-IF
  where
    t1' = fromMaybe (error msg) $ lookup t1 [unwrap premise]
    msg = show t1 ++ " -> ???"
reduction _ _ = error "No Rule"

deduce :: Rule -> Conclusion -> Maybe Premise
deduce E_IFTRUE (EvalRelation (TmIf TmTrue t2 _t3, t')) = if t2 == t' then Nothing else error "unify mismatch"
deduce E_IFFALSE (EvalRelation (TmIf TmFalse _t2 t3, t')) = if t3 == t' then Nothing else error "unify mismatch"
deduce E_IF (EvalRelation (TmIf t1 t2 t3, TmIf t1' t2' t3')) = if t2 == t2' && t3 == t3' then Just (EvalRelation (t1, t1')) else error "unify mismatch"
deduce _ _ = error "unify mismatch"

step :: Rule -> Maybe Premise -> Maybe Premise
step rule mpremise = mpremise >>= deduce rule
