module Language.B
  ( module Language.B.Parser
  , module Language.B.Types
  , deduce
  , step
  , reduction
  )
where

import Language.B.Parser
import Language.B.Types

import RIO

-- | evaluation
reduction :: Maybe Premise -> Term -> Term
reduction Nothing (TmIf TmTrue t2 _t3)   = t2             -- E-IFTRUE
reduction Nothing (TmIf TmFalse _t2 t3)  = t3             -- E-IFFALSE
reduction (Just premise) (TmIf t1 t2 t3) = TmIf t1' t2 t3 -- E-IF
  where
    t1' = fromMaybe (error msg) $ lookup t1 [unwrap premise]
    msg = show t1 ++ " -> ???"
reduction _ _ = error "Nothing reduction rule"

deduce :: Rule -> Conclusion -> Maybe Premise
deduce E_IFTRUE (EvalRelation (TmIf TmTrue t2 _t3, t'))
  | t2 == t'  -> Nothing
  | otherwise -> error "can not deduce"
deduce E_IFFALSE (EvalRelation (TmIf TmFalse _t2 t3, t'))
  | t3 == t'  -> Nothing
  | otherwise -> error "can not deduce"
deduce E_IF (EvalRelation (TmIf t1 t2 t3, TmIf t1' t2' t3'))
  | t2 == t2' && t3 == t3' -> Just (EvalRelation (t1, t1'))
  | otherwise              -> error "can not deduce"
deduce _ _ = error "can not deduce"

step :: Rule -> Maybe Premise -> Maybe Premise
step rule mpremise = mpremise >>= deduce rule
