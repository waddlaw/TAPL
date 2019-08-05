module Language.B.Example
  ( example
    )
where

import Language.B.Types

example :: EvalRelation
example = EvalRelation (TmIf t TmFalse TmFalse, TmIf u TmFalse TmFalse)

s , t, u :: Term
s = TmIf TmTrue TmFalse TmFalse

t = TmIf s TmTrue TmTrue

u = TmIf TmFalse TmTrue TmTrue
