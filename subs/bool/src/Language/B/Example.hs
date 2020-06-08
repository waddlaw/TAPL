module Language.B.Example (s, t, u) where

import Language.B.Type

s, t, u :: Term
s = TmIf TmTrue TmFalse TmFalse
t = TmIf s TmTrue TmTrue
u = TmIf TmFalse TmTrue TmTrue
