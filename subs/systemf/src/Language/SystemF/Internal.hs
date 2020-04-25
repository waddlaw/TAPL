module Language.SystemF.Internal
  ( isValue,
    isNumericValue,
    mkN,
  )
where

import Language.SystemF.Types
import RIO

-- | predicate that determines whether a given term is a value
isValue :: Term -> Bool
isValue = \case
  -- 3-1. true value
  TmTrue -> True
  -- 3-1. false value
  TmFalse -> True
  -- 9-1. abstraction value
  TmLam {} -> True
  -- 11-13. empty list
  TmNil -> True
  -- 11-13. list constructor
  TmApp (TmApp TmCons t1) t2 -> isValue t1 && isValue t2
  -- 23-1. type abstraction value
  TmTypeLam {} -> True
  -- 3-2. numeric value
  t -> isNumericValue t

-- | A predicate to determine whether a given term is a number term.
isNumericValue :: Term -> Bool
isNumericValue = \case
  -- 3-2. zero value
  TmZero -> True
  -- 3-2. successor value
  TmSucc t -> isNumericValue t
  _ -> False

mkN :: Int -> Term
mkN 0 = TmZero
mkN n = TmSucc $ mkN (n -1)
