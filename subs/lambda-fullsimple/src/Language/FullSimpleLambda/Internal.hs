module Language.FullSimpleLambda.Internal
  ( isValue,
    isNumericValue,
    isRecordValue
    )
where

import Language.FullSimpleLambda.Types

import RIO

-- | predicate that determines whether a given term is a value
isValue :: Term -> Bool
isValue = \case
  TmLam{}      -> True
  TmTrue       -> True
  TmFalse      -> True
  TmUnit       -> True                     -- 11.2 constant unit
  TmPair t1 t2 -> isValue t1 && isValue t2 -- 11.5 pair value
  TmTuple ts   -> all isValue ts           -- 11.6 tuple value
  TmRecord fs  -> all (isValue . snd) fs   -- 11.7 record value
  TmInR t _    -> isValue t                -- 11.9 tagged value (left)
  TmInL t _    -> isValue t                -- 11.9 tagged value (right)
  t            -> isNumericValue t

-- | Determining whether a given term is a number term
isNumericValue :: Term -> Bool
isNumericValue = \case
  TmZero   -> True
  TmSucc t -> isNumericValue t
  _        -> False

-- | Determine if a given term is both a record and a value
isRecordValue :: Term -> Bool
isRecordValue = \case
  t@TmRecord {} -> isValue t
  _             -> False
