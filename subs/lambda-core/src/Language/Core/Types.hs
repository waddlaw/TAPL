module Language.Core.Types
  ( Strategy (..),
    Prelude,
  )
where

import RIO

type Prelude lang = Map Text lang

data Strategy
  = FullBetaReduction
  | NormalOrder
  | CallByName
  | CallByValue
  deriving stock (Show, Read, Enum, Bounded)
