module Language.Core.Types
  ( Strategy (..)
  , Prelude
  )
where

type Prelude lang = Map Text lang

data Strategy
  = FullBetaReduction -- ^ 完全ベータ簡約
  | NormalOrder -- ^ 正規順序戦略
  | CallByName -- ^ 名前呼び戦略
  | CallByValue -- ^ 値呼び戦略
  deriving (Show, Read, Enum, Bounded)
