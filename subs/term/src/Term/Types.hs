{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
module Term.Types
  ( Term (..)
  )
where

import RIO
import Test.QuickCheck

{- | [定義 3.2.1 帰納的な項の定義]:

* 項の集合とは3つの条件を満たす最小の集合Tである
* 「最小の」という言葉は T が3つの条件によって要求される要素以外を持たないという意味。
* True, False は Bool の値とかぶるので先頭に T をつけることにする
* 全てが Term の値であることを明示的にするため、GADT で書いたが、普通に ADT で書いても良い
-}
data Term where
  TTrue :: Term -- (1)
  TFalse :: Term -- (1)
  Zero :: Term -- (1)
  Succ :: Term -> Term -- (2)
  Pred :: Term -> Term -- (2)
  IsZero :: Term -> Term -- (2)
  If :: Term -> Term -> Term -> Term -- (3)
  deriving (Eq, Ord, Show, Generic)

instance Hashable Term

instance Arbitrary Term where
  arbitrary = do
    t1 <- arbitrary
    t2 <- arbitrary
    t3 <- arbitrary
    elements
      [ TTrue
      , TFalse
      , Zero
      , Succ t1
      , Pred t1
      , IsZero t1
      , If t1 t2 t3
      ]
