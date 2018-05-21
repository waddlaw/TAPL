{-# LANGUAGE MonadComprehensions #-}
module Term.MonadSet
  ( module Term.Types
  , s
  , consts
  , size
  , depth
  , minT
  ) where

import           Term.Types

import           Data.Set.Monad (Set)
import qualified Data.Set.Monad as MonadSet

type T = Set Term

{- |
[定義 3.2.3 具体的な項の定義]:
各自然数 i について、集合 Si を以下のように定義する。

>>> s 0
fromList []

>>> s 1
fromList [TTrue,TFalse,Zero]
-}
-- 簡略化のため Int を Nat として扱う。エラー処理は実装しない。
s :: Int -> T
s 0 = MonadSet.empty
s i = MonadSet.unions [s1, s2, s3]
  where
    s1 = MonadSet.fromList [TTrue, TFalse, Zero]
    s2 = MonadSet.unions
          [ [ Succ t1   | t1 <- si ]
          , [ Pred t1   | t1 <- si ]
          , [ IsZero t1 | t1 <- si ]
          ]
    s3 = [If t1 t2 t3 | t1 <- si, t2 <- si, t3 <- si]
    si = s (i-1)

{- |
[定義 3.3.1 項tに現れる定数の集合を Consts(t) と書き、次のように定義する]:

>>> consts TTrue
fromList [TTrue]

>>> consts (Succ (Succ (Succ Zero)))
fromList [Zero]
-}
consts :: Term -> T
consts TTrue         = MonadSet.singleton TTrue
consts TFalse        = MonadSet.singleton TFalse
consts Zero          = MonadSet.singleton Zero
consts (Succ t)      = consts t
consts (Pred t)      = consts t
consts (IsZero t)    = consts t
consts (If t1 t2 t3) = MonadSet.unions $ map consts [t1, t2, t3]

{- |
[定義 3.3.2 項tのサイズを size(t) と書き、次のように定義する]:

>>> size Zero
1

>>> size (Succ (Succ (Succ Zero)))
4

>>> size (If Zero (Succ (Succ Zero)) (Succ Zero))
7
-}
size :: Term -> Int
size TTrue         = 1
size TFalse        = 1
size Zero          = 1
size (Succ t)      = size t + 1
size (Pred t)      = size t + 1
size (IsZero t)    = size t + 1
size (If t1 t2 t3) = size t1 + size t2 + size t3 + 1

{- |
[項tの深さを depth(t) と書き、次のように定義する]:

>>> depth Zero
1

>>> depth (Succ (Succ (Succ Zero)))
4

>>> depth (If Zero (Succ (Succ Zero)) (Succ Zero))
4
-}
depth :: Term -> Int
depth TTrue         = 1
depth TFalse        = 1
depth Zero          = 1
depth (Succ t)      = depth t + 1
depth (Pred t)      = depth t + 1
depth (IsZero t)    = depth t + 1
depth (If t1 t2 t3) = maximum [depth t1, depth t2, depth t3] + 1

-- | 与えられた Term が含まれる、最小の集合 T を作る
--
-- >>> minT Zero
-- fromList [TTrue,TFalse,Zero]
minT :: Term -> T
minT = s . depth
