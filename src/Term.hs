module Term
  ( s
  , consts
  , size
  , depth
  , module Term.Types
  , minT
  ) where

import           Term.Types

import           Data.Set   (Set)
import qualified Data.Set   as Set

type T = Set Term

-- | def 3.2.3 [具体的な項の定義]
-- 各自然数 i について、集合 Si を以下のように定義する。
-- 簡略化のため Int を Nat として扱う。エラー処理は実装しない。
s :: Int -> T
s 0 = Set.empty
s i = Set.unions [s1, s2, s3]
  where
    s1 = Set.fromList [TTrue, TFalse, Zero]
    s2 = Set.fromList $ concat [[Succ t1, Pred t1, IsZero t1] | t1 <- si]
    s3 = Set.fromList [If t1 t2 t3 | t1 <- si, t2 <- si, t3 <- si]
    si = Set.toList $ s (i-1)

-- | def 3.3.1 項tに現れる定数の集合を Consts(t) と書き、次のように定義する
consts :: Term -> T
consts TTrue         = Set.singleton TTrue
consts TFalse        = Set.singleton TFalse
consts Zero          = Set.singleton Zero
consts (Succ t)      = consts t
consts (Pred t)      = consts t
consts (IsZero t)    = consts t
consts (If t1 t2 t3) = Set.unions $ map consts [t1, t2, t3]

-- | def 3.3.2 項tのサイズを size(t) と書き、次のように定義する
size :: Term -> Int
size TTrue         = 1
size TFalse        = 1
size Zero          = 1
size (Succ t)      = size t + 1
size (Pred t)      = size t + 1
size (IsZero t)    = size t + 1
size (If t1 t2 t3) = size t1 + size t2 + size t3 + 1

-- | 項tの深さを depth(t) と書き、次のように定義する
depth :: Term -> Int
depth TTrue         = 1
depth TFalse        = 1
depth Zero          = 1
depth (Succ t)      = depth t + 1
depth (Pred t)      = depth t + 1
depth (IsZero t)    = depth t + 1
depth (If t1 t2 t3) = maximum [depth t1, depth t2, depth t3] + 1

minT :: Term -> T
minT = s . depth
