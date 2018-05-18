#!/usr/bin/env stack
{- stack repl
   --resolver lts-11.9
   --package QuickCheck
   --package containers
   --package tasty
   --package tasty-hunit
-}

import           Data.Set         (Set)
import qualified Data.Set         as Set
import           Prelude          hiding (False, True)
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit

-- | def 3.2.1 [帰納的な項の定義]
-- 項の集合とは以下の条件を満たす最小の集合Tである
-- 「最小の」という言葉は T が3つの条件によって要求される要素以外を持たないという意味。
data Term
  = True      | False     | Zero        -- ^ (1)
  | Succ Term | Pred Term | IsZero Term -- ^ (2)
  | If Term Term Term                   -- ^ (3)
  deriving (Eq, Ord, Show)

type T = Set Term

{- GADT で書いた方が見通しが良い気がする
data Term where
  True   :: Term
  False  :: Term
  Succ   :: Term -> Term
  Pred   :: Term -> Term
  IsZero :: Term -> Term
  If     :: Term -> Term -> Term
-}

-- | def 3.2.3 [具体的な項の定義]
-- 各自然数 i について、集合 Si を以下のように定義する。
-- 簡略化のため Int を Nat として扱う。エラー処理は実装しない。
s :: Int -> T
s 0 = Set.empty
s i = Set.unions [s1, s2, s3]
  where
    s1 = Set.fromList [True, False, Zero]
    s2 = Set.fromList $ concat [[Succ t1, Pred t1, IsZero t1] | t1 <- si]
    s3 = Set.fromList [If t1 t2 t3 | t1 <- si, t2 <- si, t3 <- si]
    si = Set.toList $ s (i-1)

-- | def 3.3.1 項tに現れる定数の集合を Consts(t) と書き、次のように定義する
consts :: Term -> T
consts True          = Set.singleton True
consts False         = Set.singleton False
consts Zero          = Set.singleton Zero
consts (Succ t)      = consts t
consts (Pred t)      = consts t
consts (IsZero t)    = consts t
consts (If t1 t2 t3) = Set.unions $ map consts [t1, t2, t3]

-- | def 3.3.2 項tのサイズを size(t) と書き、次のように定義する
size :: Term -> Int
size True          = 1
size False         = 1
size Zero          = 1
size (Succ t)      = size t + 1
size (Pred t)      = size t + 1
size (IsZero t)    = size t + 1
size (If t1 t2 t3) = size t1 + size t2 + size t3 + 1

-- | 項tの深さを depth(t) と書き、次のように定義する
depth :: Term -> Int
depth True          = 1
depth False         = 1
depth Zero          = 1
depth (Succ t)      = depth t + 1
depth (Pred t)      = depth t + 1
depth (IsZero t)    = depth t + 1
depth (If t1 t2 t3) = maximum [depth t1, depth t2, depth t3] + 1

inv01 :: Term -> Bool
inv01 t = Set.member t (s i)
  where i = depth t

minT :: Term -> T
minT = s . depth

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "Set.size (s n)" $ do
      (Set.size $ s 0) @?= 0
      (Set.size $ s 1) @?= 3
      (Set.size $ s 2) @?= 39
      (Set.size $ s 3) @?= 59439
  ]

instance Arbitrary Term where
  arbitrary = do
    t1 <- arbitrary
    t2 <- arbitrary
    t3 <- arbitrary
    elements [ True, False, Zero
             , Succ t1, Pred t1, IsZero t1
             , If t1 t2 t3
             ]
