module Extensible.Language.UntypedLambda
  ( module Extensible.Language.UntypedLambda.Types
  , module Extensible.Language.UntypedLambda.Parser
  ) where

import           Extensible.Language.UntypedLambda.Types
import           Extensible.Language.UntypedLambda.Parser
import           Language.Utils

import Data.Extensible

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | 指定された評価戦略で項を正規系に評価する
eval :: Strategy -> Term -> Term
eval s t
  | result == t = t
  | otherwise = eval s result
  where
    result = evalOneStep s t

-- | デバッグ用
trace :: Strategy -> Term -> IO ()
trace s t = mapM_ (putStrLn . render) $ reverse $ evalWithTrace s [t] t

-- | 簡約ステップ列を返す
evalWithTrace :: Strategy -> [Term] -> Term -> [Term]
evalWithTrace s acc t
  | result == t = acc
  | otherwise = evalWithTrace s acc' result
  where
    result = evalOneStep s t
    acc'   = result:acc

-- | 簡約ステップ数を返す
steps :: Term -> Int
steps = length . evalWithTrace NormalOrder []

-- | 1ステップのみ、指定された評価戦略で評価する
evalOneStep :: Strategy -> Term -> Term
evalOneStep FullBetaReduction _ = undefined -- TODO
evalOneStep NormalOrder       t = reduceNormalOrder t
evalOneStep CallByName        t = reduceCallByName t
evalOneStep CallByValue       t = reduceCallByValue t

-- | 正規順序戦略
reduceNormalOrder :: Term -> Term
reduceNormalOrder = undefined
-- reduceNormalOrder (TmApp (TmLam x old) new)             = subst x new old
-- reduceNormalOrder (TmApp t1@(TmApp _ _) t2@(TmApp _ _)) = TmApp (reduceNormalOrder t1) (reduceNormalOrder t2)
-- reduceNormalOrder (TmApp t1@(TmApp _ _) t2)             = TmApp (reduceNormalOrder t1) t2
-- reduceNormalOrder (TmApp t1 t2@(TmApp _ _))             = TmApp t1 (reduceNormalOrder t2)
-- reduceNormalOrder (TmLam v t)                           = TmLam v (reduceNormalOrder t)
-- reduceNormalOrder t                                     = t

-- | 名前呼び戦略
reduceCallByName :: Term -> Term
reduceCallByName = undefined
-- reduceCallByName (TmApp (TmLam x old) new)             = subst x new old
-- reduceCallByName (TmApp t1@(TmApp _ _) t2@(TmApp _ _)) = TmApp (reduceCallByName t1) (reduceCallByName t2)
-- reduceCallByName (TmApp t1@(TmApp _ _) t2)             = TmApp (reduceCallByName t1) t2
-- reduceCallByName (TmApp t1 t2@(TmApp _ _))             = TmApp t1 (reduceCallByName t2)
-- reduceCallByName t                                     = t

-- | 値呼び戦略
reduceCallByValue :: Term -> Term
reduceCallByValue = matchField pm . unwrapTerm
  where
    pm = #var @= var
      <: #lambda @= uncurry lambda
      <: #app @= (\(t, new) -> )
      <: nil
-- reduceCallByValue (TmApp t@(TmLam x old) new)
--   | isValue new = subst x new old
--   | otherwise   = TmApp t (reduceCallByValue new)
-- reduceCallByValue (TmApp t1@(TmApp _ _) t2@(TmApp _ _)) = TmApp (reduceCallByValue t1) (reduceCallByValue t2)
-- reduceCallByValue (TmApp t1@(TmApp _ _) t2) = TmApp (reduceCallByValue t1) t2
-- reduceCallByValue (TmApp t1 t2@(TmApp _ _)) = TmApp t1 (reduceCallByValue t2)
-- reduceCallByValue t = t

-- | β-reduction
subst :: Text -> Term -> Term -> Term
subst v1 new = matchField pm . unwrapTerm
  where
    pm = #var @= (\v2 -> if v1 == v2 then new else var v2)
      <: #lambda @= (\t@(v2,t') -> if v1 == v2 then lambda v2 t' else lambda v2 (subst v1 new t'))
      <: #app @= (\(t1,t2) -> app (subst v1 new t1) (subst v1 new t2))
      <: nil

-- | 与えられた項が閉じているかどうか判定する述語
isClosed :: Term -> Bool
isClosed = Set.null . freeVars Set.empty

-- | 項に含まれる自由変数を返す
freeVars :: Set Text -> Term -> Set Text
freeVars fv = matchField pm . unwrapTerm
  where
    pm = #var    @= (\v -> if Set.member v fv then Set.empty else Set.singleton v)
      <: #lambda @= (\(v,t) -> freeVars (Set.insert v fv) t)
      <: #app    @= (\(t1,t2) -> freeVars fv t1 `Set.union` freeVars fv t2)
      <: nil

-- | 与えられた項が値かどうか判定する述語
isValue :: Term -> Bool
isValue = matchField pm . unwrapTerm
  where
    pm = #var    @= const True
      <: #lambda @= const True
      <: #app    @= const False
      <: nil