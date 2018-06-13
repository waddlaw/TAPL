module Language.UntypedLambda
  ( module Language.UntypedLambda.Types
  , module Language.UntypedLambda.Parser
  , isClosed
  , reduceNormalOrder
  , evalOneStep
  ) where

import           Language.UntypedLambda.Parser
import           Language.UntypedLambda.Types

import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)

evalOneStep :: Strategy -> Term -> Term
evalOneStep FullBetaReduction t = undefined
evalOneStep NormalOrder       t = reduceNormalOrder t
evalOneStep CallByName        t = undefined
evalOneStep CallByValue       t = undefined

reduceNormalOrder :: Term -> Term
reduceNormalOrder (TmApp (TmLam x old) new) = subst x new old
reduceNormalOrder (TmLam v t) = TmLam v (reduceNormalOrder t)
reduceNormalOrder t = t

subst :: Text -> Term -> Term -> Term
subst v1 new t@(TmVar v2)
  | v1 == v2  = new
  | otherwise = t
subst v1 new t@(TmLam v2 t')
  | v1 == v2  = t
  | otherwise = TmLam v2 (subst v1 new t')
subst v new (TmApp t1 t2) = TmApp t1' t2'
  where
    t1' = subst v new t1
    t2' = subst v new t2

isClosed :: Term -> Bool
isClosed = Set.null . freeVars Set.empty

freeVars :: Set Text -> Term -> Set Text
freeVars fv (TmVar v)
  | Set.member v fv = Set.empty
  | otherwise = Set.singleton v
freeVars fv (TmLam v t) = freeVars fv' t
  where
    fv' = Set.insert v fv
freeVars fv (TmApp t1 t2) = fv1 `Set.union` fv2
  where
    fv1 = freeVars fv t1
    fv2 = freeVars fv t2
