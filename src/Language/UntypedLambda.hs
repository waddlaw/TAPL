module Language.UntypedLambda
  ( module Language.UntypedLambda.Types
  , module Language.UntypedLambda.Parser
  , isClosed
  ) where

import           Language.UntypedLambda.Parser
import           Language.UntypedLambda.Types

import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)

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
