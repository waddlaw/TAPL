module KnuthBendix.Utils
  ( renameVars
  , renameVarsInReductionRule
  , renameVarsInReductionRuleWithPrefix
  , normaliseTerm
  -- test
  , renameVars
  ) where

import Data.Maybe
import KnuthBendix.Types

renameVars :: Term -> Term
renameVars = renameVarsWithPrefix ""

renameVarsWithPrefix :: String -> Term -> Term
renameVarsWithPrefix prefix term = renameVar prefix term 0 . findVarsInTerm $ term

renameVar :: String -> Term -> Int -> Vars  -> Term
renameVar _ term _ [] = term
renameVar prefix term n (v:vars) = renameVar prefix newTerm (n+1) vars
  where
    newTerm = changeVarInTerm v (mkVar Nothing n) term

renameVarsInReductionRule :: ReductionRule -> ReductionRule
renameVarsInReductionRule rr = renameReductionRuleWithPrefix Nothing rr 0 . findVarsInTerm . rule $ rr

renameVarsInReductionRuleWithPrefix :: String -> ReductionRule -> ReductionRule
renameVarsInReductionRuleWithPrefix prefix rr = renameReductionRuleWithPrefix (Just prefix) rr 0 . findVarsInTerm . rule $ rr

renameReductionRuleWithPrefix :: Maybe String -> ReductionRule -> Int -> [Term] -> ReductionRule
renameReductionRuleWithPrefix _ rr _ [] = rr
renameReductionRuleWithPrefix mprefix rr n (v:vars) = renameReductionRuleWithPrefix mprefix newRr (n+1) vars
  where
    newRr     = ReductionRule newRule newResult
    newRule   = changeVarInTerm v newVar $ rule rr
    newResult = changeVarInTerm v newVar $ result rr
    newVar    = mkVar mprefix n

changeVarInTerm :: Var -> Var -> Term -> Term
changeVarInTerm old new var@(Var _)
  | old == var = new
  | otherwise  = var
changeVarInTerm old new (Func t args) = Func t $ map (changeVarInTerm old new) args

normaliseTerm :: Term -> Term
normaliseTerm term = normalise term 0 . findVarsInTerm $ term

normalise :: Term -> Int -> [Term] -> Term
normalise term _ [] = term
normalise term n (v:vars) = normalise newTerm (n+1) vars
  where
    newTerm = changeVarInTerm v (mkVar Nothing n) term

mkVar :: Maybe String -> Int -> Var
mkVar mprefix n = Var $ mconcat [prefix, "v", show n]
  where
    prefix = fromMaybe "" mprefix