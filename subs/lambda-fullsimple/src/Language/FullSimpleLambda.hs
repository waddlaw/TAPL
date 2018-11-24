{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Language.FullSimpleLambda
  ( module Language.FullSimpleLambda.Types
  , module Language.FullSimpleLambda.Parser
  , module Language.FullSimpleLambda.Pretty
  , typeof
  , desugar
  , eval
  ) where

import           RIO

import           Language.FullSimpleLambda.Parser
import           Language.FullSimpleLambda.Pretty
import           Language.FullSimpleLambda.TypeCheck
import           Language.FullSimpleLambda.Types

eval :: Term -> Term
eval (TmIf TmTrue t2 _t3) = t2 -- E-IFTRUE
eval (TmIf TmFalse _t2 t3) = t3 -- E-IFFALSE
eval (TmIf t1 t2 t3) = TmIf (eval t1) t2 t3 -- E-IF
eval (TmApp (TmLam x _ t1) v2@(isValue -> True)) = subst x v2 t1 -- E-APPABS
eval (TmApp (TmWildcard _ t12) _t2@(isValue -> True)) = t12 -- E-WILDCARD
eval (TmApp v1@(isValue -> True) t2) = TmApp v1 (eval t2) -- E-APP2
eval (TmApp t1 t2) = TmApp (eval t1) t2  -- E-APP1
eval (TmSeq _t1@(isValue -> True) t2) = t2 -- E-SEQNEXT
eval (TmSeq t1 t2) = TmSeq (eval t1) t2 -- E-SEQ
eval (TmAscribe v@(isValue -> True) _) = v -- E-ASCRIBE
eval (TmAscribe t1 tyT) = TmAscribe (eval t1) tyT -- E-ASCRIBE1
eval (TmLet x v1@(isValue -> True) t2) = subst x v1 t2 -- E-LETV
eval (TmLet x t1 t2) = TmLet x (eval t1) t2 -- E-LET
eval (TmPairFst (TmPair v1@(isValue -> True) _v2@(isValue -> True))) = v1 -- E-PAIRBETA1
eval (TmPairSnd (TmPair _v1@(isValue -> True) v2@(isValue -> True))) = v2 -- E-PAIRBETA2
eval (TmPairFst t) = TmPairFst (eval t) -- E-PROJ1
eval (TmPairSnd t) = TmPairSnd (eval t) -- E-PROJ2
eval (TmPair v1@(isValue -> True) t2) = TmPair v1 (eval t2) -- E-PAIR2
eval (TmPair t1 t2) = TmPair (eval t1) t2
eval _ = error "unexpected: eval"

subst :: Text -> Value -> Term -> Term
subst = error "not implemented"

-- | 対象の構文
--
-- TmSeq
--
-- TmWildcard
desugar :: Term -> Term
desugar (TmSeq t1 t2)     = TmApp (TmLam "x" TyUnit t2) t1 -- FIXME x notin FV(t2)
desugar (TmWildcard ty t) = TmLam "x" ty t -- FIXME x notin FV(t)
desugar (TmAscribe t ty)  = TmApp (TmLam "x" ty (TmVar 0)) t -- FIXME x notin FV(t)
desugar term              = term

----------------------
-- helper functions --
----------------------

-- | 与えられた項が値かどうか判定する述語
isValue :: Term -> Bool
isValue TmVar{}  = True
isValue TmLam{}  = True
isValue TmUnit   = True -- 11.2 Unit型
isValue TmPair{} = True -- 11.6 組
isValue _        = False
