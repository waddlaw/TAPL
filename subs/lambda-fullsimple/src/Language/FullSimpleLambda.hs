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
import qualified RIO.List.Partial          as L.Partial

import           Language.FullSimpleLambda.Parser
import           Language.FullSimpleLambda.Pretty
import           Language.FullSimpleLambda.TypeCheck
import           Language.FullSimpleLambda.Types

eval :: Term -> Term
eval (TmIf TmTrue t2 _t3) = t2 -- E-IFTRUE
eval (TmIf TmFalse _t2 t3) = t3 -- E-IFFALSE
eval (TmIf t1 t2 t3) = TmIf (eval t1) t2 t3 -- E-IF
eval (TmSucc t) = TmSucc (eval t) -- E-SUCC
eval (TmPred TmZero) = TmZero -- E-PREDZERO
eval (TmPred (TmSucc nv@(isNumericValue -> True))) = nv -- E-PREDSUCC
eval (TmPred t1) = TmPred (eval t1) -- E-PRED
eval (TmIsZero TmZero) = TmTrue -- E-ISZEROZERO
eval (TmIsZero (TmSucc (isNumericValue -> True))) = TmFalse -- E-ISZEROSUCC
eval (TmIsZero t) = TmIsZero (eval t) -- E-ISZERO
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
eval (TmPair t1 t2) = TmPair (eval t1) t2 -- E-PAIR1
eval (TmTupleProj j (TmTuple ts)) -- E-PROJTUPLE
  | and (map isValue ts) = if j < length ts then ts L.Partial.!! j else error "タプルのサイズより大きな値が指定されています"
  | otherwise = error "eval: 値ではない項が存在します。"
eval (TmTupleProj i t) = TmTupleProj i (eval t) -- E-PROJ
eval (TmTuple ts) = TmTuple (vs ++ [eval t] ++ ts) -- E-TUPLE
  where (vs, t, ts) = splitTerm ts
eval _ = error "unexpected: eval"

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
isValue TmVar{}        = True
isValue TmLam{}        = True
isValue TmTrue         = True
isValue TmFalse        = True
isValue TmUnit         = True -- 11.2 Unit型
isValue (TmPair t1 t2) = isValue t1 && isValue t2 -- 11.6 2つ組
isValue (TmTuple ts)   = and $ map isValue ts -- 11.7 組
isValue t              = isNumericValue t

-- | 与えられた項が数項かどうか判定
isNumericValue :: Term -> Bool
isNumericValue TmZero     = True
isNumericValue (TmSucc t) = isNumericValue t
isNumericValue _          = False

-- | TODO nameless term の実装
subst :: VarName -> Value -> Term -> Term
subst = error "subst is not implemented yet."

-- | 少なくとも1つは項である
splitTerm :: [Term] -> ([Value], Term, [Term])
splitTerm ts = (vs, L.Partial.head ts, L.Partial.tail ts)
  where (vs, ts) = span isValue ts