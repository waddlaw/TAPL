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
import qualified RIO.List.Partial as L.Partial

import           Language.FullSimpleLambda.Internal
import           Language.FullSimpleLambda.Parser
import           Language.FullSimpleLambda.Pretty
import           Language.FullSimpleLambda.TypeCheck
import           Language.FullSimpleLambda.Types

-- | 定義6.2.1 (P.60)
--
-- c: 打ち切り値
--
-- d: シフト数
shift :: Int -> Int -> Term -> Term
shift c d (TmVar k)
  | k < c = TmVar k
  | otherwise = TmVar (k + d)
shift c d (TmLam x ty t) = TmLam x ty $ shift (c+1) d t
shift c d (TmApp t1 t2) = (TmApp `on` shift c d) t1 t2
shift _ _ TmTrue = TmTrue
shift _ _ TmFalse = TmFalse
shift c d (TmIf t1 t2 t3) = (TmIf `on` shift c d) t1 t2 t3
shift _ _ TmZero = TmZero
shift c d (TmSucc t) = TmSucc $ shift c d t
shift c d (TmPred t) = TmPred $ shift c d t
shift c d (TmIsZero t) = TmIsZero $ shift c d t
shift _ _ TmUnit = TmUnit
shift c d (TmSeq t1 t2) = (TmSeq `on` shift c d) t1 t2
shift c d (TmWildcard ty t) = TmWildcard ty $ shift (c+1) d t
shift c d (TmAscribe t ty) = TmAscribe (shift (c+1) d t) ty
shift c d (TmLet x t1 t2) = TmLet x (shift c d t1) (shift (c+1) d t2) -- TODO (check)
shift c d (TmPair t1 t2) = (TmPair `on` shift c d) t1 t2
shift c d (TmPairFst t) = TmPairFst $ shift c d t
shift c d (TmPairSnd t) = TmPairSnd $ shift c d t
shift c d (TmTuple ts) = TmTuple $ map (shift c d) ts
shift c d (TmTupleProj i t) = TmTupleProj i $ shift c d t
shift c d (TmRecord rs) = TmRecord $ map (\(l,t) -> (l, shift c d t)) rs
shift c d (TmRecordProj l t) = TmRecordProj l $ shift c d t

-- | 定義 6.2.4 (P.60)
--
-- j: 変数番号
--
-- s: 代入する値
subst :: Int -> Value -> Term -> Term
subst j s t@(TmVar k)
  | k == j = s
  | otherwise = t
subst j s (TmLam x ty t) = TmLam x ty $ subst (j+1) (shift 0 1 s) t
subst j s (TmApp t1 t2) = (TmApp `on` subst j s) t1 t2
subst _ _ TmTrue = TmTrue
subst _ _ TmFalse = TmFalse
subst j s (TmIf t1 t2 t3) = (TmIf `on` subst j s) t1 t2 t3
subst _ _ TmZero = TmZero
subst j s (TmSucc t) = TmSucc $ subst j s t
subst j s (TmPred t) = TmPred $ subst j s t
subst j s (TmIsZero t) = TmIsZero $ subst j s t
subst _ _ TmUnit = TmUnit
subst j s (TmSeq t1 t2) = (TmSeq `on` subst j s) t1 t2
subst j s (TmWildcard ty t) = TmWildcard ty $ subst j s t
subst j s (TmAscribe t ty) = TmAscribe (subst j s t) ty
subst j s (TmLet x t1 t2) = TmLet x (subst j s t1) (subst (j+1) (shift 0 1 s) t2) -- TODO check
subst j s (TmPair t1 t2) = (TmPair `on` subst j s) t1 t2
subst j s (TmPairFst t) = TmPairFst $ subst j s t
subst j s (TmPairSnd t) = TmPairSnd $ subst j s t
subst j s (TmTuple ts) = TmTuple $ map (subst j s) ts
subst j s (TmTupleProj i t) = TmTupleProj i $ subst j s t
subst j s (TmRecord rs) = TmRecord $ map (\(l,t) -> (l, subst j s t)) rs
subst j s (TmRecordProj l t) = TmRecordProj l $ subst j s t

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
eval (TmApp (TmLam _x _ t1) v2@(isValue -> True)) = shift 0 (-1) $ subst 0 (shift 0 1 v2) t1-- E-APPABS
eval (TmApp (TmWildcard _ t12) _t2@(isValue -> True)) = t12 -- E-WILDCARD
eval (TmApp v1@(isValue -> True) t2) = TmApp v1 (eval t2) -- E-APP2
eval (TmApp t1 t2) = TmApp (eval t1) t2  -- E-APP1
eval (TmSeq _t1@(isValue -> True) t2) = t2 -- E-SEQNEXT
eval (TmSeq t1 t2) = TmSeq (eval t1) t2 -- E-SEQ
eval (TmAscribe v@(isValue -> True) _) = v -- E-ASCRIBE
eval (TmAscribe t1 tyT) = TmAscribe (eval t1) tyT -- E-ASCRIBE1
eval (TmLet _x v1@(isValue -> True) t2) = shift 0 (-1) $ subst 0 (shift 0 1 v1) t2 -- E-LETV
eval (TmLet x t1 t2) = TmLet x (eval t1) t2 -- E-LET
eval (TmPairFst (TmPair v1@(isValue -> True) _v2@(isValue -> True))) = v1 -- E-PAIRBETA1
eval (TmPairSnd (TmPair _v1@(isValue -> True) v2@(isValue -> True))) = v2 -- E-PAIRBETA2
eval (TmPairFst t) = TmPairFst (eval t) -- E-PROJ1
eval (TmPairSnd t) = TmPairSnd (eval t) -- E-PROJ2
eval (TmPair v1@(isValue -> True) t2) = TmPair v1 (eval t2) -- E-PAIR2
eval (TmPair t1 t2) = TmPair (eval t1) t2 -- E-PAIR1
eval (TmTupleProj j (TmTuple ts)) -- E-PROJTUPLE
  | all isValue ts =
      if j < length ts
      then ts L.Partial.!! j
      else error "タプルのサイズより大きな値が指定されています"
  | otherwise = error "eval: 値ではない項が存在します。"
eval (TmTupleProj i t) = TmTupleProj i (eval t) -- E-PROJ
eval (TmTuple ts) = TmTuple (vs ++ [eval t] ++ ts') -- E-TUPLE
  where (vs, t, ts') = splitTerm ts
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

-- | 少なくとも1つは項である
splitTerm :: [Term] -> ([Value], Term, [Term])
splitTerm [] = error "empty list is not expected"
splitTerm ts = (vs, L.Partial.head ts', L.Partial.tail ts')
  where (vs, ts') = span isValue ts
