{-# LANGUAGE OverloadedStrings #-}

module Language.FullSimpleLambda
  ( module Language.FullSimpleLambda.Types,
    module Language.FullSimpleLambda.Parser,
    module Language.FullSimpleLambda.Pretty,
    typeof,
    desugar,
    eval,
  )
where

import Data.Monoid
import Language.FullSimpleLambda.Internal
import Language.FullSimpleLambda.Parser
import Language.FullSimpleLambda.Pretty
import Language.FullSimpleLambda.TypeCheck
import Language.FullSimpleLambda.Types
import RIO
import qualified RIO.List.Partial as List.Partial

-- | Definition 6.2.1
--
-- c: cuttoff
--
-- d: shift number
shift :: Int -> Int -> Term -> Term
shift c d (TmVar k)
  | k < c = TmVar k
  | otherwise = TmVar (k + d)
shift c d (TmLam x ty t) = TmLam x ty $ shift (c + 1) d t
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
shift c d (TmWildcard ty t) = TmWildcard ty $ shift (c + 1) d t
shift c d (TmAscribe t ty) = TmAscribe (shift (c + 1) d t) ty
shift c d (TmLet x t1 t2) = TmLet x (shift c d t1) (shift (c + 1) d t2) -- TODO (check)
shift c d (TmPair t1 t2) = (TmPair `on` shift c d) t1 t2
shift c d (TmPairFst t) = TmPairFst $ shift c d t
shift c d (TmPairSnd t) = TmPairSnd $ shift c d t
shift c d (TmTuple ts) = TmTuple $ map (shift c d) ts
shift c d (TmTupleProj i t) = TmTupleProj i $ shift c d t
shift c d (TmRecord rs) = TmRecord $ map (second (shift c d)) rs
shift c d (TmRecordProj l t) = TmRecordProj l $ shift c d t
shift c d (TmPattern p t1 t2) = TmPattern p (shift c d t1) (shift (c + 1) d t2) -- TODO (check, 間違ってるかも)
shift c d (TmInL t ty) = TmInL (shift c d t) ty
shift c d (TmInR t ty) = TmInR (shift c d t) ty
shift c d (TmCase t (x1, t1) (x2, t2)) = TmCase (shift c d t) altL altR
  where
    altL = (x1, shift (c + 1) d t1)
    altR = (x2, shift (c + 1) d t2)

-- | Definition 6.2.4
--
-- j: variable number
--
-- s: assigned value
subst :: Int -> Value -> Term -> Term
subst j s t@(TmVar k)
  | k == j = s
  | otherwise = t
subst j s (TmLam x ty t) = TmLam x ty $ subst (j + 1) (shift 0 1 s) t
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
subst j s (TmLet x t1 t2) = TmLet x (subst j s t1) (subst (j + 1) (shift 0 1 s) t2) -- TODO check
subst j s (TmPair t1 t2) = (TmPair `on` subst j s) t1 t2
subst j s (TmPairFst t) = TmPairFst $ subst j s t
subst j s (TmPairSnd t) = TmPairSnd $ subst j s t
subst j s (TmTuple ts) = TmTuple $ map (subst j s) ts
subst j s (TmTupleProj i t) = TmTupleProj i $ subst j s t
subst j s (TmRecord rs) = TmRecord $ map (second (subst j s)) rs
subst j s (TmRecordProj l t) = TmRecordProj l $ subst j s t
subst j s (TmPattern p t1 t2) = TmPattern p (subst j s t1) (subst (j + 1) (shift 0 1 s) t2) -- TODO check (間違っていそう)
subst j s (TmInL t ty) = TmInL (subst j s t) ty
subst j s (TmInR t ty) = TmInR (subst j s t) ty
subst j s (TmCase t (x1, t1) (x2, t2)) = TmCase (subst j s t) altL altR
  where
    altL = (x1, subst (j + 1) s t1)
    altR = (x2, subst (j + 1) s t2)

eval :: Term -> Term
eval = \case
  TmIf TmTrue t2 _t3 -> t2 -- E-IFTRUE
  TmIf TmFalse _t2 t3 -> t3 -- E-IFFALSE
  TmIf t1 t2 t3 -> TmIf (eval t1) t2 t3 -- E-IF
  TmSucc t -> TmSucc (eval t) -- E-SUCC
  TmPred TmZero -> TmZero -- E-PREDZERO
  TmPred (TmSucc nv@(isNumericValue -> True)) -> nv -- E-PREDSUCC
  TmPred t -> TmPred (eval t) -- E-PRED
  TmIsZero TmZero -> TmTrue -- E-ISZEROZERO
  TmIsZero (TmSucc (isNumericValue -> True)) -> TmFalse -- E-ISZEROSUCC
  TmIsZero t -> TmIsZero (eval t) -- E-ISZERO
  TmApp (TmLam _x _ t1) v2@(isValue -> True) -> shift 0 (-1) $ subst 0 (shift 0 1 v2) t1 -- E-APPABS
  TmApp (TmWildcard _ t12) _t2@(isValue -> True) -> t12 -- E-WILDCARD
  TmApp v1@(isValue -> True) t2 -> TmApp v1 (eval t2) -- E-APP2
  TmApp t1 t2 -> TmApp (eval t1) t2 -- E-APP1
  TmSeq _t1@(isValue -> True) t2 -> t2 -- E-SEQNEXT
  TmSeq t1 t2 -> TmSeq (eval t1) t2 -- E-SEQ
  TmAscribe v@(isValue -> True) _ -> v -- E-ASCRIBE
  TmAscribe t1 tyT -> TmAscribe (eval t1) tyT -- E-ASCRIBE1
  TmLet _x v1@(isValue -> True) t2 -> shift 0 (-1) $ subst 0 (shift 0 1 v1) t2 -- E-LETV
  TmLet x t1 t2 -> TmLet x (eval t1) t2 -- E-LET
  TmPairFst (TmPair v1@(isValue -> True) _v2@(isValue -> True)) -> v1 -- E-PAIRBETA1
  TmPairFst t -> TmPairFst (eval t) -- E-PROJ1
  TmPairSnd (TmPair _v1@(isValue -> True) v2@(isValue -> True)) -> v2 -- E-PAIRBETA2
  TmPairSnd t -> TmPairSnd (eval t) -- E-PROJ2
  TmPair v1@(isValue -> True) t2 -> TmPair v1 (eval t2) -- E-PAIR2
  TmPair t1 t2 -> TmPair (eval t1) t2 -- E-PAIR1

  -- E-PROJTUPLE
  TmTupleProj j (TmTuple ts) ->
    if
        | all isValue ts ->
          if j < length ts
            then ts List.Partial.!! j
            else error "A value greater than the size of the tuple was specified"
        | otherwise -> error "A term exists that is not a value."
  -- E-PROJ
  TmTupleProj i t -> TmTupleProj i (eval t)
  -- E-TUPLE
  TmTuple ts ->
    let (vs, t, ts') = splitTerm ts
     in TmTuple (vs ++ [eval t] ++ ts')
  -- E-PROJRCD
  TmRecordProj label t@(TmRecord fields) ->
    if
        | isValue t -> fromMaybe (error "field label not found (E-PROJRCD)") $ lookup label fields
        | otherwise -> TmRecordProj label (eval t)
  -- E-PROJ
  TmRecordProj label t -> TmRecordProj label (eval t)
  -- E-RCD
  t@(TmRecord ts) -> case ts of
    [] -> t
    _ ->
      let (vfs, (label, tj), tfs) = splitRecord t
          t' = (label, eval tj)
       in if
              | isValue t -> t
              | otherwise -> TmRecord (vfs ++ [t'] ++ tfs)
  TmPattern p v@(isValue -> True) t2 -> match p v t2 -- E-LETV
  TmPattern p t1 t2 -> TmPattern p (eval t1) t2 -- E-LET
  TmInL t ty -> TmInL (eval t) ty -- E-INL
  TmInR t ty -> TmInR (eval t) ty -- E-INR
  TmCase (TmInL v@(isValue -> True) _ty) (TmVar x1, t1) _altInR -> subst x1 v t1 -- E-CASEINL
  TmCase (TmInR v@(isValue -> True) _ty) _altInL (TmVar x2, t2) -> subst x2 v t2 -- E-CASEINR
  TmCase t altL altR -> TmCase (eval t) altL altR -- E-CASE
  t -> t

-- _ -> error "unexpected term"

match :: Pattern -> Value -> (Term -> Term)
match (PtVar _ n) v = subst n v
match p@(PtRecord fs) v@(TmRecord fs')
  | isRecordValue v && sameFieldLength p v =
    appEndo $ foldMap (Endo . uncurry match) $ zip (map snd fs) (map snd fs')
  | otherwise = error "match: pattern match failure"
match PtRecord {} _ = error "match: v is not Rrcord"

sameFieldLength :: Pattern -> Value -> Bool
sameFieldLength (PtRecord fs1) (TmRecord fs2) = length fs1 == length fs2
sameFieldLength _ _ = error "unexpected field"

-- | Target Syntax
--
-- TmSeq
--
-- TmWildcard
desugar :: Term -> Term
desugar = \case
  TmSeq t1 t2 -> TmApp (TmLam "x" TyUnit t2) t1 -- FIXME x notin FV(t2)
  TmWildcard ty t -> TmLam "x" ty t -- FIXME x notin FV(t)
  TmAscribe t ty -> TmApp (TmLam "x" ty (TmVar 0)) t -- FIXME x notin FV(t)
  term -> term

----------------------
-- helper functions --
----------------------

-- | at least one term
splitTerm :: [Term] -> ([Value], Term, [Term])
splitTerm = \case
  [] -> error "empty list is not expected"
  ts ->
    let (vs, ts') = span isValue ts
     in (vs, List.Partial.head ts', List.Partial.tail ts')

-- | Assume records only
splitRecord :: Term -> ([(FieldLabel, Value)], (FieldLabel, Term), [(FieldLabel, Term)])
splitRecord = \case
  TmRecord fs ->
    let (vfs, tfs) = span (isValue . snd) fs
     in (vfs, List.Partial.head tfs, List.Partial.tail tfs)
  _ -> error "only record"
