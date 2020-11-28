{-# LANGUAGE OverloadedStrings #-}

module Language.UntypedLambda
  ( module Language.UntypedLambda.Types,
    module Language.UntypedLambda.Parser,
    module Language.UntypedLambda.Prelude,
    isClosed,
    reduceFullBeta,
    reduceNormalOrder,
    reduceCallByName,
    reduceCallByValue,
    eval,
    evalWithTrace,
    evalOneStep,
    trace,
    steps,
    subst,
    size,

    -- * Exercise 6.1.5
    removenames,
    restorenames,

    -- * Definition 6.2.1
    shift,

    -- * Definition 6.2.4
    namelessSubst,
    reduceNameless,
  )
where

import Language.Core
import Language.UntypedLambda.Parser
import Language.UntypedLambda.Prelude
import Language.UntypedLambda.Types
import RIO hiding (trace)
import qualified RIO.List as List
import qualified RIO.List.Partial as List.Partial
import qualified RIO.Set as Set
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as Text.Partial

-- | Evaluate the term in canonical form with the specified evaluation strategy
eval :: Strategy -> UntypedLambda -> UntypedLambda
eval s t
  | result == t = t
  | otherwise = eval s result
  where
    result = evalOneStep s t

-- | debugging only
trace :: Strategy -> UntypedLambda -> [UntypedLambda]
trace s t = reverse $ evalWithTrace s [t] t

-- | Return the reduction steps
evalWithTrace :: Strategy -> [UntypedLambda] -> UntypedLambda -> [UntypedLambda]
evalWithTrace s acc t
  | result == t = acc
  | otherwise = evalWithTrace s acc' result
  where
    result = evalOneStep s t
    acc' = result : acc

-- | Return the number of steps
steps :: UntypedLambda -> Int
steps = length . evalWithTrace NormalOrder []

-- | Evaluate only one step with the specified evaluation strategy
evalOneStep :: Strategy -> UntypedLambda -> UntypedLambda
evalOneStep FullBetaReduction t = reduceFullBeta t
evalOneStep NormalOrder t = reduceNormalOrder t
evalOneStep CallByName t = reduceCallByName t
evalOneStep CallByValue t = reduceCallByValue t

-- | full (non-deterministic) beta-reduction
reduceFullBeta :: UntypedLambda -> UntypedLambda
reduceFullBeta = \case
  TmApp (TmLam x old) new -> subst x new old
  TmApp t1 t2 -> TmApp (reduceFullBeta t1) (reduceFullBeta t2)
  t -> t

-- | NormalOrder
reduceNormalOrder :: UntypedLambda -> UntypedLambda
reduceNormalOrder = \case
  -- E-ABS
  TmLam x t -> TmLam x (reduceNormalOrder t)

  -- E-APPABS
  TmApp (TmLam x old) new -> subst x new old

  TmApp t1 t2 ->
    if
      -- E-APP2
      | isNANF t1 -> TmApp t1 (reduceNormalOrder t2)
      -- E-APP1
      | isNA t1 -> TmApp (reduceNormalOrder t1) t2
      | otherwise -> error "never happen"

  t -> t

-- normal forms
isNF :: UntypedLambda -> Bool
isNF = \case
  TmLam _ t -> isNF t
  t -> isNANF t

-- non-abstraction normal forms
isNANF :: UntypedLambda -> Bool
isNANF = \case
  TmVar _ -> True
  TmApp t1 t2 -> isNANF t1 && isNF t2
  _ -> False

-- non-abstractions
isNA :: UntypedLambda -> Bool
isNA = \case
  TmVar _ -> True
  TmApp _ _ -> True
  _ -> False


-- | CallByName
reduceCallByName :: UntypedLambda -> UntypedLambda
reduceCallByName = \case
  -- E-APPABS
  TmApp (TmLam x old) new -> subst x new old
  -- E-APP1
  TmApp t1 t2 -> TmApp (reduceCallByName t1) t2
  t -> t

-- | CallByValue
reduceCallByValue :: UntypedLambda -> UntypedLambda
reduceCallByValue = \case
  TmApp t@(TmLam x old) new -> 
    if
      -- E-APPABS
      | isValue new -> subst x new old
      
      -- E-APP2
      | otherwise -> TmApp t (reduceCallByValue new)

  -- E-APP1
  TmApp t1 t2 -> TmApp (reduceCallByValue t1) t2
  t -> t

-- | β-reduction
--
-- Definition 5.3.5 (P.54)
subst :: Text -> UntypedLambda -> UntypedLambda -> UntypedLambda
subst v1 after t@(TmVar v2)
  | v1 == v2 = after
  | otherwise = t
subst v1 after t@(TmLam v2 t')
  | v1 /= v2 && v2 `notIn` after = TmLam v2 (subst v1 after t')
  | otherwise = t -- TODO
  where
    notIn v term = v `Set.notMember` freeVars Set.empty term
subst v after (TmApp t1 t2) = (TmApp `on` subst v after) t1 t2

-- | a predicate that determines whether a given term is closed
isClosed :: UntypedLambda -> Bool
isClosed = Set.null . freeVars Set.empty

-- | return free variable contained in term. Definition 5.3.2 (p.52)
freeVars :: Set VarName -> UntypedLambda -> Set VarName
freeVars fv (TmVar v)
  | Set.member v fv = Set.empty
  | otherwise = Set.singleton v
freeVars fv (TmLam v t) = freeVars fv t `Set.difference` Set.singleton v
freeVars fv (TmApp t1 t2) = (Set.union `on` freeVars fv) t1 t2

-- | predicate that determines whether a given term is a value
isValue :: UntypedLambda -> Bool
isValue (TmVar _) = True
isValue (TmLam _ _) = True
isValue _ = False

-- | Calculate the size of a term. Exercise 5.3.3 (P.52)
size :: UntypedLambda -> Int
size (TmVar _) = 1
size (TmLam _ t) = 1 + size t
size (TmApp t1 t2) = size t1 + size t2

-- | Exercise 6.1.5 (P.59)
--
-- supposition: FV(t) ⊆ dom(Γ）)
removenames :: Context -> UntypedLambda -> NamelessTerm
removenames g t
  | freeVars Set.empty t `Set.isSubsetOf` Set.fromList g = removenames' g t
  | otherwise = error "Does not satisfy: FV(t) `isSubsetOf` dom(Γ)"
  where
    removenames' g' (TmVar x) = NlTmVar $ fromMaybe (error "Can't find variable") $ List.elemIndex x g'
    removenames' g' (TmLam x t1) = NlTmLam $ removenames (x : g') t1
    removenames' g' (TmApp t1 t2) = (NlTmApp `on` removenames' g') t1 t2

-- | Exercise 6.1.5 (P.59)
--
-- supposition1: The names in Γ are different from each other.
--
-- supposition2: The set V of variable names is ordered
restorenames :: Context -> NamelessTerm -> UntypedLambda
restorenames g nt
  | isValid g = restorenames' g nt
  | otherwise = error "Error: duplicate variables in context."
  where
    isValid g' = ((==) `on` (length . List.nub)) g' g'
    restorenames' g' (NlTmVar k) = TmVar (g' List.Partial.!! k)
    restorenames' g' (NlTmLam t) =
      let x = mkFreshVarName g'
       in TmLam x $ restorenames (x : g') t
    restorenames' g' (NlTmApp t1 t2) = (TmApp `on` restorenames' g') t1 t2

mkFreshVarName :: Context -> VarName
mkFreshVarName [] = "a0"
mkFreshVarName (v : _) = Text.pack $ mconcat ["a", show $ textToInt v + 1]
  where
    textToInt :: Text -> Int
    textToInt = fromMaybe 0 . readMaybe . Text.unpack . Text.Partial.tail -- FIXME

-- | Definition 6.2.1 (P.60)
--
-- c: cutoff
--
-- d: shift
shift :: Int -> Int -> NamelessTerm -> NamelessTerm
shift c d (NlTmVar k)
  | k < c = NlTmVar k
  | otherwise = NlTmVar (k + d)
shift c d (NlTmLam t) = NlTmLam $ shift (c + 1) d t
shift c d (NlTmApp t1 t2) = (NlTmApp `on` shift c d) t1 t2

-- | Definition 6.2.4 (P.60)
namelessSubst :: Int -> NamelessTerm -> NamelessTerm -> NamelessTerm
namelessSubst j s t@(NlTmVar k)
  | k == j = s
  | otherwise = t
namelessSubst j s (NlTmLam t) = NlTmLam $ namelessSubst (j + 1) (shift 0 1 s) t
namelessSubst j s (NlTmApp t1 t2) = (NlTmApp `on` namelessSubst j s) t1 t2

-- | β reduction for nameless terms (call by value)
reduceNameless :: NamelessTerm -> NamelessTerm
reduceNameless (NlTmApp (NlTmLam t12) v2) = shift 0 (-1) $ namelessSubst 0 (shift 0 1 v2) t12
reduceNameless (NlTmApp t1@(NlTmApp _ _) t2@(NlTmApp _ _)) = (NlTmApp `on` reduceNameless) t1 t2
reduceNameless (NlTmApp t1@(NlTmApp _ _) t2) = NlTmApp (reduceNameless t1) t2
reduceNameless (NlTmApp t1 t2@(NlTmApp _ _)) = NlTmApp t1 (reduceNameless t2)
reduceNameless t = t
