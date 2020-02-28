{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Language.Recon.Exercise.Ex22_7_1
  ( calcPrincipalType
  , runRecon
  , example1
  , example2
  , example3
  , example4
  )
where

import RIO
import qualified RIO.List as List
import qualified RIO.Set  as Set
import qualified RIO.Text as Text

data Ty
  = TyBool
  | TyNat
  | TyArr Ty Ty
  | TyVar VarName
  deriving (Eq, Show, Ord)

data Term
  = TmVar VarName
  | TmLam VarName Ty Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  | TmLet VarName Term Term  -- ^ 追加した
  | TmLamInf VarName Term    -- ^ 追加した
  deriving (Eq, Show)

type ConstraintSet = [(Ty, Ty)]
type Context = [(VarName, Ty)]
type VarName = Text
type ReturnType = Ty
type TyVarIdStream = [VarName]

runRecon :: Term -> (ReturnType, ConstraintSet)
runRecon = extract . recon [] tyVarIdStream
  where
    tyVarIdStream = map (tshow @Int) [1..]
    extract (ty, _, constr) = (ty, constr)

recon ::
  Context ->
  TyVarIdStream ->
  Term ->
  (ReturnType, TyVarIdStream, ConstraintSet)
recon ctx varIds = \case
  TmVar x ->
    let msg = "Variable '" <> Text.unpack x <> "' is not found in context: " <> show ctx
        ty = fromMaybe (error msg) $ List.lookup x ctx
     in (ty, varIds, [])
  TmLam x ty t ->
    let (rt, restVarIds, c) = recon ((x,ty):ctx) varIds t
     in (TyArr ty rt, restVarIds, c)
  TmApp t1 t2 ->
    let (rt1, restVarIds1, c1) = recon ctx varIds t1
        (rt2, x:restVarIds2, c2) = recon ctx restVarIds1 t2
        rt = TyVar x
        c = c1 <> c2 <> [(rt1, TyArr rt2 rt)]
     in(rt, restVarIds2, c)
  TmTrue  -> (TyBool, varIds, [])
  TmFalse -> (TyBool, varIds, [])
  TmIf t1 t2 t3 ->
    let (rt1, restVarIds1, c1) = recon ctx varIds t1
        (rt2, restVarIds2, c2) = recon ctx restVarIds1 t2
        (rt3, restVarIds3, c3) = recon ctx restVarIds2 t3
        c = c1 <> c2 <> c3 <> [(rt1, TyBool), (rt2, rt3)]
     in (rt2, restVarIds3, c)
  TmZero -> (TyNat, varIds, [])
  TmSucc t ->
    let (rt, restVarIds, c) = recon ctx varIds t
     in (TyNat, restVarIds, c <> [(rt, TyNat)])
  TmPred t ->
    let (rt, restVarIds, c) = recon ctx varIds t
     in (TyNat, restVarIds, c <> [(rt, TyNat)])
  TmIsZero t ->
    let (rt, restVarIds, c) = recon ctx varIds t
     in (TyBool, restVarIds, c <> [(rt, TyNat)])
  TmLet x t1 t2 ->
    let (_, restVarIds1, c1) = recon ctx varIds  t1
        (rt2, restVarIds2, c2) = recon ctx restVarIds1 (subst (x, t1) t2)
     in (rt2, restVarIds2, c1 <> c2)
  TmLamInf _ t ->
    let (x:restVarIds1) = varIds
        tyX = TyVar x
        (rt, restVarIds2, c) = recon ((x, tyX):ctx) restVarIds1 t
     in (TyArr tyX rt, restVarIds2, c)

subst :: (VarName, Term) -> Term -> Term
subst a@(x, s) = \case 
  t@(TmVar y)
    | x == y -> s
    | otherwise -> t
  t@(TmLam y ty t1)
    | x /= y && y `notElem` getFreeVars s -> TmLam y ty (subst a t1)
    | otherwise -> t
  TmApp t1 t2 -> TmApp (subst a t1) (subst a t2)
  TmTrue -> TmTrue
  TmFalse -> TmFalse
  TmIf t1 t2 t3 -> TmIf (subst a t1) (subst a t2) (subst a t3)
  TmZero -> TmZero
  TmSucc t -> TmSucc (subst a t)
  TmPred t -> TmPred (subst a t)
  TmIsZero t -> TmIsZero (subst a t)
  TmLet y t1 t2
    | x /= y && y `notElem` getFreeVars s -> TmLet y (subst a t1) (subst a t2)
    | otherwise -> TmLet y (subst a t1) t2
  t@(TmLamInf y t1)
    | x /= y && y `notElem` getFreeVars s -> TmLamInf y (subst a t1)
    | otherwise -> t

getFreeVars :: Term -> [VarName]
getFreeVars = Set.toList . go Set.empty
  where
    go fvs = \case
      TmVar x -> Set.singleton x
      TmLam x _ t -> go fvs t `Set.difference` Set.singleton x
      TmApp t1 t2 -> go fvs t1 <> go fvs t2
      TmTrue -> Set.empty
      TmFalse -> Set.empty
      TmIf t1 t2 t3 -> go fvs t1 <> go fvs t2 <> go fvs t3
      TmZero -> Set.empty
      TmSucc t -> go fvs t
      TmPred t -> go fvs t
      TmIsZero t -> go fvs t
      TmLet x t1 t2 -> go fvs t1 <> (go fvs t2 `Set.difference` Set.singleton x)
      TmLamInf x t -> go fvs t `Set.difference` Set.singleton x

unify :: ConstraintSet -> ConstraintSet
unify [] = []
unify ((s, t):c')
  | s == t  = unify c'
  | isVar s && s `notInFv` t =
      let sigma = (s, t)
      in  sigma : unify (map (applyC sigma) c')
  | isVar t && t `notInFv` s =
      let sigma = (t, s)
      in  sigma : unify (map (applyC sigma) c')
  | isArr s && isArr t =
      let TyArr s1 s2 = s
          TyArr t1 t2 = t
      in  unify ([(s1, t1), (s2, t2)] ++ c')
  | otherwise = error "fail"

-- utils
isVar :: Ty -> Bool
isVar TyVar{} = True
isVar _ = False

isArr :: Ty -> Bool
isArr TyArr{} = True
isArr _ = False

notInFv :: Ty -> Ty -> Bool
notInFv (TyVar x) t = x `notElem` fv t
notInFv _ _ = False

fv :: Ty -> [Text]
fv (TyVar x) = [x]
fv (TyArr ty1 ty2) = ((++) `on` fv) ty1 ty2
fv _ = []

applyC :: (Ty, Ty) -> (Ty, Ty) -> (Ty, Ty)
applyC sigma = fork (apply sigma)
  where
    fork f (a, b) = (f a, f b)

apply :: (Ty, Ty) -> Ty -> Ty
apply _ TyBool = TyBool
apply _ TyNat  = TyNat
apply sigma (TyArr ty1 ty2) = (TyArr `on` apply sigma) ty1 ty2
apply (s, t) u
  | s == u    = t
  | otherwise = u

runUnify :: (Ty, ConstraintSet) -> Ty
runUnify (ty, constr) = subst' ty (unify constr)
  where
    subst' = foldl (flip apply)

calcPrincipalType :: Term -> Ty
calcPrincipalType = runUnify . runRecon

-- >>> calcPrincipalType example1
-- TyNat
example1 :: Term
example1 = TmLet "double" var body
  where
    var = TmLam "f" (TyArr TyNat TyNat) $ TmLam "a" TyNat $ TmApp (TmVar "f") (TmApp (TmVar "f") (TmVar "a"))
    body = TmApp (TmApp (TmVar "double") (TmLam "x" TyNat (TmSucc $ TmSucc $ TmVar "x"))) (TmSucc $ TmSucc TmZero)

-- >>> calcPrincipalType example2
-- TyBool
example2 :: Term
example2 = TmLet "double" var body
  where
    var = TmLam "f" (TyArr TyBool TyBool) $ TmLam "a" TyBool $ TmApp (TmVar "f") (TmApp (TmVar "f") (TmVar "a"))
    body = TmApp (TmApp (TmVar "double") (TmLam "x" TyBool (TmVar "x"))) TmFalse

{-
let double = \f -> \a:Bool -> f (f a)
in let a = double (\x:Nat  -> succ (succ x)) 2
  in let b = double (\x:Bool -> x) false
    in 

>>> calcPrincipalType $ example3 $ TmVar "a"
TyNat

>>> calcPrincipalType $ example3 $ TmVar "b"
TyBool

>>> calcPrincipalType $ example3 $ TmVar "double"
TyArr 
    ( TyArr ( TyVar "?X_20" ) ( TyVar "?X_20" ) ) 
    ( TyArr ( TyVar "?X_20" ) ( TyVar "?X_20" ) )
-}
example3 :: Term -> Term
example3 body = TmLet "double" decl body1
  where
    decl = TmLamInf "f" $ TmLamInf "a" $ TmApp (TmVar "f") (TmApp (TmVar "f") (TmVar "a"))
    body1 = TmLet "a" (TmApp (TmApp (TmVar "double") (TmLam "x" TyNat (TmSucc $ TmSucc $ TmVar "x"))) (TmSucc $ TmSucc TmZero)) body2
    body2 = TmLet "b" (TmApp (TmApp (TmVar "double") (TmLam "x" TyBool (TmVar "x"))) TmFalse) body

{-
>>> runRecon example4
( TyNat
, 
    [ 
        ( TyBool
        , TyArr TyNat ( TyVar "?X_1" )
        ) 
    ]
)

>>> calcPrincipalType example4
*** Exception: fail
CallStack (from HasCallStack):
  error, called at src/Language/Ex22_7_1.hs:148:17 in main:Language.Ex22_7_1
-}
example4 :: Term
example4 = TmLet "x" (TmApp TmTrue TmZero) TmZero