{-# LANGUAGE LambdaCase #-}
module Ex22_5_7 where

import Data.Maybe
import Data.List

data Ty
  = TyBool
  | TyNat
  | TyArr Ty Ty
  | TyId String
  deriving (Eq, Show, Ord)

data Term
  = TmVar String
  | TmLam String Ty Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  deriving (Eq, Show)

-- | 制約集合
type Constr = [(Ty, Ty)]

type Context = [(String, Ty)]
type TyVars = [String]

recon :: Context -> Constr -> TyVars -> Term -> (Ty, TyVars, Constr)
recon ctx constr f = \case
  TmVar x ->
    let ty = snd . fromMaybe (error "Variable is not found in context.") $ find ((==x) . fst) ctx
    in (ty, f, [])
  TmLam x ty t ->
    let (rt, f', c) = recon ((x,ty):ctx) constr f t
    in  (subst (TyArr ty rt) c, f', c)
  TmApp t1 t2 ->
    let (rt1, f1, c1) = recon ctx constr f  t1
        (rt2, f2, c2) = recon ctx constr f1 t2
        x  = head f2
        f3 = tail f2
        rt = TyId ("?X_" <> x)
        c = unify (c1 <> c2 <> [(rt1, TyArr rt2 rt)])
    in (subst rt c, f3, c)
  TmTrue -> (TyBool, f, [])
  TmFalse -> (TyBool, f, [])
  TmIf t1 t2 t3 ->
    let (rt1, f1, c1) = recon ctx constr f  t1
        (rt2, f2, c2) = recon ctx constr f1 t2
        (rt3, f3, c3) = recon ctx constr f2 t3
        c = unify (c1 <> c2 <> c3 <> [(rt1, TyBool), (rt2, rt3)])
    in (subst rt2 c, f3, c)
  TmZero -> (TyNat, f, [])
  TmSucc t ->
    let (rt, f', c) = recon ctx constr f t
        c' = unify (c <> [(rt, TyNat)])
    in (TyNat, f', c')
  TmPred t ->
    let (rt, f', c) = recon ctx constr f t
        c' = unify (c <> [(rt, TyNat)])
    in (TyNat, f', c')
  TmIsZero t ->
    let (rt, f', c) = recon ctx constr f t
        c' = unify (c <> [(rt, TyNat)])
    in (TyBool, f', c')

runRecon :: Term -> Ty
runRecon = extract . recon [] [] freshVars
  where
    freshVars = map show [1..]
    extract (ty, _, _) = ty

unify :: Constr -> Constr
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
isVar (TyId _) = True
isVar _ = False

isArr :: Ty -> Bool
isArr (TyArr _ _) = True
isArr _ = False

notInFv :: Ty -> Ty -> Bool
notInFv (TyId x) t = x `notElem` (fv t)
notInFv _ _ = False

fv :: Ty -> [String]
fv (TyId x) = [x]
fv (TyArr ty1 ty2) = fv ty1 ++ fv ty2
fv _ = []

getVar :: Ty -> String
getVar (TyId x) = x

applyC :: (Ty, Ty) -> (Ty, Ty) -> (Ty, Ty)
applyC sigma (s, t) = (apply sigma s, apply sigma t)

apply :: (Ty, Ty) -> Ty -> Ty
apply _ TyBool = TyBool
apply _ TyNat  = TyNat
apply sigma (TyArr ty1 ty2) = TyArr (apply sigma ty1) (apply sigma ty2)
apply (s, t) u
  | s == u    = t
  | otherwise = u

subst :: Ty -> Constr -> Ty
subst = foldl (flip apply)

{-
λ> calcPrincipalType ex1
TyArr (TyId "X") (TyId "X")
-}
ex1 :: Term
ex1 = TmLam "x" (TyId "X") (TmVar "x")

{-
λ> calcPrincipalType ex2
TyArr (TyArr (TyId "?X_1") (TyId "?X_2")) (TyArr (TyArr TyBool (TyId "?X_1")) (TyId "?X_2"))
-}
ex2 :: Term
ex2 = TmLam "z" (TyId "ZZ") . TmLam "y" (TyId "YY") $ TmApp (TmVar "z") (TmApp (TmVar "y") TmTrue)

{-
λ> calcPrincipalType ex3
TyArr (TyArr TyBool TyBool) TyBool
-}
ex3 :: Term
ex3 = TmLam "w" (TyId "W") $ TmIf TmTrue TmFalse (TmApp (TmVar "w") TmFalse)