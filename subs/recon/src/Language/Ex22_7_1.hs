{-# LANGUAGE LambdaCase #-}
module Language.Ex22_7_1 where

import           Data.List
import           Data.Maybe
import qualified Data.Set    as Set

import           Debug.Trace

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
  | TmLet String Term Term  -- ^ 追加した
  | TmLamInf String Term -- ^ 追加した
  deriving (Eq, Show)

-- | 制約集合
type Constr = [(Ty, Ty)]

type Context = [(String, Ty)]
type TyVars = [String]

recon :: Context -> Constr -> TyVars -> Term -> (Ty, TyVars, Constr)
recon ctx constr f = \case
  TmVar x ->
    let ty = snd . fromMaybe (error $ "Variable is not found in context: " <> x <> show ctx) $ find ((==x) . fst) ctx
    in (ty, f, [])
  TmLam x ty t ->
    let (rt, f', c) = recon ((x,ty):ctx) constr f t
    in  (TyArr ty rt, f', c)
  TmApp t1 t2 ->
    let (rt1, f',  c1) = recon ctx constr f  t1
        (rt2, f'', c2) = recon ctx constr f' t2
        x = head f''
        f''' = tail f''
        rt = TyId ("?X_" <> x)
        c = c1 <> c2 <> [(rt1, TyArr rt2 rt)]
    in (rt, f''', c)
  TmTrue -> (TyBool, f, [])
  TmFalse -> (TyBool, f, [])
  TmIf t1 t2 t3 ->
    let (rt1, f',   c1) = recon ctx constr f   t1
        (rt2, f'',  c2) = recon ctx constr f'  t2
        (rt3, f''', c3) = recon ctx constr f'' t3
        c = c1 <> c2 <> c3 <> [(rt1, TyBool), (rt2, rt3)]
    in (rt2, f''', c)
  TmZero -> (TyNat, f, [])
  TmSucc t ->
    let (rt, f', c) = recon ctx constr f t
    in (TyNat, f', c <> [(rt, TyNat)])
  TmPred t ->
    let (rt, f', c) = recon ctx constr f t
    in (TyNat, f', c <> [(rt, TyNat)])
  TmIsZero t ->
    let (rt, f', c) = recon ctx constr f t
    in (TyBool, f', c <> [(rt, TyNat)])
  TmLet x t1 t2 ->
    let (rt1, f1, c1) = recon ctx constr f  t1
        (rt2, f2, c2) = recon ctx constr f1 (subst (x, t1) t2)
    in (rt2, f2, c1 <> c2)
  TmLamInf x t ->
    let tyX = TyId $ head f
        f'  = tail f
        (rt, f'', c) = recon ((x, tyX):ctx) constr f' t
    in (TyArr tyX rt, f'', c)

subst :: (String, Term) -> Term -> Term
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

getFreeVars :: Term -> [String]
getFreeVars = Set.toList . go Set.empty
  where
    go fvs = \case
      TmVar x -> Set.singleton x
      TmLam x ty t -> go fvs t `Set.difference` Set.singleton x
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

runRecon :: Term -> (Ty, Constr)
runRecon = extract . recon [] [] freshVars
  where
    freshVars = map show [1..]
    extract (ty, _, constr) = (ty, constr)

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
isVar _        = False

isArr :: Ty -> Bool
isArr (TyArr _ _) = True
isArr _           = False

notInFv :: Ty -> Ty -> Bool
notInFv (TyId x) t = x `notElem` (fv t)
notInFv _ _        = False

fv :: Ty -> [String]
fv (TyId x)        = [x]
fv (TyArr ty1 ty2) = fv ty1 ++ fv ty2
fv _               = []

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

runUnify :: (Ty, Constr) -> Ty
runUnify (ty, constr) = subst ty (unify constr)
  where
    subst = foldl (flip apply)

calcPrincipalType :: Term -> Ty
calcPrincipalType = runUnify . runRecon

{-
λ> calcPrincipalType ex1
TyNat
-}
ex1 :: Term
ex1 = TmLet "double" var body
  where
    var = TmLam "f" (TyArr TyNat TyNat) $ TmLam "a" TyNat $ TmApp (TmVar "f") (TmApp (TmVar "f") (TmVar "a"))
    body = TmApp (TmApp (TmVar "double") (TmLam "x" TyNat (TmSucc $ TmSucc $ TmVar "x"))) (TmSucc $ TmSucc TmZero)

{-
λ> calcPrincipalType ex2
TyBool
-}
ex2 :: Term
ex2 = TmLet "double" var body
  where
    var = TmLam "f" (TyArr TyBool TyBool) $ TmLam "a" TyBool $ TmApp (TmVar "f") (TmApp (TmVar "f") (TmVar "a"))
    body = TmApp (TmApp (TmVar "double") (TmLam "x" TyBool (TmVar "x"))) TmFalse

{-
let double = \f -> \a:Bool -> f (f a)
in let a = double (\x:Nat  -> succ (succ x)) 2
  in let b = double (\x:Bool -> x) false
    in

λ> calcPrincipalType $ ex3 $ TmVar "a"
TyNat
λ> calcPrincipalType $ ex3 $ TmVar "b"
TyBool
λ> calcPrincipalType $ ex3 $ TmVar "double"
TyArr (TyArr (TyId "?X_4") (TyId "?X_4")) (TyArr (TyId "?X_4") (TyId "?X_4"))
-}
ex3 :: Term -> Term
ex3 body = TmLet "double" decl body1
  where
    decl = TmLamInf "f" $ TmLamInf "a" $ TmApp (TmVar "f") (TmApp (TmVar "f") (TmVar "a"))
    body1 = TmLet "a" (TmApp (TmApp (TmVar "double") (TmLam "x" TyNat (TmSucc $ TmSucc $ TmVar "x"))) (TmSucc $ TmSucc TmZero)) body2
    body2 = TmLet "b" (TmApp (TmApp (TmVar "double") (TmLam "x" TyBool (TmVar "x"))) TmFalse) body

{-
λ> runRecon ex4
(TyNat,[(TyBool,TyArr TyNat (TyId "?X_1"))])

λ> calcPrincipalType ex4
*** Exception: fail
CallStack (from HasCallStack):
  error, called at src/Language/Ex22_7_1.hs:142:17 in main:Ex22_7_1
-}
ex4 :: Term
ex4 = TmLet "x" (TmApp TmTrue TmZero) TmZero
