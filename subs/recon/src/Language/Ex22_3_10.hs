{-# LANGUAGE LambdaCase #-}
module Ex22_3_10 where

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

runRecon :: Term -> (Ty, Constr)
runRecon = extract . recon [] [] freshVars
  where
    freshVars = map show [1..]
    extract (ty, _, constr) = (ty, constr)

{-
λ> runRecon (TmApp TmZero TmTrue)
(TyId "?X_1",[(TyNat,TyArr TyBool (TyId "?X_1"))])
-}

ex22_3_3 :: Term
ex22_3_3 = TmLam "x" (TyId "X") . TmLam "y" (TyId "Y") . TmLam "z" (TyId "Z") $ body
  where
    body = TmApp t1 t2
    t1 = TmApp (TmVar "x") (TmVar "z")
    t2 = TmApp (TmVar "y") (TmVar "z")

{-
λ> runRecon ex22_3_3 
( TyArr (TyId "X") (TyArr (TyId "Y") (TyArr (TyId "Z") (TyId "?X_3"))),[(TyId "X",TyArr (TyId "Z") (TyId "?X_1"))
, (TyId "Y",TyArr (TyId "Z") (TyId "?X_2")),(TyId "?X_1",TyArr (TyId "?X_2") (TyId "?X_3"))]
)
-}