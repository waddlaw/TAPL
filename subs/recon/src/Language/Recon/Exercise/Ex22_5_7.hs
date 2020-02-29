{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Language.Recon.Exercise.Ex22_5_7
  ( runTypingC
  , example1
  , example2
  , example3
  )
where

import RIO
import qualified RIO.List as List
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
  deriving (Eq, Show)

type ConstraintSet = [(Ty, Ty)]
type Context = [(VarName, Ty)]
type VarName = Text
type ReturnType = Ty
type TyVarIdStream = [VarName]

runTypingC :: Term -> ReturnType
runTypingC = extract . typingC [] tyVarIdStream
  where
    tyVarIdStream = map (tshow @Int) [1..]
    extract (ty, _, _) = ty

typingC ::
  Context ->
  TyVarIdStream ->
  Term ->
  (ReturnType, TyVarIdStream, ConstraintSet)
typingC ctx varIds = \case
  TmVar x ->
    let msg = "Variable '" <> Text.unpack x <> "' is not found in context: " <> show ctx
        ty = fromMaybe (error msg) $ List.lookup x ctx
     in (ty, varIds, [])
  TmLam x ty t ->
    let (rt, restVarIds, c) = typingC ((x,ty):ctx) varIds t
     in (subst (TyArr ty rt) c, restVarIds, c)
  TmApp t1 t2 ->
    let (rt1, restVarIds1, c1) = typingC ctx varIds t1
        (rt2, x:restVarIds2, c2) = typingC ctx restVarIds1 t2
        rt = TyVar x
        c = unify (c1 <> c2 <> [(rt1, TyArr rt2 rt)])
     in (subst rt c, restVarIds2, c)
  TmTrue  -> (TyBool, varIds, [])
  TmFalse -> (TyBool, varIds, [])
  TmIf t1 t2 t3 ->
    let (rt1, restVarIds1, c1) = typingC ctx varIds t1
        (rt2, restVarIds2, c2) = typingC ctx restVarIds1 t2
        (rt3, restVarIds3, c3) = typingC ctx restVarIds2 t3
        c = unify (c1 <> c2 <> c3 <> [(rt1, TyBool), (rt2, rt3)])
     in (subst rt2 c, restVarIds3, c)
  TmZero -> (TyNat, varIds, [])
  TmSucc t ->
    let (rt, restVarIds, c) = typingC ctx varIds t
        c' = unify (c <> [(rt, TyNat)])
     in (TyNat, restVarIds, c')
  TmPred t ->
    let (rt, restVarIds, c) = typingC ctx varIds t
        c' = unify (c <> [(rt, TyNat)])
     in (TyNat, restVarIds, c')
  TmIsZero t ->
    let (rt, restVarIds, c) = typingC ctx varIds t
        c' = unify (c <> [(rt, TyNat)])
     in (TyBool, restVarIds, c')

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
fv (TyArr ty1 ty2) = fv ty1 ++ fv ty2
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

subst :: Ty -> ConstraintSet -> Ty
subst = List.foldl (flip apply)

-- >>> runTypingC example1
-- TyArr ( TyVar "X" ) ( TyVar "X" )
example1 :: Term
example1 = TmLam "x" (TyVar "X") (TmVar "x")

-- >>> runTypingC example2
-- TyArr 
--    ( TyArr ( TyVar "?X_1" ) ( TyVar "?X_2" ) ) 
--    ( TyArr 
--        ( TyArr TyBool ( TyVar "?X_1" ) ) ( TyVar "?X_2" )
--    )
example2 :: Term
example2 = TmLam "z" (TyVar "ZZ") . TmLam "y" (TyVar "YY") $ TmApp (TmVar "z") (TmApp (TmVar "y") TmTrue)

-- >>> runTypingC example3
-- TyArr ( TyArr TyBool TyBool ) TyBool
example3 :: Term
example3 = TmLam "w" (TyVar "W") $ TmIf TmTrue TmFalse (TmApp (TmVar "w") TmFalse)