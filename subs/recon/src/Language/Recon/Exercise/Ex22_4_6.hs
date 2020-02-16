{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Language.Recon.Exercise.Ex22_4_6 where

import RIO

data Ty
  = TyBool
  | TyNat
  | TyArr Ty Ty
  | TyVar VarName
  deriving (Eq, Show, Ord)

type ConstraintSet = [(Ty, Ty)]
type VarName = Text

unify :: ConstraintSet -> Maybe ConstraintSet
unify [] = Just []
unify ((s, t):c')
  | s == t = unify c'
  | isVar s && s `notInFv` t =
      let sigma = (s, t)
       in (sigma :) <$> unify (map (applyC sigma) c')
  | isVar t && t `notInFv` s =
      let sigma = (t, s)
       in (sigma :) <$> unify (map (applyC sigma) c')
  | isArr s && isArr t =
      let TyArr s1 s2 = s
          TyArr t1 t2 = t
       in unify ([(s1, t1), (s2, t2)] ++ c')
  | otherwise = Nothing

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

-- examples

-- >>> unify ex22_4_3_1
-- [(TyVar "X",TyNat),(TyVar "Y",TyArr TyNat TyNat)]
ex22_4_3_1 :: ConstraintSet
ex22_4_3_1 = [(x, TyNat), (TyVar "Y", TyArr x x)]
  where
    x = TyVar "X"

-- >>> unify ex22_4_3_2
-- [(TyVar "X",TyNat),(TyVar "Y",TyNat)]
ex22_4_3_2 :: ConstraintSet
ex22_4_3_2 = [ (TyArr TyNat TyNat, TyArr (TyVar "X") (TyVar "Y")) ]

-- >>> unify ex22_4_3_3
-- [ (TyVar "X",TyArr (TyVar "U") (TyVar "W"))
-- , (TyVar "Y",TyArr (TyVar "U") (TyVar "W"))
-- , (TyVar "Z",TyArr (TyVar "U") (TyVar "W"))
-- ]
ex22_4_3_3 :: ConstraintSet
ex22_4_3_3 =
  [ (TyArr (TyVar "X") (TyVar "Y"), TyArr (TyVar "Y") (TyVar "Z"))
  , (TyVar "Z", TyArr (TyVar "U") (TyVar "W"))
  ]

-- >>> unify ex22_4_3_4
-- *** Exception: fail
ex22_4_3_4 :: ConstraintSet
ex22_4_3_4 = [ (TyNat, TyArr TyNat (TyVar "Y")) ]

-- >>> unify ex22_4_3_5
-- *** Exception: fail
ex22_4_3_5 :: ConstraintSet
ex22_4_3_5 = [ (TyVar "Y", TyArr TyNat (TyVar "Y")) ]

-- >>> unify ex22_4_3_6
-- []
ex22_4_3_6 :: ConstraintSet
ex22_4_3_6 = []

-- >>> unify ex22_5_2 
-- [ (TyVar "X"    , TyArr (TyVar "Z")    (TyVar "?X_1"))
-- , (TyVar "Y"    , TyArr (TyVar "Z")    (TyVar "?X_2"))
-- , (TyVar "?X_1" , TyArr (TyVar "?X_2") (TyVar "?X_3"))
-- ]
ex22_5_2 :: ConstraintSet
ex22_5_2 =
  [ (TyVar "X"   , TyArr (TyVar "Z")    (TyVar "?X_1"))
  , (TyVar "Y"   , TyArr (TyVar "Z")    (TyVar "?X_2"))
  , (TyVar "?X_1", TyArr (TyVar "?X_2") (TyVar "?X_3"))
  ]