{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Language.Recon.Exercise.Ex22_4_6 where

import RIO
import qualified RIO.List as List
import qualified RIO.List.Partial as List.Partial
import qualified RIO.Set as Set

data Ty
  = TyBool
  | TyNat
  | TyArr Ty Ty
  | TyVar VarName
  deriving stock (Eq, Show, Ord)

r :: [(Ty, Ty)]
r = [(TyVar "?X_1", TyArr (TyVar "?X_2") (TyVar "?X_3")), (TyVar "X", TyArr (TyVar "Z") (TyArr (TyVar "?X_2") (TyVar "?X_3"))), (TyVar "Y", TyArr (TyVar "Z") (TyVar "?X_2"))]

pretty :: Ty -> Text
pretty = \case
  TyBool -> "Bool"
  TyNat -> "Nat"
  TyArr ty1 ty2 -> pretty ty1 <> " -> " <> pretty ty2
  TyVar x -> x

type Constraint = (Ty, Ty)

type VarName = Text

unify :: Set Constraint -> Maybe (Set Constraint)
unify = fmap (Set.fromList . go . reverse) . unify1
  where
    go [] = []
    go (c@(s, t) : cs) = (s, composeC0 cs t) : go (map (composeC1 c) cs)

unify1 :: Set Constraint -> Maybe [Constraint]
unify1 c
  | Set.null c = Just []
  | Set.size c == 1 = unify2 . List.Partial.head . Set.toList $ c
  | otherwise = Just . concat . mapMaybe unify1 . Set.splitRoot $ c

unify2 :: Constraint -> Maybe [Constraint]
unify2 (s, t)
  | s == t = Just []
  | isVar s && s `notInFv` t = Just [(s, t)]
  | isVar t && t `notInFv` s = Just [(t, s)]
  | isArr s && isArr t =
    let TyArr s1 s2 = s
        TyArr t1 t2 = t
     in unify1 (Set.fromList [(s1, t1), (s2, t2)])
  | otherwise = Nothing

-- utils
isVar :: Ty -> Bool
isVar TyVar {} = True
isVar _ = False

isArr :: Ty -> Bool
isArr TyArr {} = True
isArr _ = False

notInFv :: Ty -> Ty -> Bool
notInFv (TyVar x) t = x `notElem` fv t
notInFv _ _ = False

fv :: Ty -> [Text]
fv (TyVar x) = [x]
fv (TyArr ty1 ty2) = fv ty1 ++ fv ty2
fv _ = []

composeC0 :: [Constraint] -> Ty -> Ty
composeC0 cs = \case
  t@TyVar {} -> fromMaybe t $ List.lookup t cs
  TyArr ty1 ty2 -> (TyArr `on` composeC0 cs) ty1 ty2
  ty -> ty

composeC1 :: (Ty, Ty) -> (Ty, Ty) -> (Ty, Ty)
composeC1 sigma = fork (apply sigma)
  where
    fork f (a, b) = (f a, f b)

apply :: (Ty, Ty) -> Ty -> Ty
apply _ TyBool = TyBool
apply _ TyNat = TyNat
apply sigma (TyArr ty1 ty2) = (TyArr `on` apply sigma) ty1 ty2
apply (s, t) u
  | s == u = t
  | otherwise = u

-- examples

-- >>> unify ex22_4_3_1
-- [(TyVar "X",TyNat),(TyVar "Y",TyArr TyNat TyNat)]
ex22_4_3_1 :: Set Constraint
ex22_4_3_1 = Set.fromList [(x, TyNat), (TyVar "Y", TyArr x x)]
  where
    x = TyVar "X"

-- >>> unify ex22_4_3_2
-- [(TyVar "X",TyNat),(TyVar "Y",TyNat)]
ex22_4_3_2 :: Set Constraint
ex22_4_3_2 = Set.singleton (TyArr TyNat TyNat, TyArr (TyVar "X") (TyVar "Y"))

-- >>> unify ex22_4_3_3
-- [ (TyVar "X",TyArr (TyVar "U") (TyVar "W"))
-- , (TyVar "Y",TyArr (TyVar "U") (TyVar "W"))
-- , (TyVar "Z",TyArr (TyVar "U") (TyVar "W"))
-- ]
ex22_4_3_3 :: Set Constraint
ex22_4_3_3 =
  Set.fromList
    [ (TyArr (TyVar "X") (TyVar "Y"), TyArr (TyVar "Y") (TyVar "Z")),
      (TyVar "Z", TyArr (TyVar "U") (TyVar "W"))
    ]

-- >>> unify ex22_4_3_4

-- *** Exception: fail

ex22_4_3_4 :: Set Constraint
ex22_4_3_4 = Set.singleton (TyNat, TyArr TyNat (TyVar "Y"))

-- >>> unify ex22_4_3_5

-- *** Exception: fail

ex22_4_3_5 :: Set Constraint
ex22_4_3_5 = Set.singleton (TyVar "Y", TyArr TyNat (TyVar "Y"))

-- >>> unify ex22_4_3_6
-- []
ex22_4_3_6 :: Set Constraint
ex22_4_3_6 = Set.empty

-- >>> unify ex22_5_2
-- [ (TyVar "X"    , TyArr (TyVar "Z")    (TyVar "?X_1"))
-- , (TyVar "Y"    , TyArr (TyVar "Z")    (TyVar "?X_2"))
-- , (TyVar "?X_1" , TyArr (TyVar "?X_2") (TyVar "?X_3"))
-- ]
ex22_5_2 :: Set Constraint
ex22_5_2 =
  Set.fromList
    [ (TyVar "X", TyArr (TyVar "Z") (TyVar "?X_1")),
      (TyVar "Y", TyArr (TyVar "Z") (TyVar "?X_2")),
      (TyVar "?X_1", TyArr (TyVar "?X_2") (TyVar "?X_3"))
    ]
