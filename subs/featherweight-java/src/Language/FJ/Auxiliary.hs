-- =======================================
-- = Figure 19-2: auxiliary definitions
-- =======================================
{-# LANGUAGE OverloadedStrings #-}

module Language.FJ.Auxiliary
  ( fields,
    mtype,
    mbody,
  )
where

import Language.FJ.Type
import RIO
import qualified RIO.List.Partial as List'

fields :: ClassTable -> Class -> [(Class, Field)]
fields ct = \case
  (getClassName -> "Object") -> []
  c ->
    let CL _ d cfs _ _ = ct c
     in cfs ++ fields ct d

mtype :: ClassTable -> Method -> Class -> Maybe ([Class], Class)
mtype ct m = fmap f . maybeDefined ct m
  where
    f (M rt _ args _) = (map fst args, rt)

-- |
-- >>> mbody exCT (MN "setfst") (CN "Pair")
-- ([VN "newfst"],TmNew (CN "Pair") [TmVar (VN "newfst"),TmFieldRef (TmVar (VN "this")) (FN "snd")])
mbody :: ClassTable -> Method -> Class -> Maybe ([Var], Term)
mbody ct m = fmap f . maybeDefined ct m
  where
    f (M _ _ args term) = (map snd args, term)

maybeDefined :: ClassTable -> Method -> Class -> Maybe MethodDef
maybeDefined ct m = \case
  (getClassName -> "Object") -> Nothing
  c ->
    let CL _ d _cfs _ ms = ct c
     in findMethodDef m ms <|> maybeDefined ct m d

eqMethodDef :: Method -> MethodDef -> Bool
eqMethodDef m1 (M _ m2 _ _) = m1 == m2

findMethodDef :: Method -> [MethodDef] -> Maybe MethodDef
findMethodDef m ms
  | null md = Nothing
  | otherwise = Just (List'.head md)
  where
    md = filter (eqMethodDef m) ms

-- TODO
-- override :: CT -> Method -> Class -> ([Class], Class) -> Bool
-- override ct m d (cs, c0) = case mtype ct m d of
--   Nothing       -> True
--   Just (ds, d0) -> cs == ds && c0 == d0
