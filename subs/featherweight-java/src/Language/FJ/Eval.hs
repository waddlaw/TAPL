-- ============================
-- = Figure 19-3: evaluation
-- ============================
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.FJ.Eval
  ( eval, eval'
  , evalTrace
  , isValue
  )
where

import Language.FJ.Helper
import Language.FJ.Type

import RIO hiding (to)
import qualified RIO.List.Partial as List'

isValue :: Term -> Bool
isValue = \case
  TmNew _ ts -> all isValue ts
  _          -> False

checkCast :: ClassTable -> Class -> Class -> Bool
checkCast ct from to = from == to || checkCast ct d to
  where
    CL _ d _ _ _ = ct from

subst :: [(Var, Term)] -> Term -> Term
subst fs = \case
  TmVar x            -> fromMaybe (error "subst") $ lookup x fs
  TmFieldRef t field -> TmFieldRef (subst fs t) field
  TmMethodInv t m ts -> TmMethodInv (subst fs t) m (map (subst fs) ts)
  TmNew  c ts        -> TmNew c (map (subst fs) ts)
  TmCast c t         -> TmCast c (subst fs t)

eval :: ClassTable -> Term -> Term
eval ct t
  | isValue t = t
  | otherwise = eval ct (eval' ct t)

evalTrace :: ClassTable -> Term -> [Term]
evalTrace ct t
  | isValue t = [t]
  | otherwise = t : evalTrace ct (eval' ct t)

eval' :: ClassTable -> Term -> Term
eval' ct = \case
  -- E-NEW-ARG
  TmNew c ts ->
    let (vs, ti:rest) = span isValue ts -- ti:rest is absolutely successful
     in TmNew c (vs ++ (eval' ct ti:rest))

  -- E-PROJNEW
  TmFieldRef t@(isValue -> True) fi ->
    let TmNew c vs = t
     in fst . List'.head . dropWhile ((/=fi) . snd . snd) $ zip vs $ fields ct c
  -- E-FIELD
  TmFieldRef t fi -> TmFieldRef (eval' ct t) fi

  -- E-INVKNEW
  TmMethodInv t@(isValue -> True) m ts@(all isValue -> True) ->
    let this@(TmNew c _vs) = t
        (xs, t0) = fromMaybe (error $ "E-INVKNEW: " <> show m <> ", " <> show c) $ mbody ct m c
     in subst ((mkVar "this", this):zip xs ts) t0
  -- E-INVK-ARG
  TmMethodInv t@(isValue -> True) m ts ->
    let (vs, ti:rest) = span isValue ts -- ti:rest is absolutely successful
     in TmMethodInv t m (vs ++ (eval' ct ti:rest))
  -- E-INVK-RECV
  TmMethodInv t m ts -> TmMethodInv (eval' ct t) m ts

  -- E-CASTNEW
  TmCast d t@(isValue -> True) ->
    let TmNew c' _vs = t
     in if | checkCast ct c' d -> t
           | otherwise -> error "E-CASTNEW"
  -- E-CAST
  TmCast c t -> TmCast c (eval' ct t)

  _ -> error "fail: eval'"