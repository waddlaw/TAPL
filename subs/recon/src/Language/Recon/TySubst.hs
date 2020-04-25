{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Recon.TySubst
  ( tySubst,
    example0,
    example1,
    example2,
    example3,
  )
where

import Language.Recon.Type
import RIO
import qualified RIO.Map as Map

class TySubst a where
  tySubst :: Sigma -> a -> a

instance TySubst Ty where
  tySubst :: Sigma -> Ty -> Ty
  tySubst sigma = \case
    ty@(TyVar x) -> fromMaybe ty (Map.lookup x sigma)
    TyNat -> TyNat
    TyBool -> TyBool
    TyArr ty1 ty2 -> TyArr (tySubst sigma ty1) (tySubst sigma ty2)

instance TySubst Term where
  tySubst :: Sigma -> Term -> Term
  tySubst sigma = \case
    TmVar i -> TmVar i
    TmLam x ty t -> TmLam x (tySubst sigma ty) (tySubst sigma t)
    TmApp t1 t2 -> TmApp (tySubst sigma t1) (tySubst sigma t2)
    TmTrue -> TmTrue
    TmFalse -> TmFalse
    TmIf t1 t2 t3 -> TmIf (tySubst sigma t1) (tySubst sigma t2) (tySubst sigma t3)
    TmZero -> TmZero
    TmSucc t -> TmSucc (tySubst sigma t)
    TmPred t -> TmPred (tySubst sigma t)
    TmIsZero t -> TmIsZero (tySubst sigma t)
    TmFix t -> TmFix (tySubst sigma t)

instance TySubst Context where
  tySubst :: Sigma -> Context -> Context
  tySubst sigma = map (second (tySubst sigma))

-- >>> example0
-- [("x",TyBool),("y",TyBool),("x",TyVar "Y")]
example0 :: Context
example0 = tySubst sigma ctx
  where
    -- σ = [X |-> Bool]
    sigma = Map.singleton "X" TyBool
    -- [x:X, y:X, x:Y]
    ctx = [("x", TyVar "X"), ("y", TyVar "X"), ("x", TyVar "Y")]

-- >>> example1
-- TyArr TyBool TyBool
example1 :: Ty
example1 = tySubst sigma ty
  where
    -- σ = [X |-> Bool]
    sigma = Map.singleton "X" TyBool
    -- X->X
    ty = TyArr (TyVar "X") (TyVar "X")

-- >>> example2
-- TyArr TyBool (TyArr (TyVar "X") (TyVar "X"))
example2 :: Ty
example2 = tySubst sigma ty
  where
    -- σ = [X |-> Bool, Y |-> X->X]
    sigma = Map.fromList [("X", TyBool), ("Y", TyArr (TyVar "X") (TyVar "X"))]
    -- X->Y
    ty = TyArr (TyVar "X") (TyVar "Y")

-- >>> example3
-- TmLam "x" TyBool (TmVar "x")
example3 :: Term
example3 = tySubst sigma term
  where
    -- σ = [X |-> Bool, Y |-> X->X]
    sigma = Map.fromList [("X", TyBool)]
    -- λx:X. x
    term = TmLam "x" (TyVar "X") (TmVar "x")
