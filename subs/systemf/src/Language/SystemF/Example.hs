{-# LANGUAGE OverloadedStrings #-}

module Language.SystemF.Example where

import Language.SystemF.Types
import RIO

-- λX. λx:X. x
identity :: Term
identity = TmTypeLam "X" . TmLam "x" (TyVar "X" 1) $ TmVar "x" 0

-- λX. λf:X->X. λa:X. f (f a)
double :: Term
double = TmTypeLam "X" . TmLam "f" (TyArr (TyVar "X" 1) (TyVar "X" 1)) . TmLam "a" (TyVar "X" 2) $ TmApp (TmVar "f" 1) (TmApp (TmVar "f" 1) (TmVar "a" 0))

-- double [Nat]
doubleNat :: Term
doubleNat = TmTypeApp double TyNat

-- double [Nat->Nat]
doubleNatArrowNat :: Term
doubleNatArrowNat = TmTypeApp double (TyArr TyNat TyNat)

-- λX. double [X->X] (double [X])
quadruple :: Term
quadruple = TmTypeLam "X" $ TmApp (TmTypeApp double (TyArr (TyVar "X" 0) (TyVar "X" 0))) (TmTypeApp double (TyVar "X" 0))

-- λx:∀X.X->X. x [∀X.X->X] x
selfApp :: Term
selfApp = TmLam "x" (TyForAll "X" (TyArr (TyVar "X" 0) (TyVar "X" 0))) $ TmApp (TmTypeApp (TmVar "x" 0) (TyForAll "X" (TyArr (TyVar "X" 0) (TyVar "X" 0)))) (TmVar "x" 0)

map' :: Term
map' =
  TmTypeLam "X" $ TmTypeLam "Y"
    $ TmLam "f" (TyArr (TyVar "X" 1) (TyVar "Y" 0))
    $ TmFix
    $ TmLam "m" (TyArr (TyList $ TyVar "X" 3) (TyList $ TyVar "Y" 2))
    $ TmLam "l" (TyList $ TyVar "X" 4)
    $ TmIf
      (TmApp (TmTypeApp TmIsNil (TyVar "X" 4)) (TmVar "l" 0))
      (TmTypeApp TmNil (TyVar "Y" 3))
      ( TmApp
          (TmApp (TmTypeApp TmCons (TyVar "Y" 3)) (TmApp (TmVar "f" 2) e1))
          (TmApp (TmVar "m" 1) e2)
      )
  where
    e1 = TmApp (TmTypeApp TmHead (TyVar "X" 4)) (TmVar "l" 0)
    e2 = TmApp (TmTypeApp TmTail (TyVar "X" 4)) (TmVar "l" 0)
