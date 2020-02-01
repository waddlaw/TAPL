{-# LANGUAGE OverloadedStrings #-}
module Language.SystemF.Example where

import RIO
import Language.SystemF.Types

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

-- nil = λX. 
nil :: Term
nil = 