module Language.Recon.Type
  ( Ty (..)
  , VarName
  , Term (..)
  , Context
  , TyVarName
  , Sigma
  , ConstraintSet
  , ReturnType
  )
where

import RIO

data Ty
  = TyArr Ty Ty
  | TyBool
  | TyNat
  | TyVar VarName
  deriving stock (Eq, Show, Ord)

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
  | TmFix Term
  deriving stock (Eq, Show)

type Context = [(VarName, Ty)]
type VarName = Text
type TyVarName = Text

-- 型代入
type Sigma = Map TyVarName Ty

type ConstraintSet = Set (Ty, Ty)
type ReturnType = Ty
