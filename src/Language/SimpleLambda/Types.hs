{-# LANGUAGE NoImplicitPrelude #-}
module Language.SimpleLambda.Types
  ( Ty (..)
  , Term (..)
  , Context
  , Binding (..)
  , TypedLambda
  ) where

import RIO

type TypedLambda = Term
type Context = [(Text, Binding)]

data Binding
  = NameBind
  | VarBind Ty

data Ty
  = TyArr Ty Ty  -- ^ 関数型
  | TyBool       -- ^ Bool型
  deriving Eq

data Term
  = TmVar Int Int -- FIXME
  | TmAbs Text Ty Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  deriving Eq