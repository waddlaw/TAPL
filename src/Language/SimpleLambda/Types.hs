{-# LANGUAGE NoImplicitPrelude #-}
module Language.SimpleLambda.Types
  ( Ty (..)
  , Term (..)
  , Context
  , Binding (..)
  , TypedLambda
  ) where

import           RIO

import           Data.Text.Prettyprint.Doc

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
  = TmVar Int -- FIXME
  | TmLam Text Ty Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  deriving Eq

instance Pretty Term where
  pretty (TmVar x)      = pretty x
  pretty (TmLam x ty t) = pretty "λ" <> pretty x <> pretty ":" <> pretty ty <> pretty "." <+> pretty t
  pretty (TmApp t1 t2)  = ppr t1 <+> ppr t2
    where
      ppr t@(TmVar _) = pretty t
      ppr t           = parens (pretty t)
  pretty TmTrue = pretty "true"
  pretty TmFalse = pretty "false"
  pretty (TmIf t1 t2 t3) = pretty "if" <+> pretty t1 <+> pretty "then" <+> pretty t2 <+> pretty "else" <+> pretty t3

instance Pretty Ty where
  pretty TyBool          = pretty "Bool"
  pretty (TyArr ty1 ty2) = ppr ty1 <+> pretty "->" <+> pretty ty2
    where
      ppr t@TyBool = pretty t
      ppr t        = parens (pretty t)
