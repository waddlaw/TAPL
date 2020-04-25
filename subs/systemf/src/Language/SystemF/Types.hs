{-# LANGUAGE OverloadedStrings #-}

module Language.SystemF.Types
  ( Ty (..),
    Term (..),
    Context,
    addContext,
    toContext,
    unCtx,
    Binding (..),
    pprTerm,
    VarName (..),
    Value,
  )
where

import Data.Text.Prettyprint.Doc
import RIO
import qualified RIO.List.Partial as L.Partial

newtype Context = Context {unCtx :: [Binding]}
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via [Binding]

addContext :: Binding -> Context -> Context
addContext v = Context . (v :) . unCtx

toContext :: Binding -> Context
toContext = Context . pure

data Binding
  = -- | 9-1.  empty context
    EmptyBinding
  | -- | 9-1.  term variable binding
    TermVarBind VarName Ty
  | -- | 23-1. type variable binding
    TypeVarBind TyVarName
  deriving stock (Eq, Show)

instance Pretty Binding where
  pretty = \case
    EmptyBinding -> mempty
    TermVarBind varName _ -> pretty varName
    TypeVarBind tyVarName -> pretty tyVarName

data Ty
  = -- | 8-1.  type of booleans
    TyBool
  | -- | 8-2.  type of natural numbers
    TyNat
  | -- | 9-1.  type of functions
    TyArr Ty Ty
  | -- | 11-13 type of lists
    TyList Ty
  | -- | 23-1  type variable
    TyVar TyVarName Int
  | -- | 23-1. universal type
    TyForAll TyVarName Ty
  deriving stock (Eq, Show)

instance Pretty Ty where
  pretty = pprType mempty

pprType :: Context -> Ty -> Doc ann
pprType ctx = \case
  TyVar _ n ->
    let ctx' = unCtx ctx
        fv = ctx' L.Partial.!! n
     in if length ctx' <= n then "TYFV" <> pretty n else pretty fv
  TyBool -> "Bool"
  TyNat -> "Nat"
  TyArr ty1 ty2 -> wrapPpr ty1 <> "->" <> wrapPpr ty2
  TyForAll tyVarName ty ->
    let ctx' = addContext (TypeVarBind tyVarName) ctx
     in "∀" <> pretty tyVarName <> "." <> pprType ctx' ty
  TyList ty -> brackets (pprType ctx ty)
  where
    wrapPpr a
      | isAtom a = pprType ctx a
      | otherwise = parens (pprType ctx a)

newtype VarName = VarName Text
  deriving stock (Eq, Show)
  deriving (Pretty, IsString) via Text

newtype TyVarName = TyVarName Text
  deriving stock (Eq, Show)
  deriving (Pretty, IsString) via Text

type Value = Term

data Term
  = -- | 3-1.   constant true
    TmTrue
  | -- | 3-1.   constant false
    TmFalse
  | -- | 3-1.   conditional
    TmIf Term Term Term
  | -- | 3-2.   constant zero
    TmZero
  | -- | 3-2.   successor
    TmSucc Term
  | -- | 3-2.   predecessor
    TmPred Term
  | -- | 3-2.   zero test
    TmIsZero Term
  | -- | 9-1.   variable
    TmVar VarName Int
  | -- | 9-1.   abstraction
    TmLam VarName Ty Term
  | -- | 9-1.   application
    TmApp Term Term
  | -- | 11-12. fixed point of t
    TmFix Term
  | -- | 11-13. empty list
    TmNil
  | -- | 11-13. list constructor
    TmCons
  | -- | 11-13. test for empty list
    TmIsNil
  | -- | 11-13. head of a list
    TmHead
  | -- | 11-13. tail of a list
    TmTail
  | -- | 23-1.  type abstraction
    TmTypeLam TyVarName Term
  | -- | 23-1.  type application
    TmTypeApp Term Ty
  deriving stock (Eq, Show)

instance Pretty Term where
  pretty = pprTerm mempty

pprTerm :: Context -> Term -> Doc ann
pprTerm ctx = \case
  TmVar _ n ->
    let ctx' = unCtx ctx
        fv = ctx' L.Partial.!! n
     in if length ctx' <= n then "FV" <> pretty n else pretty fv
  TmLam x ty t ->
    let ctx' = addContext (TermVarBind x ty) ctx
     in "λ" <> pretty x <> ":" <> pprType ctx' ty <> "." <+> pprTerm ctx' t
  TmApp t1 t2 -> wrapPpr t1 <+> wrapPpr t2
  TmTypeLam tyVarName t ->
    let ctx' = addContext (TypeVarBind tyVarName) ctx
     in "λ" <> pretty tyVarName <> "." <+> pprTerm ctx' t
  TmTypeApp t ty -> wrapPpr t <+> brackets (pprType ctx ty)
  TmZero -> "0"
  TmSucc t -> "succ" <+> parens (pprTerm ctx t)
  TmPred t -> "pred" <+> parens (pprTerm ctx t)
  TmIsZero t -> "iszero" <+> parens (pprTerm ctx t)
  TmTrue -> "true"
  TmFalse -> "false"
  TmIf t1 t2 t3 ->
    "if" <+> pprTerm ctx t1
      <+> "then"
      <+> pprTerm ctx t2
      <+> "else"
      <+> pprTerm ctx t3
  -- 11-12. General recursion
  TmFix t -> "fix" <+> pprTerm ctx t
  -- 11-13. Lists
  TmNil -> "[]"
  TmCons -> "(:)"
  TmIsNil -> "isnil"
  TmHead -> "head"
  TmTail -> "tail"
  where
    wrapPpr a
      | isAtom a = pprTerm ctx a
      | otherwise = parens (pprTerm ctx a)

class IsAtom a where
  isAtom :: a -> Bool

instance IsAtom Ty where
  isAtom = \case
    TyNat -> True
    TyBool -> True
    TyArr {} -> False
    TyVar {} -> True
    TyForAll {} -> False
    TyList {} -> False

instance IsAtom Term where
  isAtom = \case
    TmVar {} -> True
    TmLam {} -> False
    TmApp {} -> False
    TmTypeLam {} -> False
    TmTypeApp {} -> False
    TmTrue -> True
    TmFalse -> True
    TmIf {} -> False
    TmZero -> True
    TmSucc {} -> False
    TmPred {} -> False
    TmIsZero {} -> False
    TmFix {} -> False
    TmNil -> True
    TmCons {} -> False
    TmIsNil {} -> False
    TmHead {} -> False
    TmTail {} -> False
