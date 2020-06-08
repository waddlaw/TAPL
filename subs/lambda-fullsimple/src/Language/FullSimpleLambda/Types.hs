{-# LANGUAGE OverloadedStrings #-}

module Language.FullSimpleLambda.Types
  ( Ty (..),
    Term (..),
    Value,
    Context,
    ContextType (..),
    addContext,
    unCtx,
    Binding (..),
    FullSimpleTypedLambda,
    pprFullSimple,
    VarName,
    Pattern (..),
    FieldLabel,
  )
where

import Data.Text.Prettyprint.Doc
import RIO
import qualified RIO.List.Partial as L.Partial
import qualified RIO.Text as Text

type FullSimpleTypedLambda = Term

-- | Term の部分集合
type Value = Term

type VarName = Text

newtype Context = Context {unCtx :: [(ContextType, Binding)]}
  deriving stock (Eq, Show)

instance Semigroup Context where
  ctx1 <> ctx2 = Context (unCtx ctx1 <> unCtx ctx2)

instance Monoid Context where
  mempty = Context []

instance IsString Context where
  fromString v = Context [(VarContext (Text.pack v), NameBind)]

addContext :: (ContextType, Binding) -> Context -> Context
addContext v = Context . (v :) . unCtx

data ContextType
  = VarContext {unWrapVarContext :: Text}
  | PatternContext Pattern
  deriving stock (Eq, Show)

instance IsString ContextType where
  fromString = VarContext . Text.pack

instance Pretty ContextType where
  pretty = \case
    VarContext varName -> pretty varName
    PatternContext p -> pretty p

data Binding
  = -- | 自由変数
    NameBind
  | -- | 型付きの変数
    VarBind Ty
  | -- | 型付きのパターン
    PatternBind Ty
  deriving stock (Eq, Show)

data Ty
  = -- | 関数型
    TyArr Ty Ty
  | -- | ブール値型
    TyBool
  | -- | 自然数型
    TyNat
  | -- | 11.2 Unit type
    TyUnit
  | -- | 11.6 直積型
    TyProd Ty Ty
  | -- | 11.7 組の型
    TyTuple [Ty]
  | -- | 11.8 レコードの型
    TyRecord [(FieldLabel, Ty)]
  | -- | 11.9 和型
    TySum Ty Ty
  deriving stock (Eq, Show)

instance Pretty Ty where
  pretty = \case
    TyBool -> "Bool"
    TyNat -> "Nat"
    TyUnit -> "Unit"
    TyArr ty1 ty2 ->
      let ppr' t@TyBool = pretty t
          ppr' t = parens (pretty t)
       in ppr' ty1 <+> "->" <+> pretty ty2
    TyProd ty1 ty2 -> pretty ty1 <+> "×" <+> pretty ty2
    TyTuple ts -> encloseSep lbrace rbrace comma (map pretty ts)
    TyRecord fields ->
      let pprField (label, ty) = pretty label <> ";" <> pretty ty
       in encloseSep lbrace rbrace comma (map pprField fields)
    TySum ty1 ty2 -> pretty ty1 <> "+" <> pretty ty2

-- | レコードのフィールドラベル
type FieldLabel = Text

-- type Alts = [Alt]

-- | (variable, body)
type Alt = (Term, Term)

data Term
  = TmVar Int
  | TmLam VarName Ty Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmZero
  | TmSucc Term
  | TmPred Term
  | TmIsZero Term
  | -- | 11.2   Constant unit
    TmUnit
  | -- | 11.3   Sequencing
    TmSeq Term Term
  | -- | 11.3   Wildcards
    TmWildcard Ty Term
  | -- | 11.3   Ascription
    TmAscribe Term Ty
  | -- | 11.4   Let binding
    TmLet VarName Term Term
  | -- | 11.5   Pair
    TmPair Term Term
  | -- | 11.5   First projection
    TmPairFst Term
  | -- | 11.5   Second projection
    TmPairSnd Term
  | -- | 11.6   Tuple
    TmTuple [Term]
  | -- | 11.6   Projection
    TmTupleProj Int Term
  | -- | 11.7   Record (Treat fields in different order as different records)
    TmRecord [(FieldLabel, Term)]
  | -- | 11.7   Record projection
    TmRecordProj FieldLabel Term
  | -- | 11.8   Pattern binding
    TmPattern Pattern Term Term
  | -- | 11.9   Tagging (left)
    TmInL Term Ty
  | -- | 11.9   Tagging (right)
    TmInR Term Ty
  | -- | 11.9   Case
    TmCase Term Alt Alt
  deriving stock (Eq, Show)

instance Pretty Term where
  pretty = pprFullSimple mempty

pprFullSimple :: Context -> Term -> Doc ann
pprFullSimple ctx = \case
  TmVar n ->
    let ctx' = unCtx ctx
        fv = fst (ctx' L.Partial.!! n)
     in if
            | length ctx' <= n -> "FV" <> pretty n
            | otherwise -> pretty fv
  TmLam x ty t ->
    let ctx' = addContext (VarContext x, VarBind ty) ctx
     in "λ" <> pretty x <> ":" <> pretty ty <> "." <+> pprFullSimple ctx' t
  TmApp t1 t2 ->
    let ppr t@(TmVar _) = pprFullSimple ctx t
        ppr t@TmTrue = pprFullSimple ctx t
        ppr t@TmFalse = pprFullSimple ctx t
        ppr t = parens (pprFullSimple ctx t)
     in ppr t1 <+> ppr t2
  TmTrue -> "true"
  TmFalse -> "false"
  TmIf t1 t2 t3 ->
    "if" <+> pprFullSimple ctx t1
      <+> "then"
      <+> pprFullSimple ctx t2
      <+> "else"
      <+> pprFullSimple ctx t3
  TmZero -> "0"
  TmSucc t -> "succ" <+> pprFullSimple ctx t
  TmPred t -> "pred" <+> pprFullSimple ctx t
  TmIsZero t -> "iszero" <+> pprFullSimple ctx t
  TmUnit -> "()"
  TmSeq t1 t2 -> pprFullSimple ctx t1 <> ";" <> pprFullSimple ctx t2
  TmWildcard ty t -> "λ_:" <> pretty ty <> "." <+> pprFullSimple ctx t
  TmAscribe t ty -> pprFullSimple ctx t <+> "as" <+> pretty ty <+> ":" <+> pretty ty
  TmLet var tlet tbody ->
    let ctx' = addContext (VarContext var, undefined) ctx
     in "let" <+> pretty var <> "=" <> pprFullSimple ctx tlet <+> "in" <+> pprFullSimple ctx' tbody
  TmPair t1 t2 -> "{" <> pprFullSimple ctx t1 <> "," <> pprFullSimple ctx t2 <> "}"
  TmPairFst t -> pprFullSimple ctx t <> ".1"
  TmPairSnd t -> pprFullSimple ctx t <> ".2"
  TmTuple ts -> encloseSep lbrace rbrace comma (map (pprFullSimple ctx) ts)
  TmTupleProj i t -> pprFullSimple ctx t <> "." <> pretty i
  TmRecord fields ->
    let pprField (label, t) = pretty label <> "=" <> pprFullSimple ctx t
     in encloseSep lbrace rbrace comma $ map pprField fields
  TmRecordProj label t -> pprFullSimple ctx t <> dot <> pretty label
  TmPattern p tlet tbody ->
    let ctx' = getContext p
     in "let" <+> pprPattern ctx' p <> "=" <> pprFullSimple ctx tlet
          <+> "in"
          <+> pprFullSimple ctx' tbody
  TmInL t ty -> "inl" <+> pprFullSimple ctx t <+> "as" <+> pretty ty
  TmInR t ty -> "inr" <+> pprFullSimple ctx t <+> "as" <+> pretty ty
  TmCase t altL altR ->
    "case" <+> pprFullSimple ctx t
      <+> "of"
      <+> "inl"
      <+> pprAlt ctx altL
      <+> pipe
      <+> "inr"
      <+> pprAlt ctx altR

getContext :: Pattern -> Context
getContext = \case
  PtVar varName _ -> addContext (VarContext varName, NameBind) mempty
  PtRecord fs -> foldMap (getContext . snd) fs

-- | Exercise 11.8 pattern match
data Pattern
  = -- | variable pattern
    PtVar VarName Int
  | -- | record pattern
    PtRecord [(FieldLabel, Pattern)]
  deriving stock (Eq, Show)

instance Pretty Pattern where
  pretty = pprPattern mempty

pprPattern :: Context -> Pattern -> Doc ann
pprPattern ctx = \case
  PtVar varName n ->
    let ctx' = unCtx ctx
     in if
            | length ctx' <= n -> "FV" <> pretty n
            | otherwise -> pretty varName
  PtRecord fs ->
    let pprField (label, p) = pretty label <> "=" <> pprPattern ctx p
     in encloseSep lbrace rbrace comma $ map pprField fs

pprAlt :: Context -> Alt -> Doc ann
pprAlt ctx (t1, t2) = pprFullSimple ctx t1 <+> "=>" <+> pprFullSimple ctx t2

-- pprAlts :: Context -> Alts -> Doc ann
-- pprAlts ctx = sep . punctuate (mempty <+> pipe) . map (pprAlt ctx)
