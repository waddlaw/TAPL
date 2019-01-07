{-# LANGUAGE NoImplicitPrelude #-}
module Language.FullSimpleLambda.Types
  ( Ty (..)
  , Term (..)
  , Value
  , Context
  , addContext
  , unCtx
  , Binding (..)
  , FullSimpleTypedLambda
  , pprFullSimple
  , VarName
  ) where

import           RIO
import qualified RIO.List.Partial          as L.Partial
import qualified RIO.Text                  as Text

import           Data.Text.Prettyprint.Doc

type FullSimpleTypedLambda = Term

type Value = Term -- ^ Term の部分集合
type VarName = Text

newtype Context = Context { unCtx :: [(Text, Binding)] }
  deriving (Eq, Show)

instance Semigroup Context where
  ctx1 <> ctx2 = Context (unCtx ctx1 <> unCtx ctx2)

instance Monoid Context where
  mempty = Context []

instance IsString Context where
  fromString v = Context [(Text.pack v, NameBind)]

addContext :: (Text, Binding) -> Context -> Context
addContext v = Context . (v:) . unCtx

data Binding
  = NameBind   -- ^ 自由変数
  | VarBind Ty -- ^ 型付きの変数
  deriving (Eq, Show)

data Ty
  = TyArr Ty Ty   -- ^ 関数型
  | TyBool        -- ^ ブール値型
  | TyNat         -- ^ 自然数型
  | TyUnit        -- ^ 11.2 Unit型
  | TyProd Ty Ty  -- ^ 11.6 直積型
  | TyTuple [Ty]  -- ^ 11.7 組の型
  deriving (Eq, Show)

instance Pretty Ty where
  pretty TyBool = pretty "Bool"
  pretty TyNat  = pretty "Nat"
  pretty TyUnit = pretty "Unit"
  pretty (TyArr ty1 ty2) = ppr' ty1 <+> pretty "->" <+> pretty ty2
    where
      ppr' t@TyBool = pretty t
      ppr' t        = parens (pretty t)
  pretty (TyProd ty1 ty2) = pretty ty1 <+> pretty "×" <+> pretty ty2
  pretty (TyTuple ts) = encloseSep lbrace rbrace comma (map pretty ts)

type FieldLabel = Text

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
  | TmUnit                   -- ^ 11.2 Unit 型
  | TmSeq Term Term          -- ^ 11.3 逐次実行
  | TmWildcard Ty Term       -- ^ 11.3 ワイルドカード
  | TmAscribe Term Ty        -- ^ 11.4 型指定
  | TmLet VarName Term Term  -- ^ 11.5 let
  | TmPair Term Term         -- ^ 11.6 2つ組
  | TmPairFst Term           -- ^ 11.6 第一要素の射影
  | TmPairSnd Term           -- ^ 11.6 第二要素の射影
  | TmTuple [Term]           -- ^ 11.7 組
  | TmTupleProj Int Term     -- ^ 11.7 射影
  | TmRecord [(FieldLabel, Term)] -- ^ 11.8 レコード (フィールドの順序が異なれば、異なるレコードとして扱う)
  deriving (Eq, Show)

instance Pretty Term where
  pretty = pprFullSimple mempty

pprFullSimple :: Context -> Term -> Doc ann
pprFullSimple _ TmUnit = pretty "unit"
pprFullSimple ctx (TmVar n) =
    if length ctx' <= n
    then pretty "FV" <> pretty n
    else pretty fv
  where
    ctx' = unCtx ctx
    fv = fst (ctx' L.Partial.!! n)
pprFullSimple ctx (TmLam x ty t) = pretty "λ" <> pretty x <> pretty ":" <> pretty ty <> pretty "." <+> pprFullSimple ctx' t
  where ctx' = addContext (x, VarBind ty) ctx
pprFullSimple ctx (TmApp t1 t2)  = ppr t1 <+> ppr t2
  where
    ppr t@(TmVar _) = pprFullSimple ctx t
    ppr t@TmTrue    = pprFullSimple ctx t
    ppr t@TmFalse   = pprFullSimple ctx t
    ppr t           = parens (pprFullSimple ctx t)
pprFullSimple _ TmTrue  = pretty "true"
pprFullSimple _ TmFalse = pretty "false"
pprFullSimple ctx (TmIf t1 t2 t3) = pretty "if" <+> pprFullSimple ctx t1 <+> pretty "then" <+> pprFullSimple ctx t2 <+> pretty "else" <+> pprFullSimple ctx t3
pprFullSimple _ TmZero = pretty "0"
pprFullSimple ctx (TmSucc t) = pretty "succ" <+> pprFullSimple ctx t
pprFullSimple ctx (TmPred t) = pretty "pred" <+> pprFullSimple ctx t
pprFullSimple ctx (TmIsZero t) = pretty "iszero" <+> pprFullSimple ctx t
pprFullSimple ctx (TmSeq t1 t2) = pprFullSimple ctx t1 <> pretty ";" <> pprFullSimple ctx t2
pprFullSimple ctx (TmWildcard ty t) = pretty "λ_:" <> pretty ty <> pretty "." <+> pprFullSimple ctx t
pprFullSimple ctx (TmAscribe t ty) = pprFullSimple ctx t <+> pretty "as" <+> pretty ty <+> pretty ":" <+> pretty ty
pprFullSimple ctx (TmLet var tlet tbody) = pretty "let" <+> pretty var <> pretty "=" <> pprFullSimple ctx tlet <+> pretty "in" <+> pprFullSimple ctx' tbody
  where ctx' = addContext (var, undefined) ctx
pprFullSimple ctx (TmPair t1 t2) = pretty "{" <> pprFullSimple ctx t1 <> pretty "," <> pprFullSimple ctx t2 <> pretty "}"
pprFullSimple ctx (TmPairFst t) = pprFullSimple ctx t <> pretty ".1"
pprFullSimple ctx (TmPairSnd t) = pprFullSimple ctx t <> pretty ".2"
pprFullSimple ctx (TmTuple ts) = encloseSep lbrace rbrace comma (map (pprFullSimple ctx) ts)
pprFullSimple ctx (TmTupleProj i t) = pprFullSimple ctx t <> pretty "." <> pretty i
