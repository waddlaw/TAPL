module Language.SimpleLambda.Types
  ( Ty (..),
    Term (..),
    Context,
    addContext,
    unCtx,
    Binding (..),
    SimpleTypedLambda,
    pprSimple,
  )
where

import Data.Text.Prettyprint.Doc
import RIO
import qualified RIO.List.Partial as List.Partial
import qualified RIO.Text as Text

type SimpleTypedLambda = Term

newtype Context = Context {unCtx :: [(Text, Binding)]}
  deriving (Eq, Show)

instance Semigroup Context where
  ctx1 <> ctx2 = Context (unCtx ctx1 <> unCtx ctx2)

instance Monoid Context where
  mempty = Context []

instance IsString Context where
  fromString v = Context [(Text.pack v, NameBind)]

addContext :: (Text, Binding) -> Context -> Context
addContext v = Context . (v :) . unCtx

data Binding
  = -- | 型無しの変数
    NameBind
  | -- | 型付きの変数
    VarBind Ty
  deriving (Eq, Show)

data Ty
  = -- | 関数型
    TyArr Ty Ty
  | -- | Bool型
    TyBool
  deriving (Eq, Show)

data Term
  = TmVar Int -- FIXME
  | TmLam Text Ty Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  deriving (Eq, Show)

instance Pretty Term where
  pretty = pprSimple mempty

pprSimple :: Context -> Term -> Doc ann
pprSimple ctx (TmVar n) =
  if length ctx' <= n
    then pretty "FV" <> pretty n
    else pretty fv
  where
    ctx' = unCtx ctx
    fv = fst (ctx' List.Partial.!! n)
pprSimple ctx (TmLam x ty t) = pretty "λ" <> pretty x <> pretty ":" <> pretty ty <> pretty "." <+> pprSimple ctx' t
  where
    ctx' = addContext (x, VarBind ty) ctx
pprSimple ctx (TmApp t1 t2) = ppr t1 <+> ppr t2
  where
    ppr t@(TmVar _) = pprSimple ctx t
    ppr t@TmTrue = pprSimple ctx t
    ppr t@TmFalse = pprSimple ctx t
    ppr t = parens (pprSimple ctx t)
pprSimple _ TmTrue = pretty "true"
pprSimple _ TmFalse = pretty "false"
pprSimple ctx (TmIf t1 t2 t3) = pretty "if" <+> pprSimple ctx t1 <+> pretty "then" <+> pprSimple ctx t2 <+> pretty "else" <+> pprSimple ctx t3

instance Pretty Ty where
  pretty TyBool = pretty "Bool"
  pretty (TyArr ty1 ty2) = ppr' ty1 <+> pretty "->" <+> pretty ty2
    where
      ppr' t@TyBool = pretty t
      ppr' t = parens (pretty t)
