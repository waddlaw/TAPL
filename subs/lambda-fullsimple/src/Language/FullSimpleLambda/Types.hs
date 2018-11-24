{-# LANGUAGE NoImplicitPrelude #-}
module Language.FullSimpleLambda.Types
  ( Ty (..)
  , Term (..)
  , Context
  , addContext
  , unCtx
  , Binding (..)
  , FullSimpleTypedLambda
  , pprFullSimple
  ) where

import           RIO
import qualified RIO.List.Partial          as L.Partial
import qualified RIO.Text                  as Text

import           Data.Text.Prettyprint.Doc

type FullSimpleTypedLambda = Term
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
  = TyArr Ty Ty  -- ^ 関数型
  | TyBool       -- ^ Bool型
  | TyUnit       -- ^ 11.2 Unit型
  deriving (Eq, Show)

data Term
  = TmVar Int
  | TmLam Text Ty Term
  | TmApp Term Term
  | TmTrue
  | TmFalse
  | TmIf Term Term Term
  | TmUnit                -- ^ 11.2 Unit 型
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

instance Pretty Ty where
  pretty TyBool = pretty "Bool"
  pretty TyUnit = pretty "Unit"
  pretty (TyArr ty1 ty2) = ppr' ty1 <+> pretty "->" <+> pretty ty2
    where
      ppr' t@TyBool = pretty t
      ppr' t        = parens (pretty t)
