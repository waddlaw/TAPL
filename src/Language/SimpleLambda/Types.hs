{-# LANGUAGE NoImplicitPrelude #-}
module Language.SimpleLambda.Types
  ( Ty (..)
  , Term (..)
  , Context
  , addContext
  , unCtx
  , Binding (..)
  , SimpleTypedLambda
  , pprSimple
  ) where

import           RIO
import qualified RIO.Text as Text
import qualified RIO.List.Partial          as L.Partial

import           Data.Text.Prettyprint.Doc

type SimpleTypedLambda = Term
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
  pretty = pprSimple []

pprSimple :: [Text] -> Term -> Doc ann
pprSimple fvs (TmVar n)  = if length fvs <= n
                    then pretty "FV" <> pretty n
                    else pretty (fvs L.Partial.!! n)
pprSimple fvs (TmLam x ty t) = pretty "λ" <> pretty x <> pretty ":" <> pretty ty <> pretty "." <+> pprSimple fvs' t
  where fvs' = x:fvs
pprSimple fvs (TmApp t1 t2)  = ppr t1 <+> ppr t2
  where
    ppr t@(TmVar _) = pprSimple fvs t
    ppr t@TmTrue    = pprSimple fvs t
    ppr t@TmFalse   = pprSimple fvs t
    ppr t           = parens (pprSimple fvs t)
pprSimple _ TmTrue  = pretty "true"
pprSimple _ TmFalse = pretty "false"
pprSimple fvs (TmIf t1 t2 t3) = pretty "if" <+> pprSimple fvs t1 <+> pretty "then" <+> pprSimple fvs t2 <+> pretty "else" <+> pprSimple fvs t3

instance Pretty Ty where
  pretty TyBool          = pretty "Bool"
  pretty (TyArr ty1 ty2) = ppr' ty1 <+> pretty "->" <+> pretty ty2
    where
      ppr' t@TyBool = pretty t
      ppr' t        = parens (pretty t)
