{-# LANGUAGE NoImplicitPrelude #-}
module Language.FullSimpleLambda.Pretty
  ( prettyFullSimpleText
  , pprFullSimple
  ) where

import           RIO
import qualified RIO.List.Partial                      as L.Partial

import           Language.FullSimpleLambda.TypeCheck
import           Language.FullSimpleLambda.Types

import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text

prettyFullSimpleText :: Context -> FullSimpleTypedLambda -> Text
prettyFullSimpleText ctx = renderStrict . layoutCompact . pprFullSimple ctx

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
pprFullSimple ctx (TmSeq t1 t2) = pprFullSimple ctx t1 <> pretty ";" <> pprFullSimple ctx t2
pprFullSimple ctx (TmWildcard ty t) = pretty "λ_:" <> pretty ty <> pretty "." <+> pprFullSimple ctx t
pprFullSimple ctx (TmAscribe t ty) = pprFullSimple ctx t <+> pretty "as" <+> pretty ty <+> pretty ":" <+> pretty ty
pprFullSimple ctx (TmLet var tlet tbody) = pretty "let" <+> pretty var <> pretty "=" <> pprFullSimple ctx tlet <+> pretty "in" <+> pprFullSimple ctx' tlet
  where ctx' = addContext (var, VarBind (typeof ctx tlet)) ctx
