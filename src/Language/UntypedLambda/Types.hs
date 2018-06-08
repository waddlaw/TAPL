module Language.UntypedLambda.Types
  ( Term (..)
  ) where

import           Data.Text                 (Text)
import           Data.Text.Prettyprint.Doc

data Term
  = TmVar Text
  | TmLam Text Term
  | TmApp Term Term
  deriving (Eq, Show)

instance Pretty Term where
  pretty (TmVar x)     = pretty x
  pretty (TmLam x t)   = pretty "λ" <> pretty x <> pretty "." <+> pretty t
  pretty (TmApp t1 t2) = pretty t1 <+> pretty t2
