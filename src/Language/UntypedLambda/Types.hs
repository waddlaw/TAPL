module Language.UntypedLambda.Types
  ( Term (..)
  , Strategy (..)
  ) where

import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as T
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

instance IsString Term where
  fromString = TmVar . T.pack

data Strategy
  = FullBetaReduction -- ^ 完全ベータ簡約
  | NormalOrder       -- ^ 正規順序戦略
  | CallByName        -- ^ 名前呼び戦略
  | CallByValue       -- ^ 値呼び戦略
