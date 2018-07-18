{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Language.UntypedLambda.Types
  ( Term (..)
  , Strategy (..)
  , UntypedLambda
  , (@@)
  , λ
  , Context
  , VarName
  , NamelessTerm (..)
  ) where

import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Prettyprint.Doc

type UntypedLambda = Term Text

type VarName = Text

-- | 教科書とは逆で ["x", "y", "z"] は [0, 1, 2] と左からインデックスを付ける
type Context = [VarName]

data NamelessTerm
  = NlTmVar Int
  | NlTmLam NamelessTerm
  | NlTmApp NamelessTerm NamelessTerm
  deriving (Eq, Show)

instance IsString NamelessTerm where
  fromString = NlTmVar . read

(@@) :: UntypedLambda -> UntypedLambda -> UntypedLambda
t1 @@ t2 = TmApp t1 t2

λ :: Text -> UntypedLambda -> UntypedLambda
λ = TmLam

data Term a
  = TmVar a
  | TmLam VarName (Term a)
  | TmApp (Term a) (Term a)
  deriving (Eq, Show)

instance Pretty UntypedLambda where
  pretty (TmVar x)     = pretty x
  pretty (TmLam x t)   = pretty "λ" <> pretty x <> pretty "." <+> pretty t
  pretty (TmApp t1 t2) = ppr t1 <+> ppr t2
    where
      ppr t@(TmVar _) = pretty t
      ppr t           = parens (pretty t)

instance IsString UntypedLambda where
  fromString = TmVar . T.pack

data Strategy
  = FullBetaReduction -- ^ 完全ベータ簡約
  | NormalOrder       -- ^ 正規順序戦略
  | CallByName        -- ^ 名前呼び戦略
  | CallByValue       -- ^ 値呼び戦略
  deriving (Show, Read, Enum, Bounded)

