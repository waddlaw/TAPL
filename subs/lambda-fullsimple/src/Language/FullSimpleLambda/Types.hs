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
  ) where

import           RIO
import qualified RIO.List.Partial          as L.Partial
import qualified RIO.Text                  as Text

import           Data.Text.Prettyprint.Doc

type FullSimpleTypedLambda = Term

type Value = Term -- ^ Term の部分集合

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
  | TmSeq Term Term       -- ^ 11.3 逐次実行
  | TmWildcard Ty Term    -- ^ 11.3 ワイルドカード
  | TmAscribe Term Ty     -- ^ 11.4 型指定
  | TmLet Text Term Term  -- ^ 11.5 let
  deriving (Eq, Show)

instance Pretty Ty where
  pretty TyBool = pretty "Bool"
  pretty TyUnit = pretty "Unit"
  pretty (TyArr ty1 ty2) = ppr' ty1 <+> pretty "->" <+> pretty ty2
    where
      ppr' t@TyBool = pretty t
      ppr' t        = parens (pretty t)
