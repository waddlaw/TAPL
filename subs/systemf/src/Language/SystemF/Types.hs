module Language.SystemF.Types
  ( Ty (..),
    Term (..),
    Context,
    addContext,
    toContext,
    unCtx,
    Binding (..),
    SystemF,
    pprSystemF,
    VarName (..)
    )
where

import Data.Text.Prettyprint.Doc
import RIO
import qualified RIO.List.Partial as L.Partial

type SystemF = Term

newtype Context = Context {unCtx :: [Binding]}
  deriving stock (Eq, Show)
  deriving (Semigroup, Monoid) via [Binding]

addContext :: Binding -> Context -> Context
addContext v = Context . (v:) . unCtx

toContext :: Binding -> Context
toContext = Context . pure

data Binding
  = TermVarBind VarName Ty -- ^ 9-1. 項変数の束縛
  | TypeVarBind TyVarName -- ^ 23-1. 型変数の束縛
  deriving stock (Eq, Show)

instance Pretty Binding where
  pretty = \case
    TermVarBind varName _ -> pretty varName
    TypeVarBind tyVarName -> pretty tyVarName

data Ty
  = TyBool -- ^ 8-1. Bool型
  | TyNat -- ^ 8-2. 自然数型
  | TyArr Ty Ty -- ^ 9-1. 関数型
  | TyVar TyVarName       -- ^ 23-1 型変数
  | TyForAll TyVarName Ty -- ^ 23-1. 全称型
  deriving stock (Eq, Show)

instance Pretty Ty where
  pretty = \case
    TyBool -> pretty "Bool"
    TyNat -> pretty "Nat"
    TyArr ty1 ty2 -> wrapPpr ty1 <> pretty "->" <> wrapPpr ty2
    TyVar tyVarName -> pretty tyVarName
    TyForAll tyVarName ty -> pretty "∀" <> pretty tyVarName <> pretty "." <> pretty ty
    where
      wrapPpr a
        | isAtom a = pretty a
        | otherwise = parens (pretty a)

newtype VarName = VarName Text
  deriving stock (Eq, Show)
  deriving (Pretty, IsString) via Text

newtype TyVarName = TyVarName Text
  deriving stock (Eq, Show)
  deriving (Pretty, IsString) via Text

data Term
  = TmTrue              -- ^ 3-1. 定数真
  | TmFalse             -- ^ 3-1. 定数偽
  | TmIf Term Term Term -- ^ 3-1. 条件式
  | TmZero              -- ^ 3-2. 定数ゼロ
  | TmSucc Term         -- ^ 3-2. 後者値
  | TmPred Term         -- ^ 3-2. 前者値
  | TmIsZero Term       -- ^ 3-2. ゼロ判定
  | TmVar VarName Int   -- ^ 9-1. 変数
  | TmLam VarName Ty Term  -- ^ 9-1. ラムダ抽象
  | TmApp Term Term     -- ^ 9-1. 関数適用
  | TmTypeLam TyVarName Term -- ^ 23-1. 型抽象
  | TmTypeApp Term Ty   -- ^ 23-1. 型適用
  deriving stock (Eq, Show)

instance Pretty Term where
  pretty = pprSystemF mempty

pprSystemF :: Context -> Term -> Doc ann
pprSystemF ctx = \case
  TmVar _ n ->
    let ctx' = unCtx ctx
        fv = ctx' L.Partial.!! n
    in if length ctx' <= n then pretty "FV" <> pretty n else pretty fv
  TmLam x ty t ->
    let ctx' = addContext (TermVarBind x ty) ctx
    in pretty "λ" <> pretty x <> pretty ":" <> pretty ty <> pretty "." <+> pprSystemF ctx' t
  TmApp t1 t2 -> wrapPpr t1 <+> wrapPpr t2
  TmTypeLam tyVarName t ->
    let ctx' = addContext (TypeVarBind tyVarName) ctx
    in pretty "λ" <> pretty tyVarName <> pretty "." <+> pprSystemF ctx' t
  TmTypeApp t ty -> wrapPpr t <+> brackets (pretty ty)
  TmZero -> pretty "0"
  TmSucc t -> pretty "succ" <+> parens (pprSystemF ctx t)
  TmPred t -> pretty "pred" <+> parens (pprSystemF ctx t)
  TmIsZero t -> pretty "iszero" <+> parens (pprSystemF ctx t)
  TmTrue -> pretty "true"
  TmFalse -> pretty "false"
  TmIf t1 t2 t3 ->  pretty "if"   <+> pprSystemF ctx t1
                <+> pretty "then" <+> pprSystemF ctx t2
                <+> pretty "else" <+> pprSystemF ctx t3
  where
    wrapPpr a
      | isAtom a = pprSystemF ctx a
      | otherwise = parens (pprSystemF ctx a)

class IsAtom a where
  isAtom :: a -> Bool

instance IsAtom Ty where
  isAtom = \case
    TyNat -> True
    TyBool -> True
    TyArr {} -> False
    TyVar {} -> True
    TyForAll {} -> False

instance IsAtom Term where
  isAtom = \case
    TmVar {} -> True
    TmLam {} -> False
    TmApp {} -> False
    TmTypeLam {} -> False
    TmTypeApp {} -> False
    TmTrue {} -> True
    TmFalse {} -> True
    TmIf {} -> False
    TmZero -> True
    TmSucc {} -> False
    TmPred {} -> False
    TmIsZero {} -> False