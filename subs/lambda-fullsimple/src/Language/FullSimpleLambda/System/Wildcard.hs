{-# LANGUAGE OverloadedStrings #-}

-- | 演習11.3.2 ワイルドカード
module Language.FullSimpleLambda.System.Wildcard
  ( Term (..),
    Ty (..),
    Context (..),
    eval,
    typeof
    )
where

import Language.FullSimpleLambda.Class
import RIO

data Wildcard

type Value = Term Wildcard

instance System Wildcard where

  data Term Wildcard
    = TmVar Int -- ^ 変数
    | TmLam VarName (Ty Wildcard) (Term Wildcard) -- ^ ラムダ抽象
    | TmApp (Term Wildcard) (Term Wildcard) -- ^ 関数適用
    | TmWildcard (Ty Wildcard) (Term Wildcard) -- ^ 11.3 ワイルドカード
    deriving (Show, Eq)

  data Ty Wildcard
    = TyArr (Ty Wildcard) (Ty Wildcard) -- ^ 関数の型
    deriving (Show, Eq)

  data Context Wildcard
    = CtxEmpty -- ^ 空の文脈
    | CtxVar (Context Wildcard) VarName (Ty Wildcard) -- ^ 項変数の束縛
    deriving (Show, Eq)

  data Pattern Wildcard

  eval :: Term Wildcard -> Term Wildcard
  eval = \case
    TmApp t1@(TmLam _ _ t12) t2
      -- | E-APP1
      | not (isValue t1) -> TmApp (eval t1) t2
      -- | E-APP2
      | isValue t1 && not (isValue t2) -> TmApp t1 (eval t2)
      -- | E-APPABS
      | isValue t1 && isValue t2 -> shift 0 (-1) $ subst 0 (shift 0 1 t2) t12
    -- E-WILDCARD
    TmApp (TmWildcard _ t12) t2
      | isValue t2 -> t12
    _ -> error "unexpected term"

  typeof :: Context Wildcard -> Term Wildcard -> Ty Wildcard
  typeof ctx = \case
    -- | T-VAR
    TmVar i -> case getTypeFromContext i ctx of
      Nothing -> error "Not found type variable in Context"
      Just ty -> ty
    -- | T-ABS
    TmLam x tyT1 t2 -> TyArr tyT1 tyT2
      where
        tyT2 = typeof ctx' t2
        ctx' = CtxVar ctx x tyT1
    -- | T-APP
    TmApp t1 t2 ->
      case tyT1 of
        TyArr tyT11 tyT12 ->
          if tyT2 == tyT11
            then tyT12
            else
              error . unlines
                $ [ "parameter type mismatch (T-APP): ",
                    "tyT2: " <> show tyT2,
                    "tyT11: " <> show tyT11
                    ]
      where
        tyT1 = typeof ctx t1
        tyT2 = typeof ctx t2
    -- T-WILDCARD
    TmWildcard tyT1 t2 -> TyArr tyT1 (typeof ctx t2)

  desugar :: Term Wildcard -> Term Wildcard
  desugar (TmWildcard ty t) = TmLam "x" ty t -- FIXME x notin FV(t)
  desugar term = term

isValue :: Term Wildcard -> Bool
isValue = \case
  TmLam {} -> True -- ^ ラムダ抽象値
  _ -> False

subst :: Int -> Value -> Term Wildcard -> Term Wildcard
subst j s = \case
  t@(TmVar k)
    | k == j -> s
    | otherwise -> t
  TmLam x ty t -> TmLam x ty $ subst (j + 1) (shift 0 1 s) t
  TmApp t1 t2 -> (TmApp `on` subst j s) t1 t2
  TmWildcard ty t -> TmWildcard ty $ subst j s t

shift :: Int -> Int -> Term Wildcard -> Term Wildcard
shift c d = \case
  TmVar k
    | k < c -> TmVar k
    | otherwise -> TmVar (k + d)
  TmLam x ty t -> TmLam x ty $ shift (c + 1) d t
  TmApp t1 t2 -> (TmApp `on` shift c d) t1 t2
  TmWildcard ty t -> TmWildcard ty $ shift (c + 1) d t

getTypeFromContext :: Int -> Context Wildcard -> Maybe (Ty Wildcard)
getTypeFromContext 0 = \case
  CtxEmpty -> Nothing
  CtxVar _ _ ty -> Just ty
getTypeFromContext i = \case
  CtxEmpty -> Nothing
  CtxVar ctx' _ _ -> getTypeFromContext (i -1) ctx'
