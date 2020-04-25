{-# LANGUAGE OverloadedStrings #-}

-- | 図 11.3 型指定
module Language.FullSimpleLambda.System.As
  ( Term (..),
    Ty (..),
    Context (..),
    eval,
    typeof,
  )
where

import Language.FullSimpleLambda.Class
import RIO

data As

type Value = Term As

instance System As where
  data Term As
    = -- | 変数
      TmVar Int
    | -- | ラムダ抽象
      TmLam VarName (Ty As) (Term As)
    | -- | 関数適用
      TmApp (Term As) (Term As)
    | -- | 11.4   型指定
      TmAscribe (Term As) (Ty As)
    deriving stock (Show, Eq)

  data Ty As
    = -- | 関数の型
      TyArr (Ty As) (Ty As)
    | -- | Unit 型
      TyUnit
    deriving stock (Show, Eq)

  data Context As
    = -- | 空の文脈
      CtxEmpty
    | -- | 項変数の束縛
      CtxVar (Context As) VarName (Ty As)
    deriving stock (Show, Eq)

  data Pattern As

  eval :: Term As -> Term As
  eval = \case
    TmApp t1@(TmLam _ _ t12) t2
      -- E-APP1
      | not (isValue t1) -> TmApp (eval t1) t2
      -- E-APP2
      | isValue t1 && not (isValue t2) -> TmApp t1 (eval t2)
      -- E-APPABS
      | isValue t1 && isValue t2 -> shift 0 (-1) $ subst 0 (shift 0 1 t2) t12
    TmAscribe t ty
      -- E-ASCRIBE
      | isValue t -> t
      -- E-ASCRIBE1
      | not (isValue t) -> TmAscribe (eval t) ty
    _ -> error "unexpected term"

  typeof :: Context As -> Term As -> Ty As
  typeof ctx = \case
    -- T-VAR
    TmVar i -> case getTypeFromContext i ctx of
      Nothing -> error "Not found type variable in Context"
      Just ty -> ty
    -- T-ABS
    TmLam x tyT1 t2 -> TyArr tyT1 tyT2
      where
        tyT2 = typeof ctx' t2
        ctx' = CtxVar ctx x tyT1
    -- T-APP
    TmApp t1 t2 ->
      case tyT1 of
        TyArr tyT11 tyT12 ->
          if tyT2 == tyT11
            then tyT12
            else
              error . unlines $
                [ "parameter type mismatch (T-APP): ",
                  "tyT2: " <> show tyT2,
                  "tyT11: " <> show tyT11
                ]
        _ -> error "arrow type expected (T-APP)"
      where
        tyT1 = typeof ctx t1
        tyT2 = typeof ctx t2
    -- T-ASCRIBE
    TmAscribe t ty
      | ty == typeof ctx t -> ty
      | otherwise -> error "ascribe type mismatch error (T-ASCRIBE)"

  desugar :: Term As -> Term As
  desugar (TmAscribe t ty) = TmApp (TmLam "x" ty (TmVar 0)) t -- FIXME x notin FV(t)
  desugar term = term

isValue :: Term As -> Bool
isValue = \case
  TmLam {} -> True -- ラムダ抽象値
  _ -> False

subst :: Int -> Value -> Term As -> Term As
subst j s = \case
  t@(TmVar k)
    | k == j -> s
    | otherwise -> t
  TmLam x ty t -> TmLam x ty $ subst (j + 1) (shift 0 1 s) t
  TmApp t1 t2 -> (TmApp `on` subst j s) t1 t2
  TmAscribe t ty -> TmAscribe (subst j s t) ty

shift :: Int -> Int -> Term As -> Term As
shift c d = \case
  TmVar k
    | k < c -> TmVar k
    | otherwise -> TmVar (k + d)
  TmLam x ty t -> TmLam x ty $ shift (c + 1) d t
  TmApp t1 t2 -> (TmApp `on` shift c d) t1 t2
  TmAscribe t ty -> TmAscribe (shift (c + 1) d t) ty

getTypeFromContext :: Int -> Context As -> Maybe (Ty As)
getTypeFromContext 0 = \case
  CtxEmpty -> Nothing
  CtxVar _ _ ty -> Just ty
getTypeFromContext i = \case
  CtxEmpty -> Nothing
  CtxVar ctx' _ _ -> getTypeFromContext (i -1) ctx'
