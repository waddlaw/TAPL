-- | 図 11.4 let 束縛
module Language.FullSimpleLambda.System.Let
  ( Term (..),
    Ty (..),
    Context (..),
    eval,
    typeof,
  )
where

import Language.FullSimpleLambda.Class
import RIO

data Let

type Value = Term Let

instance System Let where
  data Term Let
    = -- | 変数
      TmVar Int
    | -- | ラムダ抽象
      TmLam VarName (Ty Let) (Term Let)
    | -- | 関数適用
      TmApp (Term Let) (Term Let)
    | -- | let 束縛
      TmLet VarName (Term Let) (Term Let)
    deriving stock (Show, Eq)

  data Ty Let
    = -- | 関数の型
      TyArr (Ty Let) (Ty Let)
    deriving stock (Show, Eq)

  data Context Let
    = -- | 空の文脈
      CtxEmpty
    | -- | 項変数の束縛
      CtxVar (Context Let) VarName (Ty Let)
    deriving stock (Show, Eq)

  data Pattern Let

  eval :: Term Let -> Term Let
  eval = \case
    TmApp t1@(TmLam _ _ t12) t2
      -- E-APP1
      | not (isValue t1) -> TmApp (eval t1) t2
      -- E-APP2
      | isValue t1 && not (isValue t2) -> TmApp t1 (eval t2)
      -- E-APPABS
      | isValue t1 && isValue t2 -> shift 0 (-1) $ subst 0 (shift 0 1 t2) t12
    TmLet x t1 t2
      -- E-LETV
      | isValue t1 -> shift 0 (-1) $ subst 0 (shift 0 1 t1) t2
      -- E-LET
      | not (isValue t1) -> TmLet x (eval t1) t2
    _ -> error "unexpected term"

  typeof :: Context Let -> Term Let -> Ty Let
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
      where
        tyT1 = typeof ctx t1
        tyT2 = typeof ctx t2
    -- T-LET
    TmLet x t1 t2 -> typeof ctx' t2
      where
        tyT1 = typeof ctx t1
        ctx' = CtxVar ctx x tyT1

  desugar :: Term Let -> Term Let
  desugar = id

isValue :: Term Let -> Bool
isValue = \case
  TmLam {} -> True -- ラムダ抽象値
  _ -> False

subst :: Int -> Value -> Term Let -> Term Let
subst j s = \case
  t@(TmVar k)
    | k == j -> s
    | otherwise -> t
  TmLam x ty t -> TmLam x ty $ subst (j + 1) (shift 0 1 s) t
  TmApp t1 t2 -> (TmApp `on` subst j s) t1 t2
  TmLet x t1 t2 -> TmLet x (subst j s t1) (subst (j + 1) (shift 0 1 s) t2) -- TODO check

shift :: Int -> Int -> Term Let -> Term Let
shift c d = \case
  TmVar k
    | k < c -> TmVar k
    | otherwise -> TmVar (k + d)
  TmLam x ty t -> TmLam x ty $ shift (c + 1) d t
  TmApp t1 t2 -> (TmApp `on` shift c d) t1 t2
  TmLet x t1 t2 -> TmLet x (shift c d t1) (shift (c + 1) d t2) -- TODO (check)

getTypeFromContext :: Int -> Context Let -> Maybe (Ty Let)
getTypeFromContext 0 = \case
  CtxEmpty -> Nothing
  CtxVar _ _ ty -> Just ty
getTypeFromContext i = \case
  CtxEmpty -> Nothing
  CtxVar ctx' _ _ -> getTypeFromContext (i -1) ctx'
