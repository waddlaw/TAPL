-- | 図 11.2 Unit 型
module Language.FullSimpleLambda.System.Unit
  ( Term (..),
    Ty (..),
    Context (..),
    eval,
    typeof
    )
where

import Language.FullSimpleLambda.Class
import RIO

data Unit

type Value = Term Unit

instance System Unit where

  data Term Unit
    = TmVar Int -- ^ 変数
    | TmLam VarName (Ty Unit) (Term Unit) -- ^ ラムダ抽象
    | TmApp (Term Unit) (Term Unit) -- ^ 関数適用
    | TmUnit -- ^ 定数 unit
    deriving (Show, Eq)

  data Ty Unit
    = TyArr (Ty Unit) (Ty Unit) -- ^ 関数の型
    | TyUnit -- ^ Unit 型
    deriving (Show, Eq)

  data Context Unit
    = CtxEmpty -- ^ 空の文脈
    | CtxVar (Context Unit) VarName (Ty Unit) -- ^ 項変数の束縛
    deriving (Show, Eq)

  data Pattern Unit

  eval :: Term Unit -> Term Unit
  eval = \case
    TmApp t1@(TmLam _ _ t12) t2
      -- | E-APP1
      | not (isValue t1) -> TmApp (eval t1) t2
      -- | E-APP2
      | isValue t1 && not (isValue t2) -> TmApp t1 (eval t2)
      -- | E-APPABS
      | isValue t1 && isValue t2 -> shift 0 (-1) $ subst 0 (shift 0 1 t2) t12
    _ -> error "unexpected term"

  typeof :: Context Unit -> Term Unit -> Ty Unit
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
            error . unlines $
              [ "parameter type mismatch (T-APP): ",
                "tyT2: " <> show tyT2,
                "tyT11: " <> show tyT11
                ]
        _ -> error "arrow type expected (T-APP)"
      where
        tyT1 = typeof ctx t1
        tyT2 = typeof ctx t2
    -- | T-UNIT
    TmUnit -> TyUnit

  desugar :: Term Unit -> Term Unit
  desugar = id

isValue :: Term Unit -> Bool
isValue = \case
  TmLam {} -> True -- ^ ラムダ抽象値
  TmUnit -> True -- ^ 定数 unit
  _ -> False

subst :: Int -> Value -> Term Unit -> Term Unit
subst j s = \case
  t@(TmVar k)
    | k == j -> s
    | otherwise -> t
  TmLam x ty t -> TmLam x ty $ subst (j + 1) (shift 0 1 s) t
  TmApp t1 t2 -> (TmApp `on` subst j s) t1 t2
  TmUnit -> TmUnit

shift :: Int -> Int -> Term Unit -> Term Unit
shift c d = \case
  TmVar k
    | k < c -> TmVar k
    | otherwise -> TmVar (k + d)
  TmLam x ty t -> TmLam x ty $ shift (c + 1) d t
  TmApp t1 t2 -> (TmApp `on` shift c d) t1 t2
  TmUnit -> TmUnit

getTypeFromContext :: Int -> Context Unit -> Maybe (Ty Unit)
getTypeFromContext 0 = \case
  CtxEmpty -> Nothing
  CtxVar _ _ ty -> Just ty
getTypeFromContext i = \case
  CtxEmpty -> Nothing
  CtxVar ctx' _ _ -> getTypeFromContext (i -1) ctx'
