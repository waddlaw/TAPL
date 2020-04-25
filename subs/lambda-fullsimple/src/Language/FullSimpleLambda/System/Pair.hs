-- | 図 11.5 二つ組
module Language.FullSimpleLambda.System.Pair
  ( Term (..),
    Ty (..),
    Context (..),
    eval,
    typeof,
  )
where

import Language.FullSimpleLambda.Class
import RIO

data Pair

type Value = Term Pair

instance System Pair where
  data Term Pair
    = -- | 変数
      TmVar Int
    | -- | ラムダ抽象
      TmLam VarName (Ty Pair) (Term Pair)
    | -- | 関数適用
      TmApp (Term Pair) (Term Pair)
    | -- | 2つ組
      TmPair (Term Pair) (Term Pair)
    | -- | 第一要素の射影
      TmPairFst (Term Pair)
    | -- | 第二要素の射影
      TmPairSnd (Term Pair)
    deriving stock (Show, Eq)

  data Ty Pair
    = -- | 関数の型
      TyArr (Ty Pair) (Ty Pair)
    | -- | 直積型
      TyProd (Ty Pair) (Ty Pair)
    deriving stock (Show, Eq)

  data Context Pair
    = -- | 空の文脈
      CtxEmpty
    | -- | 項変数の束縛
      CtxVar (Context Pair) VarName (Ty Pair)
    deriving stock (Show, Eq)

  data Pattern Pair

  eval :: Term Pair -> Term Pair
  eval = \case
    TmApp t1@(TmLam _ _ t12) t2
      -- E-APP1
      | not (isValue t1) -> TmApp (eval t1) t2
      -- E-APP2
      | isValue t1 && not (isValue t2) -> TmApp t1 (eval t2)
      -- E-APPABS
      | isValue t1 && isValue t2 -> shift 0 (-1) $ subst 0 (shift 0 1 t2) t12
    TmPairFst t@(TmPair t1 t2)
      -- E-PAIRBETA1
      | isValue t1 && isValue t2 -> t1
      -- E-PROJ1
      | otherwise -> TmPairFst (eval t)
    TmPairSnd t@(TmPair t1 t2)
      -- E-PAIRBETA2
      | isValue t1 && isValue t2 -> t2
      -- E-PROJ2
      | otherwise -> TmPairSnd (eval t)
    TmPair t1 t2
      -- E-PAIR1
      | not (isValue t1) -> TmPair (eval t1) t2
      -- E-PAIR2
      | isValue t1 -> TmPair t1 (eval t2)
    _ -> error "unexpected term"

  typeof :: Context Pair -> Term Pair -> Ty Pair
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
    -- T-PAIR
    TmPair t1 t2 -> TyProd (typeof ctx t1) (typeof ctx t2)
    -- T-PORJ1
    TmPairFst t -> case typeof ctx t of
      TyProd ty _ -> ty
      _ -> error "type mismatch (T-PORJ1)"
    -- T-PROJ2
    TmPairSnd t -> case typeof ctx t of
      TyProd _ ty -> ty
      _ -> error "type mismatch (T-PROJ2)"

  desugar :: Term Pair -> Term Pair
  desugar = id

isValue :: Term Pair -> Bool
isValue = \case
  TmLam {} -> True -- ラムダ抽象値
  TmPair t1 t2 -> isValue t1 && isValue t2 -- 二つ組値
  _ -> False

subst :: Int -> Value -> Term Pair -> Term Pair
subst j s = \case
  t@(TmVar k)
    | k == j -> s
    | otherwise -> t
  TmLam x ty t -> TmLam x ty $ subst (j + 1) (shift 0 1 s) t
  TmApp t1 t2 -> (TmApp `on` subst j s) t1 t2
  TmPair t1 t2 -> (TmPair `on` subst j s) t1 t2
  TmPairFst t -> TmPairFst $ subst j s t
  TmPairSnd t -> TmPairSnd $ subst j s t

shift :: Int -> Int -> Term Pair -> Term Pair
shift c d = \case
  TmVar k
    | k < c -> TmVar k
    | otherwise -> TmVar (k + d)
  TmLam x ty t -> TmLam x ty $ shift (c + 1) d t
  TmApp t1 t2 -> (TmApp `on` shift c d) t1 t2
  TmPair t1 t2 -> (TmPair `on` shift c d) t1 t2
  TmPairFst t -> TmPairFst $ shift c d t
  TmPairSnd t -> TmPairSnd $ shift c d t

getTypeFromContext :: Int -> Context Pair -> Maybe (Ty Pair)
getTypeFromContext 0 = \case
  CtxEmpty -> Nothing
  CtxVar _ _ ty -> Just ty
getTypeFromContext i = \case
  CtxEmpty -> Nothing
  CtxVar ctx' _ _ -> getTypeFromContext (i -1) ctx'
