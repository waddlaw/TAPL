{-# LANGUAGE OverloadedStrings #-}

-- | 図 11.2 Unit 型 + 逐次実行
module Language.FullSimpleLambda.System.Seq
  ( Term (..),
    Ty (..),
    Context (..),
    eval,
    typeof
    )
where

import Language.FullSimpleLambda.Class
import RIO hiding (Seq)

data Seq

type Value = Term Seq

instance System Seq where

  data Term Seq
    = TmVar Int -- ^ 変数
    | TmLam VarName (Ty Seq) (Term Seq) -- ^ ラムダ抽象
    | TmApp (Term Seq) (Term Seq) -- ^ 関数適用
    | TmUnit -- ^ 定数 unit
    | TmSeq (Term Seq) (Term Seq) -- ^ 逐次実行 (t1;t2)
    deriving (Show, Eq)

  data Ty Seq
    = TyArr (Ty Seq) (Ty Seq) -- ^ 関数の型
    | TyUnit -- ^ Unit 型
    deriving (Show, Eq)

  data Context Seq
    = CtxEmpty -- ^ 空の文脈
    | CtxVar (Context Seq) VarName (Ty Seq) -- ^ 項変数の束縛
    deriving (Show, Eq)

  data Pattern Seq

  eval :: Term Seq -> Term Seq
  eval = \case
    TmApp t1@(TmLam _ _ t12) t2
      -- | E-APP1
      | not (isValue t1) -> TmApp (eval t1) t2
      -- | E-APP2
      | isValue t1 && not (isValue t2) -> TmApp t1 (eval t2)
      -- | E-APPABS
      | isValue t1 && isValue t2 -> shift 0 (-1) $ subst 0 (shift 0 1 t2) t12
    TmSeq t1 t2
      -- | E-SEQ
      | not (isValue t1) -> TmSeq (eval t1) t2
      -- | E-SEQNEXT
      | isValue t1 -> t2
    _ -> error "unexpected term"

  typeof :: Context Seq -> Term Seq -> Ty Seq
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
        _ -> error "arrow type expected (T-APP)"
      where
        tyT1 = typeof ctx t1
        tyT2 = typeof ctx t2
    -- | T-UNIT
    TmUnit -> TyUnit
    -- | T-SEQ
    TmSeq t1 t2
      | typeof ctx t1 == TyUnit -> typeof ctx t2
      | otherwise -> error "type mismatch (T-SEQ)"

  desugar :: Term Seq -> Term Seq
  desugar (TmSeq t1 t2) = TmApp (TmLam "x" TyUnit t2) t1 -- FIXME x notin FV(t2)
  desugar term = term

isValue :: Term Seq -> Bool
isValue = \case
  TmLam {} -> True -- ^ ラムダ抽象値
  TmUnit -> True -- ^ 定数 unit
  _ -> False

subst :: Int -> Value -> Term Seq -> Term Seq
subst j s = \case
  t@(TmVar k)
    | k == j -> s
    | otherwise -> t
  TmLam x ty t -> TmLam x ty $ subst (j + 1) (shift 0 1 s) t
  TmApp t1 t2 -> (TmApp `on` subst j s) t1 t2
  TmUnit -> TmUnit
  TmSeq t1 t2 -> (TmSeq `on` subst j s) t1 t2

shift :: Int -> Int -> Term Seq -> Term Seq
shift c d = \case
  TmVar k
    | k < c -> TmVar k
    | otherwise -> TmVar (k + d)
  TmLam x ty t -> TmLam x ty $ shift (c + 1) d t
  TmApp t1 t2 -> (TmApp `on` shift c d) t1 t2
  TmUnit -> TmUnit
  TmSeq t1 t2 -> (TmSeq `on` shift c d) t1 t2

getTypeFromContext :: Int -> Context Seq -> Maybe (Ty Seq)
getTypeFromContext 0 = \case
  CtxEmpty -> Nothing
  CtxVar _ _ ty -> Just ty
getTypeFromContext i = \case
  CtxEmpty -> Nothing
  CtxVar ctx' _ _ -> getTypeFromContext (i -1) ctx'
