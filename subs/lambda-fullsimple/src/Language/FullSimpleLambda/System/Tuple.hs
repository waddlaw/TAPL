-- | 図 11.6 組
module Language.FullSimpleLambda.System.Tuple
  ( Term (..),
    Ty (..),
    Context (..),
    eval,
    typeof
    )
where

import Language.FullSimpleLambda.Class
import RIO
import qualified RIO.List.Partial as L.Partial

data Tuple

type Value = Term Tuple

instance System Tuple where

  data Term Tuple
    = TmVar Int -- ^ 変数
    | TmLam VarName (Ty Tuple) (Term Tuple) -- ^ ラムダ抽象
    | TmApp (Term Tuple) (Term Tuple) -- ^ 関数適用
    | TmTuple [Term Tuple] -- ^ 組
    | TmTupleProj Int (Term Tuple) -- ^ 射影
    deriving (Show, Eq)

  data Ty Tuple
    = TyArr (Ty Tuple) (Ty Tuple) -- ^ 関数の型
    | TyTuple [Ty Tuple] -- ^ 組の型
    deriving (Show, Eq)

  data Context Tuple
    = CtxEmpty -- ^ 空の文脈
    | CtxVar (Context Tuple) VarName (Ty Tuple) -- ^ 項変数の束縛
    deriving (Show, Eq)

  data Pattern Tuple

  eval :: Term Tuple -> Term Tuple
  eval = \case
    TmApp t1@(TmLam _ _ t12) t2
      -- E-APP1
      | not (isValue t1) -> TmApp (eval t1) t2
      -- E-APP2
      | isValue t1 && not (isValue t2) -> TmApp t1 (eval t2)
      -- E-APPABS
      | isValue t1 && isValue t2 -> shift 0 (-1) $ subst 0 (shift 0 1 t2) t12
    -- E-PROJTUPLE
    TmTupleProj j (TmTuple ts)
      | all isValue ts ->
        if j < length ts
          then ts L.Partial.!! j
          else error "タプルのサイズより大きな値が指定されています"
      | otherwise -> error "eval: 値ではない項が存在します。"
    -- E-PROJ
    TmTupleProj i t -> TmTupleProj i (eval t)
    -- E-TUPLE
    TmTuple ts -> TmTuple (vs ++ [eval t] ++ ts')
      where
        (vs, t, ts') = splitTerm ts
    _ -> error "unexpected term"

  typeof :: Context Tuple -> Term Tuple -> Ty Tuple
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
              error . unlines
                $ [ "parameter type mismatch (T-APP): ",
                    "tyT2: " <> show tyT2,
                    "tyT11: " <> show tyT11
                    ]
        _ -> error "arrow type expected (T-APP)"
      where
        tyT1 = typeof ctx t1
        tyT2 = typeof ctx t2
    -- T-TUPLE
    TmTuple ts -> TyTuple $ map (typeof ctx) ts
    -- T-PROJ
    TmTupleProj j t -> case typeof ctx t of
      TyTuple tys -> tys L.Partial.!! j
      _ -> error "type mismatch (T-PROJ)"

  desugar :: Term Tuple -> Term Tuple
  desugar = id

isValue :: Term Tuple -> Bool
isValue = \case
  TmLam {} -> True -- ラムダ抽象値
  TmTuple ts -> all isValue ts -- 組の値
  _ -> False

subst :: Int -> Value -> Term Tuple -> Term Tuple
subst j s = \case
  t@(TmVar k)
    | k == j -> s
    | otherwise -> t
  TmLam x ty t -> TmLam x ty $ subst (j + 1) (shift 0 1 s) t
  TmApp t1 t2 -> (TmApp `on` subst j s) t1 t2
  TmTuple ts -> TmTuple $ map (subst j s) ts
  TmTupleProj i t -> TmTupleProj i $ subst j s t

shift :: Int -> Int -> Term Tuple -> Term Tuple
shift c d = \case
  TmVar k
    | k < c -> TmVar k
    | otherwise -> TmVar (k + d)
  TmLam x ty t -> TmLam x ty $ shift (c + 1) d t
  TmApp t1 t2 -> (TmApp `on` shift c d) t1 t2
  TmTuple ts -> TmTuple $ map (shift c d) ts
  TmTupleProj i t -> TmTupleProj i $ shift c d t

getTypeFromContext :: Int -> Context Tuple -> Maybe (Ty Tuple)
getTypeFromContext 0 = \case
  CtxEmpty -> Nothing
  CtxVar _ _ ty -> Just ty
getTypeFromContext i = \case
  CtxEmpty -> Nothing
  CtxVar ctx' _ _ -> getTypeFromContext (i -1) ctx'

-- | 少なくとも1つは項である
splitTerm :: [Term Tuple] -> ([Value], Term Tuple, [Term Tuple])
splitTerm [] = error "empty list is not expected"
splitTerm ts = (vs, L.Partial.head ts', L.Partial.tail ts')
  where
    (vs, ts') = span isValue ts
