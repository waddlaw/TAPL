-- | 図 11.7 レコード
module Language.FullSimpleLambda.System.Record
  ( Term (..),
    Ty (..),
    Context (..),
    eval,
    typeof,
  )
where

import Language.FullSimpleLambda.Class
import RIO
import qualified RIO.List.Partial as L.Partial

data Record

type Value = Term Record

type FieldLabel = Text

instance System Record where
  data Term Record
    = -- | 変数
      TmVar Int
    | -- | ラムダ抽象
      TmLam VarName (Ty Record) (Term Record)
    | -- | 関数適用
      TmApp (Term Record) (Term Record)
    | -- | レコード
      TmRecord [(FieldLabel, Term Record)]
    | -- | 射影
      TmRecordProj FieldLabel (Term Record)
    deriving stock (Show, Eq)

  data Ty Record
    = -- | 関数の型
      TyArr (Ty Record) (Ty Record)
    | -- | レコードの型
      TyRecord [(FieldLabel, Ty Record)]
    deriving stock (Show, Eq)

  data Context Record
    = -- | 空の文脈
      CtxEmpty
    | -- | 項変数の束縛
      CtxVar (Context Record) VarName (Ty Record)
    deriving stock (Show, Eq)

  data Pattern Record

  eval :: Term Record -> Term Record
  eval = \case
    TmApp t1@(TmLam _ _ t12) t2
      -- E-APP1
      | not (isValue t1) -> TmApp (eval t1) t2
      -- E-APP2
      | isValue t1 && not (isValue t2) -> TmApp t1 (eval t2)
      -- E-APPABS
      | isValue t1 && isValue t2 -> shift 0 (-1) $ subst 0 (shift 0 1 t2) t12
    TmRecordProj label t@(TmRecord fields)
      -- E-PROJRCD
      | isValue t -> fromMaybe (error "field label not found (E-PROJRCD)") $ lookup label fields
      -- E-PROJ
      | otherwise -> TmRecordProj label (eval t)
    -- E-RCD
    t@(TmRecord _)
      | isValue t -> t
      | otherwise -> TmRecord (vfs ++ [(label, eval tj)] ++ tfs)
      where
        (vfs, (label, tj), tfs) = splitRecord t
    _ -> error "unexpected term"

  typeof :: Context Record -> Term Record -> Ty Record
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
    -- T-RCD
    TmRecord fields -> TyRecord $ map (second (typeof ctx)) fields
    -- T-PROJ
    TmRecordProj label t -> case typeof ctx t of
      TyRecord fields -> fromMaybe (error "field label not found (T-PROJ)") $ lookup label fields
      _ -> error "type mismatch (T-PROJ)"

  desugar :: Term Record -> Term Record
  desugar = id

isValue :: Term Record -> Bool
isValue = \case
  TmLam {} -> True -- ラムダ抽象値
  TmRecord fs -> all (isValue . snd) fs -- レコードの値
  _ -> False

subst :: Int -> Value -> Term Record -> Term Record
subst j s = \case
  t@(TmVar k)
    | k == j -> s
    | otherwise -> t
  TmLam x ty t -> TmLam x ty $ subst (j + 1) (shift 0 1 s) t
  TmApp t1 t2 -> (TmApp `on` subst j s) t1 t2
  TmRecord rs -> TmRecord $ map (second (subst j s)) rs
  TmRecordProj l t -> TmRecordProj l $ subst j s t

shift :: Int -> Int -> Term Record -> Term Record
shift c d = \case
  TmVar k
    | k < c -> TmVar k
    | otherwise -> TmVar (k + d)
  TmLam x ty t -> TmLam x ty $ shift (c + 1) d t
  TmApp t1 t2 -> (TmApp `on` shift c d) t1 t2
  TmRecord rs -> TmRecord $ map (second (shift c d)) rs
  TmRecordProj l t -> TmRecordProj l $ shift c d t

getTypeFromContext :: Int -> Context Record -> Maybe (Ty Record)
getTypeFromContext 0 = \case
  CtxEmpty -> Nothing
  CtxVar _ _ ty -> Just ty
getTypeFromContext i = \case
  CtxEmpty -> Nothing
  CtxVar ctx' _ _ -> getTypeFromContext (i -1) ctx'

-- | レコードのみ想定
splitRecord :: Term Record -> ([(FieldLabel, Value)], (FieldLabel, Term Record), [(FieldLabel, Term Record)])
splitRecord (TmRecord fs) = (vfs, L.Partial.head tfs, L.Partial.tail tfs)
  where
    (vfs, tfs) = span (isValue . snd) fs
splitRecord _ = error "only record"
