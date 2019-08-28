-- | 図 11.8 (型無し) レコードパターン
module Language.FullSimpleLambda.System.RecordPattern
  ( Term (..),
    Ty (..),
    Context (..),
    eval,
    typeof
    )
where

import Data.Monoid
import Language.FullSimpleLambda.Class
import RIO
import qualified RIO.List.Partial as L.Partial

data RecordPattern

type Value = Term RecordPattern

type FieldLabel = Text

instance System RecordPattern where

  data Term RecordPattern
    = TmVar Int -- ^ 変数
    | TmLam VarName (Ty RecordPattern) (Term RecordPattern) -- ^ ラムダ抽象
    | TmApp (Term RecordPattern) (Term RecordPattern) -- ^ 関数適用
    | TmRecord [(FieldLabel, Term RecordPattern)] -- ^ レコード
    | TmRecordProj FieldLabel (Term RecordPattern) -- ^ 射影
    | TmPattern (Pattern RecordPattern) (Term RecordPattern) (Term RecordPattern) -- ^ パターン束縛
    deriving (Show, Eq)

  data Ty RecordPattern
    = TyArr (Ty RecordPattern) (Ty RecordPattern) -- ^ 関数の型
    | TyRecord [(FieldLabel, Ty RecordPattern)] -- ^ レコードの型
    deriving (Show, Eq)

  data Context RecordPattern
    = CtxEmpty -- ^ 空の文脈
    | CtxVar (Context RecordPattern) VarName (Ty RecordPattern) -- ^ 項変数の束縛
    | CtxDelta (Context RecordPattern) (Term RecordPattern) (Ty RecordPattern)
    deriving (Show, Eq)

  data Pattern RecordPattern
    = PtVar VarName Int -- ^ 変数パターン
    | PtRecord [(FieldLabel, Pattern RecordPattern)] -- ^ レコードパターン
    deriving (Show, Eq)

  eval :: Term RecordPattern -> Term RecordPattern
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
    TmPattern p t1 t2
      -- E-LETV
      | isValue t1 -> match p t1 t2
      -- E-LET
      | not (isValue t1) -> TmPattern p (eval t1) t2
    _ -> error "unexpected term"

  typeof :: Context RecordPattern -> Term RecordPattern -> Ty RecordPattern
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
    -- T-RCD
    TmRecord fields -> TyRecord $ map (\(l, t) -> (l, typeof ctx t)) fields
    -- T-PROJ
    TmRecordProj label t -> case typeof ctx t of
      TyRecord fields -> fromMaybe (error "field label not found (T-PROJ)") $ lookup label fields
      _ -> error "type mismatch (T-PROJ)"
    -- T-LET
    TmPattern p t1 t2 -> typeof ctx' t2
      where
        ty1 = typeof ctx t1
        ctx' = ctx <> delta p ty1

  desugar :: Term RecordPattern -> Term RecordPattern
  desugar = id

  match :: Pattern RecordPattern -> Value -> (Term RecordPattern -> Term RecordPattern)
  match (PtVar _ n) v = subst n v
  match p@(PtRecord fs) v@(TmRecord fs')
    | isRecordValue v && sameFieldLength p v =
      appEndo $ foldMap (Endo . uncurry match) $ zip (map snd fs) (map snd fs')
    | otherwise = error "match: pattern match failure"
  match PtRecord {} _ = error "match: v is not Rrcord"

isValue :: Term RecordPattern -> Bool
isValue = \case
  TmLam {} -> True -- ラムダ抽象値
  TmRecord fs -> all (isValue . snd) fs -- レコードの値
  _ -> False

subst :: Int -> Value -> Term RecordPattern -> Term RecordPattern
subst j s = \case
  t@(TmVar k)
    | k == j -> s
    | otherwise -> t
  TmLam x ty t -> TmLam x ty $ subst (j + 1) (shift 0 1 s) t
  TmApp t1 t2 -> (TmApp `on` subst j s) t1 t2
  TmRecord rs -> TmRecord $ map (\(l, t) -> (l, subst j s t)) rs
  TmRecordProj l t -> TmRecordProj l $ subst j s t
  TmPattern p t1 t2 -> TmPattern p (subst j s t1) (subst (j + 1) (shift 0 1 s) t2) -- TODO check (間違っていそう)

shift :: Int -> Int -> Term RecordPattern -> Term RecordPattern
shift c d = \case
  TmVar k
    | k < c -> TmVar k
    | otherwise -> TmVar (k + d)
  TmLam x ty t -> TmLam x ty $ shift (c + 1) d t
  TmApp t1 t2 -> (TmApp `on` shift c d) t1 t2
  TmRecord rs -> TmRecord $ map (\(l, t) -> (l, shift c d t)) rs
  TmRecordProj l t -> TmRecordProj l $ shift c d t
  TmPattern p t1 t2 -> TmPattern p (shift c d t1) (shift (c + 1) d t2) -- TODO (check, 間違ってるかも)

getTypeFromContext :: Int -> Context RecordPattern -> Maybe (Ty RecordPattern)
getTypeFromContext 0 = \case
  CtxEmpty -> Nothing
  CtxVar _ _ ty -> Just ty
getTypeFromContext i = \case
  CtxEmpty -> Nothing
  CtxVar ctx' _ _ -> getTypeFromContext (i -1) ctx'

-- | レコードのみ想定
splitRecord :: Term RecordPattern -> ([(FieldLabel, Value)], (FieldLabel, Term RecordPattern), [(FieldLabel, Term RecordPattern)])
splitRecord (TmRecord fs) = (vfs, L.Partial.head tfs, L.Partial.tail tfs)
  where
    (vfs, tfs) = span (isValue . snd) fs
splitRecord _ = error "only record"

-- | 与えられた項がレコードかつ、値かどうか判定
isRecordValue :: Term RecordPattern -> Bool
isRecordValue t@TmRecord {} = isValue t
isRecordValue _ = False

sameFieldLength :: Pattern RecordPattern -> Value -> Bool
sameFieldLength (PtRecord fs1) (TmRecord fs2) = length fs1 == length fs2
sameFieldLength _ _ = error "unexpected field"

delta :: Pattern RecordPattern -> Ty RecordPattern -> Context RecordPattern
delta (PtVar varName _) = CtxVar CtxEmpty varName
delta (PtRecord pfs) = \case
  TyRecord tfs ->
    foldr (\(p, ty') acc -> CtxVar acc) CtxEmpty
      $ zip (map snd pfs) (map snd tfs)
  _ -> error "delta: expected Record term"
