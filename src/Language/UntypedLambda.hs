{-# LANGUAGE OverloadedStrings #-}
module Language.UntypedLambda
  ( module Language.UntypedLambda.Types
  , module Language.UntypedLambda.Parser
  , isClosed
  , reduceNormalOrder
  , reduceCallByName
  , reduceCallByValue
  , eval
  , evalWithTrace
  , evalOneStep
  , trace
  , steps
  , subst
  , size
  -- * 演習 6.1.5
  , removenames
  , restorenames
  -- * 定義 6.2.1
  , shift
  -- * 定義 6.2.4
  , namelessSubst
  , reduceNameless
  ) where

import           Language.UntypedLambda.Parser
import           Language.UntypedLambda.Types
import           Language.Utils

import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import qualified Data.Text                     as T

-- | 指定された評価戦略で項を正規系に評価する
eval :: Strategy -> UntypedLambda -> UntypedLambda
eval s t
  | result == t = t
  | otherwise = eval s result
  where
    result = evalOneStep s t

-- | デバッグ用
trace :: Strategy -> UntypedLambda -> IO ()
trace s t = mapM_ (putStrLn . render) $ reverse $ evalWithTrace s [t] t

-- | 簡約ステップ列を返す
evalWithTrace :: Strategy -> [UntypedLambda] -> UntypedLambda -> [UntypedLambda]
evalWithTrace s acc t
  | result == t = acc
  | otherwise = evalWithTrace s acc' result
  where
    result = evalOneStep s t
    acc'   = result:acc

-- | 簡約ステップ数を返す
steps :: UntypedLambda -> Int
steps = length . evalWithTrace NormalOrder []

-- | 1ステップのみ、指定された評価戦略で評価する
evalOneStep :: Strategy -> UntypedLambda -> UntypedLambda
evalOneStep FullBetaReduction _ = undefined -- TODO
evalOneStep NormalOrder       t = reduceNormalOrder t
evalOneStep CallByName        t = reduceCallByName t
evalOneStep CallByValue       t = reduceCallByValue t

-- | 正規順序戦略
reduceNormalOrder :: UntypedLambda -> UntypedLambda
reduceNormalOrder (TmApp (TmLam x old) new)             = subst x new old
reduceNormalOrder (TmApp t1@(TmApp _ _) t2@(TmApp _ _)) = TmApp (reduceNormalOrder t1) (reduceNormalOrder t2)
reduceNormalOrder (TmApp t1@(TmApp _ _) t2)             = TmApp (reduceNormalOrder t1) t2
reduceNormalOrder (TmApp t1 t2@(TmApp _ _))             = TmApp t1 (reduceNormalOrder t2)
reduceNormalOrder (TmLam v t)                           = TmLam v (reduceNormalOrder t)
reduceNormalOrder t                                     = t

-- | 名前呼び戦略
reduceCallByName :: UntypedLambda -> UntypedLambda
reduceCallByName (TmApp (TmLam x old) new)             = subst x new old
reduceCallByName (TmApp t1@(TmApp _ _) t2@(TmApp _ _)) = TmApp (reduceCallByName t1) (reduceCallByName t2)
reduceCallByName (TmApp t1@(TmApp _ _) t2)             = TmApp (reduceCallByName t1) t2
reduceCallByName (TmApp t1 t2@(TmApp _ _))             = TmApp t1 (reduceCallByName t2)
reduceCallByName t                                     = t

-- | 値呼び戦略
reduceCallByValue :: UntypedLambda -> UntypedLambda
reduceCallByValue (TmApp t@(TmLam x old) new)
  | isValue new = subst x new old
  | otherwise   = TmApp t (reduceCallByValue new)
reduceCallByValue (TmApp t1@(TmApp _ _) t2@(TmApp _ _)) = TmApp (reduceCallByValue t1) (reduceCallByValue t2)
reduceCallByValue (TmApp t1@(TmApp _ _) t2) = TmApp (reduceCallByValue t1) t2
reduceCallByValue (TmApp t1 t2@(TmApp _ _)) = TmApp t1 (reduceCallByValue t2)
reduceCallByValue t = t

-- | β-reduction
--
-- 定義5.3.5 (P.54)
subst :: Text -> UntypedLambda -> UntypedLambda -> UntypedLambda
subst v1 after t@(TmVar v2)
  | v1 == v2  = after
  | otherwise = t
subst v1 after t@(TmLam v2 t')
  | v1 /= v2 && v2 `notIn` after = TmLam v2 (subst v1 after t')
  | otherwise = t -- TODO
  where
    notIn v term = v `Set.notMember` freeVars Set.empty term
subst v after (TmApp t1 t2) = (TmApp `on` subst v after) t1 t2

-- | 与えられた項が閉じているかどうか判定する述語
--
-- 項が閉じている = 自由変数が無い
--
-- 閉じた項はコンビネータとも呼ばれる。
isClosed :: UntypedLambda -> Bool
isClosed = Set.null . freeVars Set.empty

-- | 項に含まれる自由変数を返す
--
-- 定義5.3.2 (p.52)
freeVars :: Set VarName -> UntypedLambda -> Set VarName
freeVars fv (TmVar v)
  | Set.member v fv = Set.empty
  | otherwise = Set.singleton v
freeVars fv (TmLam v t) = freeVars fv t `Set.difference` Set.singleton v
freeVars fv (TmApp t1 t2) = (Set.union `on` freeVars fv) t1 t2

-- | 与えられた項が値かどうか判定する述語
isValue :: UntypedLambda -> Bool
isValue (TmVar _)   = True
isValue (TmLam _ _) = True
isValue _           = False

-- | 項のサイズを計算する
--
-- 演習5.3.3 (P.52)
size :: UntypedLambda -> Int
size (TmVar _)     = 1
size (TmLam _ t)   = 1 + size t
size (TmApp t1 t2) = size t1 + size t2

-- | 演習6.1.5 (P.59)
--
-- 仮定: FV(t) ⊆ dom(Γ）)
removenames :: Context -> UntypedLambda -> NamelessTerm
removenames g t
  | freeVars Set.empty t `Set.isSubsetOf` Set.fromList g = removenames' g t
  | otherwise = error "Does not satisfy: FV(t) `isSubsetOf` dom(Γ)"
  where
    removenames' g' (TmVar x)     = NlTmVar $ fromMaybe (error "Can't find variable") $ elemIndex x g'
    removenames' g' (TmLam x t1)  = NlTmLam $ removenames (x:g') t1
    removenames' g' (TmApp t1 t2) = (NlTmApp `on` removenames' g') t1 t2

-- | 演習6.1.5 (P.59)
--
-- 仮定1: Γに含まれる名前は互いに異なる
-- 仮定2: 変数名の集合Vは順序付けられている
restorenames :: Context -> NamelessTerm -> UntypedLambda
restorenames g nt
  | isValid g = restorenames' g nt
  | otherwise = error "Error: duplicate variables in context."
  where
    isValid g' = ((==) `on` (length . nub)) g' g'
    restorenames' g' (NlTmVar k) = TmVar (g' !! k)
    restorenames' g' (NlTmLam t) =
      let x = mkFreshVarName g'
       in TmLam x $ restorenames (x:g') t
    restorenames' g' (NlTmApp t1 t2) = (TmApp `on` restorenames' g') t1 t2

mkFreshVarName :: Context -> VarName
mkFreshVarName [] = "a0"
mkFreshVarName (v:_) =  T.pack $ mconcat ["a", show $ textToInt v + 1]
  where
    textToInt :: Text -> Int
    textToInt = read . T.unpack . T.tail

-- | 定義6.2.1 (P.60)
--
-- c: 打ち切り値
--
-- d: シフト数
shift :: Int -> Int -> NamelessTerm -> NamelessTerm
shift c d (NlTmVar k)
  | k < c = NlTmVar k
  | otherwise = NlTmVar (k + d)
shift c d (NlTmLam t) = NlTmLam $ shift (c+1) d t
shift c d (NlTmApp t1 t2) = (NlTmApp `on` shift c d) t1 t2

-- | 定義6.2.4 (P.60)
namelessSubst :: Int -> NamelessTerm -> NamelessTerm -> NamelessTerm
namelessSubst j s t@(NlTmVar k)
  | k == j = s
  | otherwise = t
namelessSubst j s (NlTmLam t) = NlTmLam $ namelessSubst (j+1) (shift 0 1 s) t
namelessSubst j s (NlTmApp t1 t2) = (NlTmApp `on` namelessSubst j s) t1 t2

-- | 名前無し項のβ簡約 (値呼び)
reduceNameless :: NamelessTerm -> NamelessTerm
reduceNameless (NlTmApp (NlTmLam t12) v2) = shift 0 (-1) $ namelessSubst 0 (shift 0 1 v2) t12
reduceNameless (NlTmApp t1@(NlTmApp _ _) t2@(NlTmApp _ _)) = (NlTmApp `on` reduceNameless) t1 t2
reduceNameless (NlTmApp t1@(NlTmApp _ _) t2) = NlTmApp (reduceNameless t1) t2
reduceNameless (NlTmApp t1 t2@(NlTmApp _ _)) = NlTmApp t1 (reduceNameless t2)
reduceNameless t = t
