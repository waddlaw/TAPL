{-# LANGUAGE LambdaCase #-}
module Language.FJ () where

import Data.Maybe

-- | FJ のプログラム
type Program = (CT, Term)

-- | Class 表
type CT = Class -> ClassDef

-- | クラス宣言
data ClassDef = CL
  Class             -- ^ 自身のクラス名
  Class             -- ^ スーパークラス名
  [(Class, Field)]  -- ^ フィールド宣言
  ConstDef          -- ^ コンストラクタ宣言
  [MethodDef]       -- ^ メソッド宣言
  deriving (Eq, Show)

-- | コンストラクタ宣言
data ConstDef = K
  Class -- ^ コンストラクタ名
  [(Class, Field)] -- ^ インスタンスのフィールドの初期化に利用するフィールド。前半部分にスーパークラスのためのフィールドが含まれる
  deriving (Eq, Show)

-- | メソッド宣言
data MethodDef = M
  Class          -- ^ 戻り値の型 (クラス) の名前
  Method         -- ^ メソッド名
  [(Class, Var)] -- ^ メソッドの引数 (引数の型と引数の変数名)
  Term           -- ^ メソッドの本体
  deriving (Eq, Show)

-- | 項
data Term
  = TmVar Var                      -- ^ 変数
  | TmFieldRef Term Field          -- ^ フィールドアクセス
  | TmMethodInv Term Method [Term] -- ^ メソッド呼び出し
  | TmNew Class [Term]             -- ^ オブジェクト生成
  | TmCast Class Term              -- ^ キャスト
  deriving (Eq, Show)

-- | ただの String でも良いけど、間違えそうなので newtype にした
newtype Class  = CN String  deriving (Eq, Show)
newtype Method = MN String  deriving (Eq, Show)
newtype Field  = FN String  deriving (Eq, Show)
newtype Var    = VN String  deriving (Eq, Show)

isValue :: Term -> Bool
isValue = \case
  TmNew _ ts -> all isValue ts
  _          -> False

-- ============================
-- = 図 19-2. 補助的な定義
-- ============================

{- |
>>> fields exCT (CN "Pair")
[(CN "Object",FN "fst"),(CN "Object",FN "snd")]
-}
fields :: CT -> Class -> [(Class, Field)]
fields ct = \case
  CN "Object" -> []
  c -> let CL _ d cfs _ _ = ct c
       in  cfs ++ fields ct d

{- |
>>> mtype exCT (MN "setfst") (CN "Pair")
([CN "Object"],CN "Pair")
-}
mtype :: CT -> Method -> Class -> Maybe ([Class], Class)
mtype ct m = fmap f . mhelper ct m
  where
    f (M rt _ args _) = (map fst args, rt)

{- |
>>> mbody exCT (MN "setfst") (CN "Pair")
([VN "newfst"],TmNew (CN "Pair") [TmVar (VN "newfst"),TmFieldRef (TmVar (VN "this")) (FN "snd")])
-}
mbody :: CT -> Method -> Class -> Maybe ([Var], Term)
mbody ct m = fmap f . mhelper ct m
  where
    f (M _ _ args term) = (map snd args, term)

mhelper :: CT -> Method -> Class -> Maybe MethodDef
mhelper ct m = \case
  CN "Object" -> Nothing
  c -> let CL _ d cfs _ ms = ct c
       in  maybe (mhelper ct m d) pure $ findMethodDef m ms

eqMethodDef :: Method -> MethodDef -> Bool
eqMethodDef m1 (M _ m2 _ _) = m1 == m2

findMethodDef :: Method -> [MethodDef] -> Maybe MethodDef
findMethodDef m ms
    | null md   = Nothing
    | otherwise = Just (head md)
  where
    md = filter (eqMethodDef m) ms

override :: CT -> Method -> Class -> ([Class], Class) -> Bool
override ct m d (cs, c0) = case mtype ct m d of
  Nothing -> True
  Just (ds, d0) -> cs == ds && c0 == d0

-- ============================
-- = 図 19-3. 評価
-- ============================

{- |
>>> uncurry eval example 
TmNew (CN "Pair") [TmNew (CN "B") [],TmFieldRef (TmNew (CN "Pair") [TmNew (CN "A") [],TmNew (CN "B") []]) (FN "snd")]
-}
eval :: CT -> Term -> Term
eval ct t
  | isValue t = t
  | otherwise = eval' ct t

eval' :: CT -> Term -> Term
eval' ct = \case
  TmFieldRef t fi ->
    if isValue t
    then let TmNew c vs = t
          in fst . head . dropWhile ((/=fi) . snd . snd) $ zip vs $ fields ct c -- E-PROJNEW
    else TmFieldRef (eval ct t) fi  -- E-FIELD
  TmMethodInv t m ts ->
    if isValue t
    then if all isValue ts
          then let this@(TmNew c vs) = t
                   (xs, t0) = fromMaybe (error $ "E-INVKNEW: " <> show m <> ", " <> show c) $ mbody ct m c
               in subst ((VN "this", this):zip xs ts) t0 -- E-INVKNEW
          else let (vs, ti:ts) = span isValue ts
               in TmMethodInv t m (vs ++ (eval ct ti:ts))  -- E-INVK-ARG
    else TmMethodInv (eval ct t) m ts  -- E-INVK-RECV
  TmNew c ts ->
    let (vs, ti:ts) = span isValue ts
    in TmNew c (vs ++ (eval ct ti:ts))  -- E-NEW-ARG
  TmCast d@c t ->
    if isValue t
    then let TmNew c vs = t  -- E-CASTNEW (TODO)
         in if checkCast ct c d then t else error "E-CASTNEW"
    else TmCast c (eval ct t) -- E-CAST

subst :: [(Var, Term)] -> Term -> Term
subst fs = \case
  TmVar x            -> fromMaybe (error "subst") $ lookup x fs
  TmFieldRef t field -> TmFieldRef (subst fs t) field
  TmMethodInv t m ts -> TmMethodInv (subst fs t) m (map (subst fs) ts)
  TmNew c ts         -> TmNew c (map (subst fs) ts)
  TmCast c t         -> TmCast c (subst fs t)

checkCast :: CT -> Class -> Class -> Bool
checkCast ct from to = from == to || checkCast ct d to
  where
    CL _ d _ _ _ = ct from

-- ============================
-- = サンプルプログラム
-- ============================

example :: Program
example = (exCT, mainMethod)
  where
    mainMethod = TmMethodInv p (MN "setfst") [b]
    p = TmNew (CN "Pair") [a, b]
    a = TmNew (CN "A") []
    b = TmNew (CN "B") []

example2 :: Program
example2 = (exCT, mainMethod)
  where
    mainMethod = TmFieldRef cast (FN "snd")
    cast = TmCast (CN "Pair") m
    m = TmFieldRef p1 (FN "fst")
    p1 = TmNew (CN "Pair") [p2, a]
    p2 = TmNew (CN "Pair") [a, b]
    a = TmNew (CN "A") []
    b = TmNew (CN "B") [] 

exCT :: CT
exCT (CN name) = case name of
  "A"    -> CL (CN "A") (CN "Object") [] (K (CN "A") []) []
  "B"    -> CL (CN "B") (CN "Object") [] (K (CN "B") []) []
  "Pair" -> CL (CN "Pair") (CN "Object")
               [ (CN "Object", FN "fst") , (CN "Object", FN "snd") ]
               pairConstr
               [pairMethod]
  _ -> error ("Can't find Class " ++ name ++ " in Class Tables.")
  where
    pairConstr = K (CN "Pair") [ (CN "Object", FN "fst"), (CN "Object", FN "snd") ]
    pairMethod = M (CN "Pair") (MN "setfst") [(CN "Object", VN "newfst")] body
    body       = TmNew (CN "Pair") [TmVar (VN "newfst"), TmFieldRef (TmVar (VN "this")) (FN "snd")]

-- 計算規則
{-
- フィールド参照
- メソッド呼び出し
- キャスト

オブジェクトはまず new 項に簡約されることを仮定 (ラムダ計算ではラムダ抽象に簡約される)

E-PROJNEW の動作例
   new Pair(new A(), new B()).snd
-> new B()

E-INVKNEW の動作例
   new Pair(new A(), new B()).setfst(new B())
-> [ newfst |-> new B()
   , this   |-> new Pair(new A(), new B())
   ]
   new Pair(newfst, this.snd)
-> new Pair(new B(), new Pair(new A(), new B()).snd)

E-CASTNEW の動作例
   (Pair)new Pair(new A(), new B())
-> new Pair(new A(), new B())

行き詰まり状態になる可能性がある計算
  (1) クラスに宣言されていないフィールドを参照する
  (2) クラスに宣言されていないメソッドを呼び出す
  (3) オブジェクトの実行時のクラスのスーパークラス以外の何かにキャストする

(1), (2) については、正しく型付けされたプログラムでは絶対に起き得ないことが保証される。
ダウンキャストが起きなければ (3) も保証できる。

   ((Pair)new Pair(new Pair(new A(), new B()),new A()).fst).snd
-> ((Pair)new Pair(new A(), new B())).snd
-> new Pair(new A(), new B()).snd
-> new B()
-}

-- MEMO
{-
明示的に書くもの
  ・スーパークラス
  ・コンストラクタ
  ・フィールド参照やメソッド呼び出しのレシーバ

   new Pair(new A(), new B()).setfst(new B())
-> new Pair(new B(), new B())

メタ変数の取り扱い

- A, B, C, D, E: クラス名
- f, g: フィールド名
- m: メソッド名
- x：仮引数名
- s, t: 項
- u, v: 値
- CL: クラス定義
- K: コンストラクタ宣言
- M: メソッド宣言
- this: 特殊変数。変数の集合に含まれるが、メソッドの引数の名前として用いられることはないもの。すべてのメソッド宣言で暗に束縛されているもの


-}