# 定義 22.1.1 型代入 (代入)

- 型変数から型への有限写像
- 記号 $\sigma$ を使って表す

```hs
type TypeSubst = TyVar -> Type
```

## 表記

表記 | 意味
-----|-------
$\sigma = [X \mapsto T, Y \mapsto U]$ | $X$ を $T$ に、$Y$ を $U$ に対応付ける代入
$dom(\sigma) = \{X, Y\}$ | $\sigma$ に含まれる組の**左側**に現れる**型変数**の集合
$range(\sigma) = \{T, U\}$ | $\sigma$ に含まれる組の**右側**に現れる**型**の集合

- 代入の定義域と値域に同時に同じ変数が現れうることに注意

```hs
-- [X |-> T, Y |-> U]
sigma = f
  where
    f X = T
    f Y = U

-- dom(sigma)   == Set (X, Y)
-- range(sigma) == Set (T, U)
```

## 具体例

$\sigma = [X \mapsto Bool, Y \mapsto X \rightarrow X]$

- $X$ を $Bool$ に対応付ける
- $Y$ を $X \rightarrow X$ に対応付ける
  - $Y$ を $Bool \rightarrow Bool$ に対応付けるのではないことに注意
  - 正しい: $\sigma(X \rightarrow Y) = Bool \rightarrow (X \rightarrow X)$
  - 間違い: $\sigma(X \rightarrow Y) = Bool \rightarrow (Bool \rightarrow Bool)$

## 代入の適用

$$
\begin{aligned}
\sigma(X) &= \begin{cases}
   \enspace T & \text{$(X \mapsto T) \in \sigma$ の場合} \\
   \enspace X & \text{$X$ が $\sigma$ の定義域にない場合}
\end{cases} \\
\sigma(Nat) &= Nat \\
\sigma(Bool) &= Bool \\
\sigma(T_{1} \rightarrow T_{2}) &= \sigma T_{1} \rightarrow \sigma T_{2}
\end{aligned}
$$

## 変数捕縛

型代入時の変数捕縛を避けるために特別な何かをする必要はない。

項のレベルでは以下のような誤った代入が発生してしまう。

$$
\begin{aligned}
& (\lambda x. \lambda z.x) \enspace z \\
 = \enspace &  [x \mapsto z](\lambda z.x) \\
 = \enspace &  \lambda z.z
\end{aligned}
$$

しかし、型レベルでは発生しない。(型変数を束縛することができないため)

## 型代入の拡張

### 文脈への拡張

$$
\sigma(x_{1}:T_{1},\dotsc,x_{n}:T_{n}) = (x_{1}:\sigma T_{1},\dotsc,x_{n}:\sigma T_{n})
$$

### 項への拡張

$$
\begin{aligned}
&\sigma(x)             &= \enspace & x \\
&\sigma(\lambda x:T.t) &= \enspace & \lambda x:\sigma T.\sigma t\\
&\sigma(t \enspace t)  &= \enspace& \sigma t \enspace \sigma t \\
& \sigma(true) &=\enspace& true \\
& \sigma(false) &=\enspace& false \\
& \sigma(if \space t \space then \space t \space else \space t) &=\enspace& if \enspace \sigma t \enspace then \enspace \sigma t \enspace else \enspace \sigma t \\
& \sigma(0) &=\enspace& 0 \\
& \sigma(succ \enspace t) &=\enspace& succ \enspace \sigma t \\
& \sigma(pred \enspace t) &=\enspace& pred \enspace \sigma t \\
& \sigma(iszero \enspace t) &=\enspace& iszero \enspace \sigma t \\
\end{aligned}
$$

## 重要な性質

型付け判断式の妥当性を保存する。
