# 定理 22.3.5 [制約型付けの健全性]

- 健全性の定義
  - 8.3 で出てきた安全性と同じ。つまり進行+保存を満たす。
- $(\sigma, T)$ が $(\Gamma, t, S, C)$ の解
  - 定義 22.3.4
  - $\sigma$ が $C$ を充足し、かつ $\sigma S = T$ となるような組 $(\sigma, T)$ のこと
- $(\Gamma, t)$ の解
  - 定義 22.2.1
  - $\sigma \Gamma \vdash \sigma t : T$ なる組 $(\sigma, T)$ のこと

## 証明で使う道具

型代入の拡張を以下の通り定義しておく。

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

## 示したいこと

- もし $(\sigma, T)$ が $(\Gamma, t, S, C)$ の解であれば、それは $(\Gamma, t)$ の解でもある

つまり、仮定した $(\Gamma, t, S, C)$ の解によって得られる $(\sigma, \Gamma)$ を使って $\sigma \Gamma \vdash \sigma t : T$ に対して型付け規則が存在することを示せば良い。

$\Gamma \vdash t : S \mid C\ \Rightarrow \ \sigma \Gamma \vdash \sigma t : T$ (ただし $\sigma$ は制約 $C$ を充足する)

## 証明

$\Gamma \vdash t : S \mid C$ の制約型付け導出に関する帰納法による。最後に使われた規則に基づく場合分けを行う。

### CT-VAR

$\Gamma \vdash x : S \mid \{\}$

_ | _
---|-----
文脈 | $x:S \in \Gamma$
項 | $t=x$
項の型 | $S$
制約集合 | $C = \{\}$

仮定より $(\sigma, T)$ は $(\Gamma, x, S, \{\})$ の解である。

$C$ は空であるから $T = \sigma S$ である。

$(\sigma, T)$ が $(\Gamma, x)$ の解でもあることを示すために、$\sigma \Gamma \vdash \sigma x : T$ なる組 $(\sigma, T)$ であることを確認する。

$$
\sigma \Gamma \vdash \sigma x : T \\
\sigma \Gamma \vdash \sigma x : \sigma S \\
\sigma \Gamma \vdash \sigma (x : S)
$$

`T-VAR` より

$$
\sigma (x : S) \in \sigma \Gamma \\
\sigma (x : S) \in \sigma (x:S, \Gamma) \\
\sigma (x : S) \in \sigma (x:S), \sigma \Gamma
$$

よって成り立つ。

### CT-ABS

$\Gamma \vdash \lambda x:T_{1}.\ t_{2}: T_{1} \rightarrow S_{2} \mid C$

_ | _
---|-----
文脈 | $\Gamma$
項 | $t = \lambda x : T_{1}.\ t_{2}$
項の型 | $S = T_{1} \rightarrow S_{2}$
制約集合 | $C$
帰納法の仮定 | $\Gamma,x:T_{1} \vdash t_{2}: S_{2} \mid C$

仮定より $(\sigma, T)$ は $(\Gamma, \lambda x : T_{1}.\ t_{2}, T_{1} \rightarrow S_{2}, C)$ の解である。

よって、$\sigma$ は $C$ を単一化し、かつ $T = \sigma (T_{1} \rightarrow S_{2}) = \sigma T_{1} \rightarrow \sigma T_{2}$ となるような組 $(\sigma, T)$ である。

$(\sigma, T)$ が $(\Gamma, \lambda x : T_{1}.\ t_{2})$ の解でもあることを示すために、$\sigma \Gamma \vdash \sigma (\lambda x : T_{1}.\ t_{2}) : T$ なる組 $(\sigma, T)$ であることを確認する。

$$
\sigma \Gamma \vdash \sigma (\lambda x : T_{1}.\ t_{2}) : T \\
\sigma \Gamma \vdash \lambda x : \sigma T_{1}.\ t_{2} : \sigma T_{1} \rightarrow \sigma S_{2}
$$

`T-ABS` より

$$
\sigma \Gamma, x:\sigma T_{1} \vdash \sigma t_{2} : \sigma S_{2}
$$

$(\sigma, \sigma S_{2})$ は $(\Gamma,x:T_{1}, t_{2}, S_{2}, C)$ の解なので、帰納法の仮定より、同様に $(\Gamma,x:T_{1}, t_{2})$ の解でもある。

よって $\sigma (\Gamma,x:T_{1}) \vdash \sigma t_{2} : T$ となる組 $(\sigma, T)$ のことである。

$$
\sigma (\Gamma,x:T_{1}) \vdash \sigma t_{2} : T \\
\sigma \Gamma, x : \sigma T_{1} \vdash \sigma t_{2} : \sigma S_{2}
$$

よって成り立つ。

### CT-APP

$\Gamma \vdash t_{1}\ t_{2} : X \mid C^{\prime}$

_ | _
---|-----
文脈 | $\Gamma$
項 | $t = t_{1}\ t_{2}$
項の型 | $S = X$
制約集合 | $C^{\prime} = C_{1} \cup C_{2} \cup \{T_{1} = T_{2} \rightarrow X \}$
帰納法の仮定 | $\Gamma \vdash t_{1}: T_{1} \mid C_{1}$ <br> $\Gamma \vdash t_{2}: T_{2} \mid C_{2}$

仮定より $(\sigma, T)$ は $(\Gamma, t_{1}\ t_{2}, X, C_{1} \cup C_{2} \cup \{T_{1} = T_{2} \rightarrow X\})$ の解である。

よって、$\sigma$ は $C_{1}$ と $C_{2}$ を単一化し、かつ $\sigma T_{1} = \sigma (T_{2} \rightarrow X) = \sigma T_{2} \rightarrow \sigma X$, $T = \sigma X$ となるような組 $(\sigma, T)$ である。

$(\sigma, T)$ が $(\Gamma, t_{1}\ t_{2})$ の解でもあることを示すために、$\sigma \Gamma \vdash \sigma (t_{1}\ t_{2}) : T$ なる組 $(\sigma, T)$ であることを確認する。

$$
\sigma \Gamma \vdash \sigma (t_{1}\ t_{2}) : T \\
\sigma \Gamma \vdash \sigma t_{1} \ \sigma t_{2} : \sigma X
$$

$(\sigma, \sigma T_{1})$ と $(\sigma, \sigma T_{2})$ はそれぞれ、$(\Gamma, t_{1}, T_{1}, C_{1})$ と $(\Gamma, t_{2}, T_{2}, C_{2})$ の解である。

よって、帰納法の仮定より、それぞれ $(\sigma \Gamma \vdash \sigma t_{1} : \sigma T_{1})$ と $(\sigma \Gamma \vdash \sigma t_{2} : \sigma T_{2})$ の解でもある。

また $(\sigma \Gamma \vdash \sigma t_{1} : \sigma T_{1})= (\sigma \Gamma \vdash \sigma t_{1} : \sigma T_{2} \rightarrow \sigma X)$ なので

`T-APP` より

$$
\sigma \Gamma \vdash \sigma t_{1} \ \sigma t_{2}: \sigma X
$$

よって成り立つ。

### CT-ZERO

$\Gamma \vdash 0 : Nat \mid \{\}$

_ | _
---|-----
文脈 | $\Gamma$
項 | $0$
項の型 | $S = Nat$
制約集合 | $\{\}$

仮定より $(\sigma, T)$ は $(\Gamma, 0, Nat, \{\})$ の解である。

$C$ は空なので $T = \sigma Nat$ である。

$(\sigma, T)$ が $(\Gamma, 0)$ の解でもあることを示すために、$\sigma \Gamma \vdash \sigma 0 : T$ なる組 $(\sigma, T)$ であることを確認する。

$$
\sigma \Gamma \vdash \sigma 0 : T \\
\sigma \Gamma \vdash \sigma 0 : \sigma Nat \\
\sigma \Gamma \vdash 0 : Nat
$$

`T-ZERO` より、成り立つ

### CT-SUCC

$\Gamma \vdash succ\ t_{1} : Nat \mid C^{\prime}$

_ | _
---|-----
文脈 | $\Gamma$
項 | $succ\ t_{1}$
項の型 | $S = Nat$
制約集合 | $C^{\prime} = C \cup \{ T = Nat \}$
帰納法の仮定 | $\Gamma \vdash t_{1} : T \mid C$

仮定より $(\sigma, T)$ は $(\Gamma, succ\ t_{1}, Nat, C \cup \{ T = Nat \})$ の解である。

そのため、$\sigma$ は $C$ を単一化し、かつ $\sigma T = \sigma Nat$ となるような組 $(\sigma, T)$ である。

また、$(\sigma, T)$ は $(\Gamma, succ \ t_{1})$ の解でもある。

$$
\sigma \Gamma \vdash \sigma (succ t_{1}) : \sigma T
$$

$(\sigma, \sigma T)$ は $(\Gamma, t_{1}, T, C)$ の解である。

帰納法の仮定より、$(\sigma, \sigma T)$ は $(\Gamma,t_{1})$ の解でもあるため

$$
\sigma \Gamma \vdash \sigma t_{1} : \sigma T \\
\sigma \Gamma \vdash \sigma t_{1} : Nat
$$

となる。



### CT-PRED

### CT-ISZERO

### CT-TRUE

### CT-FALSE

### CT-IF

