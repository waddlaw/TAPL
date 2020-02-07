# 定理22.1.2 [型代入の下での型付けの保存]

$\sigma$ が任意の型代入で、かつ $\Gamma \vdash t:T$ ならば $\sigma \Gamma \vdash \sigma t : \sigma T$ である

## 証明

型付け導出に関する単純な帰納法。

### T-VAR

- $t = x$
- $x:T \in \Gamma$

$$
\sigma \Gamma \vdash \sigma x : \sigma T
$$

よって成り立つ。

また $x:T \notin \Gamma$ の場合は空虚に真であるため成り立つ。

### T-ABS

- $t = \lambda x:T_{1} \ldotp t_{2}$
- $T = T_{1} \rightarrow T_{2}$
- $\Gamma, x:T_{1} \vdash t_{2} : T_{2}$

帰納法の仮定より

$$
\sigma (\Gamma, x:T_{1}) \vdash \sigma t_{2} : \sigma T_{2} \\
\sigma \Gamma, x:\sigma T_{1} \vdash \sigma t_{2} : \sigma T_{2}
$$

`T-ABS` より

$$
\sigma \Gamma \vdash \lambda x:\sigma T_{1} \ldotp \sigma t_{2} : \sigma T_{1} \rightarrow \sigma T_{2} \\
\sigma \Gamma \vdash \sigma (\lambda x:T_{1}. t_{2}) : \sigma (T_{1} \rightarrow T_{2})
$$

よって、成り立つ。

### T-APP

- $t = t_{1} \enspace t_{2}$
- $T = T_{12}$
- $\Gamma \vdash t_{1}:T_{11} \rightarrow T_{12}$
- $\Gamma \vdash t_{2}:T_{11}$

帰納法の仮定より

$$
\sigma \Gamma \vdash \sigma t_{1}: \sigma (T_{11} \rightarrow T_{12}) \\
\sigma \Gamma \vdash \sigma t_{1}: \sigma T_{11} \rightarrow \sigma T_{12}
$$

かつ

$$
\sigma \Gamma \vdash \sigma t_{2}:\sigma T_{11}
$$

`T-APP` より

$$
\sigma \Gamma \vdash \sigma t_{1} \enspace \sigma t_{2} : \sigma T_{12} \\
\sigma \Gamma \vdash \sigma (t_{1} \enspace t_{2}) : \sigma T_{12}
$$

よって成り立つ。

### T-TRUE

- $t = true$
- $T = Bool$

$\sigma(true) = true$ のため成り立つ

### T-FALSE

- $t = false$
- $T = Bool$

$\sigma(false) = false$ のため成り立つ

### T-IF

- $t = if \space t_{1} \space then \space t_{2} \space else \space t_{3}$
- $T = T$
- $\Gamma \vdash t_{1} : Bool$
- $\Gamma \vdash t_{2} : T$
- $\Gamma \vdash t_{3} : T$

帰納法の仮定より

- $\sigma \Gamma \vdash \sigma t_{1} : \sigma Bool\\
 = \sigma \Gamma \vdash \sigma t_{1} : Bool$
- $\sigma \Gamma \vdash \sigma t_{2} : \sigma T$
- $\sigma \Gamma \vdash \sigma t_{3} : \sigma T$

`T-IF` より

$$
\sigma \Gamma \vdash if \enspace \sigma t_{1} \enspace then \enspace \sigma t_{2} \enspace else \enspace \sigma t_{3} : \sigma T \\
= \sigma \Gamma \vdash \sigma (if \enspace t_{1} \enspace then \enspace t_{2} \enspace else \enspace t_{3}) : \sigma T
$$

### T-ZERO

- $t = 0$
- $T = Nat$

$\sigma(0) = 0$ より成り立つ。

### T-SUCC

- $t = succ \enspace t_{1}$
- $T = Nat$
- $\Gamma \vdash t_{1} : Nat$

帰納法の仮定より

$$
\sigma \Gamma \vdash \sigma t_{1} : \sigma Nat \\
= \sigma \Gamma \vdash \sigma t_{1} : \sigma Nat
$$

`T-SUCC` より

$$
\sigma \Gamma \vdash succ \enspace \sigma t_{1} : \sigma Nat \\
= \sigma \Gamma \vdash \sigma (succ \enspace t_{1}) : \sigma Nat
$$

よって成り立つ。

### T-PRED

- $t = pred \enspace t_{1}$
- $T = Nat$
- $\Gamma \vdash t_{1} : Nat$

帰納法の仮定より

$$
\sigma \Gamma \vdash \sigma t_{1} : \sigma Nat \\
= \sigma \Gamma \vdash \sigma t_{1} : \sigma Nat
$$

`T-PRED` より

$$
\sigma \Gamma \vdash pred \enspace \sigma t_{1} : \sigma Nat \\
= \sigma \Gamma \vdash \sigma (pred \enspace t_{1}) : \sigma Nat
$$

よって成り立つ。

### T-ISZERO

- $t = iszero \enspace t_{1}$
- $T = Bool$
- $\Gamma \vdash t_{1} : Bool$

帰納法の仮定より

$$
\sigma \Gamma \vdash \sigma t_{1} : \sigma Bool \\
= \sigma \Gamma \vdash \sigma t_{1} : \sigma Bool
$$

`T-ISZERO` より

$$
\sigma \Gamma \vdash iszero \enspace \sigma t_{1} : \sigma Bool \\
= \sigma \Gamma \vdash \sigma (iszero \enspace t_{1}) : \sigma Bool
$$

よって成り立つ。
