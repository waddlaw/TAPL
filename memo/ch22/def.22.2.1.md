# 定義 22.2.1

$\Gamma$ を文脈とし、$t$項とする。$(\Gamma, t)$ の解とは、$\sigma \Gamma \vdash \sigma t : T$ なる組 $(\sigma, T)$ のこと。

## 例 22.2.2

$\Gamma = f:X, a:Y$ かつ $t = f \space a$ とする。

## (1)

- $\sigma = [X \mapsto Y \rightarrow Nat]$
- $\Gamma = f:X, a:Y$
- $t = f \space a$
- $T = Nat$

$$
\begin{aligned}
\sigma \Gamma &= [X \mapsto Y \rightarrow Nat](f:X, a:Y) & \\
&= f : Y \rightarrow Nat, a:Y & \\
\\
\sigma t &= [X \mapsto Y \rightarrow Nat](f \space a) & \\
&= f \space a \\
\\
\sigma \Gamma &\vdash \sigma t : T \\
f : Y \rightarrow Nat, a:Y &\vdash f \enspace a : Nat
\end{aligned}
$$

## (2)

- $\sigma = [X \mapsto Y \rightarrow Z]$
- $\Gamma = f:X, a:Y$
- $t = f \space a$
- $T = Z$

$$
\begin{aligned}
\sigma \Gamma &= [X \mapsto Y \rightarrow Z](f:X, a:Y) & \\
&= f : Y \rightarrow Z, a:Y & \\
\\
\sigma t &= [X \mapsto Y \rightarrow Z](f \space a) & \\
&= f \space a \\
\\
\sigma \Gamma &\vdash \sigma t : T \\
f : Y \rightarrow Z, a:Y &\vdash f \enspace a : Z
\end{aligned}
$$

## (3)

- $\sigma = [X \mapsto Y \rightarrow Z, Z \mapsto Nat]$
- $\Gamma = f:X, a:Y$
- $t = f \space a$
- $T = Z$

$$
\begin{aligned}
\sigma \Gamma &= [X \mapsto Y \rightarrow Z, Z \mapsto Nat](f:X, a:Y) & \\
&= f : Y \rightarrow Z, a:Y & \\
\\
\sigma t &= [X \mapsto Y \rightarrow Z, Z \mapsto Nat](f \space a) & \\
&= f \space a \\
\\
\sigma \Gamma &\vdash \sigma t : T \\
f : Y \rightarrow Z, a:Y &\vdash f \enspace a : Z
\end{aligned}
$$

## (4)

- $\sigma = [X \mapsto Y \rightarrow Nat \rightarrow Nat]$
- $\Gamma = f:X, a:Y$
- $t = f \space a$
- $T = Nat \rightarrow Nat$

$$
\begin{aligned}
\sigma \Gamma &= [X \mapsto Y \rightarrow Nat \rightarrow Nat](f:X, a:Y) & \\
&= f : Y \rightarrow Nat \rightarrow Nat, a:Y & \\
\\
\sigma t &= [X \mapsto Y \rightarrow Nat \rightarrow Nat](f \space a) & \\
&= f \space a \\
\\
\sigma \Gamma &\vdash \sigma t : T \\
f : Y \rightarrow Nat \rightarrow Nat, a:Y &\vdash f \enspace a : Nat \rightarrow Nat
\end{aligned}
$$

## (5)

- $\sigma = [X \mapsto Nat \rightarrow Nat, Y \mapsto Nat]$
- $\Gamma = f:X, a:Y$
- $t = f \space a$
- $T = Nat$

$$
\begin{aligned}
\sigma \Gamma &= [X \mapsto Nat \rightarrow Nat, Y \mapsto Nat](f:X, a:Y) & \\
&= f : Nat \rightarrow Nat, a:Nat & \\
\\
\sigma t &= [X \mapsto Nat \rightarrow Nat, Y \mapsto Nat](f \space a) & \\
&= f \space a \\
\\
\sigma \Gamma &\vdash \sigma t : T \\
f : Nat \rightarrow Nat, a:Nat &\vdash f \enspace a : Nat
\end{aligned}
$$