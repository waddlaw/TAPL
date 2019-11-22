# 演習 22.5.2

空の型付け文脈の下での、$
\lambda x:X \ldotp \lambda y:Y \ldotp \lambda z : Z \ldotp (s \space z) \enspace (y \space z)
$ の主要型は何か？

# 解答

空の型付け文脈であり、また演習22.3.3の結果から $(\Gamma,t,S,C)$ はそれぞれ

変数 | 値
-----|------
$\Gamma$ | $\varnothing$
$t$ | $\lambda x:X \ldotp \lambda y:Y \ldotp \lambda z : Z \ldotp (s \space z) \enspace (y \space z)$
$S$ | $X \rightarrow (Y \rightarrow (Z \rightarrow S))$
$C$ | $\{X=Z\rightarrow T_{1}, Y=Z\rightarrow T_{2}, T_{1}=T_{2}\rightarrow S\}$

となる。

次に $(\Gamma,t,S,C)$ の解 $(\sigma, T)$ を図22-2の単一化アルゴリズムにより計算すれば、それぞれ

変数 | 値
----|------
$\sigma$ | $[T_{1} \mapsto T_{2} \rightarrow S] \circ\ [Y \mapsto Z \rightarrow T_{2}] \circ\ [X \mapsto Z \rightarrow T_{1}]$
$T$ $(=\sigma S)$ | $(Z \rightarrow T_{2} \rightarrow S) \rightarrow (Z \rightarrow T_{2}) \rightarrow Z \rightarrow S$

となる。

$T$ の計算は以下の通り

$$
[T_{1} \mapsto T_{2} \rightarrow S] \circ\ [Y \mapsto Z \rightarrow T_{2}] \circ\ [X \mapsto Z \rightarrow T_{1}]\ (X \rightarrow (Y \rightarrow (Z \rightarrow S))) \\

[T_{1} \mapsto T_{2} \rightarrow S] \circ\ [Y \mapsto Z \rightarrow T_{2}]\ ((Z \rightarrow T_{1}) \rightarrow (Y \rightarrow (Z \rightarrow S))) \\

[T_{1} \mapsto T_{2} \rightarrow S] \ ((Z \rightarrow T_{1}) \rightarrow ((Z \rightarrow T_{2}) \rightarrow (Z \rightarrow S))) \\

((Z \rightarrow (T_{2} \rightarrow S)) \rightarrow ((Z \rightarrow T_{2}) \rightarrow (Z \rightarrow S))) \\

(Z \rightarrow T_{2} \rightarrow S) \rightarrow (Z \rightarrow T_{2}) \rightarrow Z \rightarrow S
$$

定理22.4.5より、計算した $\sigma$ は主要単一化子なので $t$ の主要型は $(Z \rightarrow T_{2} \rightarrow S) \rightarrow (Z \rightarrow T_{2}) \rightarrow Z \rightarrow S$ となる。
