# 演習 22.4.3

以下の制約集合に対する主要単一化子を (存在するならば) 書き下せ。

## 準備

- `充足する` (単一化する) という用語は定義22.3.1で定義される
- $\sigma$ は定義22.1.1によって定義される

## 1. $\{ X = Nat, Y = X \rightarrow X \}$

定義より、以下を満たすような主要単一化子を見つければ良い

- $\sigma X = \sigma Nat$
- $\sigma Y = \sigma (X \rightarrow X)$

この制約に対する代入は以下しかありえない。

- $[ X \mapsto Nat, Y \mapsto Nat \rightarrow Nat ]$

そのため、上記の代入が主要単一化子である。

## 2. $\{ Nat \rightarrow Nat = X \rightarrow Y \}$

- $\sigma (Nat \rightarrow Nat) = \sigma (X \rightarrow Y)$

この制約に対する代入は以下しかありえない。

- $[ X \mapsto Nat, Y \mapsto Nat ]$

## 3. $\{ X \rightarrow Y = Y \rightarrow Z, Z = U \rightarrow W \}$

- $\sigma (X \rightarrow Y) = \sigma(Y \rightarrow Z)$
- $\sigma Z = \sigma (U \rightarrow W)$

まず $[ Z \mapsto U \rightarrow W]$ を得る。すると1つ目の等式は

- $\sigma (X \rightarrow Y) = \sigma(Y \rightarrow U \rightarrow Z)$

となるため、結果として $[ X \mapsto U \rightarrow W,Y \mapsto U \rightarrow W,Z \mapsto U \rightarrow W]$ が主要単一化子となる。

## 4. $\{ Nat = Nat \rightarrow Y \}$

- $\sigma Nat = \sigma (Nat \rightarrow Y)$

存在しない。

## 5. $\{ Y = Nat \rightarrow Y \}$

- $\sigma Y = \sigma (Nat \rightarrow Y)$

存在しない。

## 6. $\{ \}$

$[\ ]$