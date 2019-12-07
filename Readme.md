# 型システム入門 プログラミング言語と型の理論

[![CircleCI](https://circleci.com/gh/waddlaw/TAPL.svg?style=svg&circle-token=8ce7ac6650bb2b4998a484e802ea77f812fd9401)](https://circleci.com/gh/waddlaw/TAPL)
[![Build Status](https://travis-ci.org/waddlaw/TAPL.svg?branch=master)](https://travis-ci.org/waddlaw/TAPL)
[![Netlify Status](https://api.netlify.com/api/v1/badges/8c7df8ad-e448-40fd-821e-9338ad72482b/deploy-status)](https://app.netlify.com/sites/tapl/deploys)

- [オーム社の公式ページ](https://www.ohmsha.co.jp/book/9784274069116/)
- [日本語版サポートページ](http://tapl.proofcafe.org/)
  - [正誤表](http://tapl.proofcafe.org/errata)
- [原著サポートページ](http://www.cis.upenn.edu/~bcpierce/)
  - [Errata](http://www.cis.upenn.edu/~bcpierce/tapl/index.html)

## スクリーンショット

型無し算術式

![型無し算術式](screenshots/untyped-arith.gif)

型無し λ 計算

![型無しλ計算](screenshots/untyped-lambda.gif)

## Quick Start Guide

| app           | versionn |
| ------------- | -------- |
| cabal-install | 3.0.0.0  |
| cabal-fmt     | 0.1.1    |
| haskell-ci    | 0.8      |
| ormolu        | 0.0.2.0  |
| stack         | 2.1.3    |

```shell
$ git clone https://github.com/waddlaw/TAPL.git
$ cd TAPL
$ stack run
```

ghcid

```shell
λ ghcid --allow-eval "--command=ghci XXX.hs"
```

## 開発

- [haddock](https://waddlaw.github.io/TAPL/)

### stack

```shell
# 開発
$ stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -qg -RTS"

# テスト
$ stack test --fast --file-watch

## hoogle 生成
$ stack hoogle --keep-going

## haddock 生成
$ stack clean && stack haddock

## ベンチマーク
# all
$ stack bench --benchmark-arguments "--small"

# gauge
$ stack bench tapl:bench:gauge --benchmark-arguments "--small"

# criterion
$ stack bench tapl:bench:criterion --benchmark-arguments "--output bench.html"
```

### cabal

```shell
# ビルド
$ cabal build

# テスト
$ cabal test all
```

### コマンド

````shell
# pedantic
$ stack clean && stack test --fast --pedantic --file-watch

# hlint
$ hlint .

```shell
# フォーマッター
$ ./style.sh

# travis-ci の設定ファイル生成
λ haskell-ci --config cabal.haskell-ci cabal.project
````

## 実行方法

```shell
$ stack run proofb

$ stack run nb

$ stack run untyped-lambda
```
