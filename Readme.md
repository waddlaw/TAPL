# 型システム入門 プログラミング言語と型の理論

[![CircleCI](https://circleci.com/gh/waddlaw/TAPL.svg?style=svg&circle-token=8ce7ac6650bb2b4998a484e802ea77f812fd9401)](https://circleci.com/gh/waddlaw/TAPL)
[![Build Status](https://travis-ci.org/waddlaw/TAPL.svg?branch=master)](https://travis-ci.org/waddlaw/TAPL)
[![Netlify Status](https://api.netlify.com/api/v1/badges/8c7df8ad-e448-40fd-821e-9338ad72482b/deploy-status)](https://app.netlify.com/sites/tapl/deploys)

- [オーム社の公式ページ](https://www.ohmsha.co.jp/book/9784274069116/)
- [日本語版サポートページ](http://tapl.proofcafe.org/)
  - [正誤表](http://tapl.proofcafe.org/errata)
- [原著サポートページ](http://www.cis.upenn.edu/~bcpierce/)
  - [Errata](http://www.cis.upenn.edu/~bcpierce/tapl/index.html)

## Available Systems

name (package name) | parse | eval | typecheck | prettypring | note
-----------|----|-----|---|----|----------------------------------
B (bool)   | ✅ | ✅ | _ | ✅ | Figure 3-1: Booleans
NB (arith) | ✅ | ✅ | - | ✅ | FIgure 3-2: Arithmetic expressions

## Screenshot

Arithmetic expressions

![Arithmetic expressions](screenshots/untyped-arith.gif)

Untyped lambda-calculus

![Untyped lambda-calculus](screenshots/untyped-lambda.gif)

## Quick Start Guide

app | versionn
------|-------
cabal-install | 3.0.0.0
cabal-fmt | 0.1.2
haskell-ci | 0.8
stack | 2.1.3

```shell
$ git clone https://github.com/waddlaw/TAPL.git
$ cd TAPL
$ stack run
```

ghcid

```shell
λ ghcid --allow-eval "--command=ghci XXX.hs"
```

## Development

- [haddock](https://waddlaw.github.io/TAPL/)

### stack

```shell
# build
$ stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -qg -RTS"

# test
$ stack test --fast --file-watch

## generate hoogle database
$ stack hoogle --keep-going

## generate haddock document
$ stack clean && stack haddock

## benchmark
# all
$ stack bench --benchmark-arguments "--small"

# gauge
$ stack bench tapl:bench:gauge --benchmark-arguments "--small"

# criterion
$ stack bench tapl:bench:criterion --benchmark-arguments "--output bench.html"
```

### cabal

```shell
# build
$ cabal build

# test
$ cabal test all
```

### other commands

```shell
# pedantic
$ stack clean && stack test --fast --pedantic --file-watch

# hlint
$ hlint .

# generate travis-ci config
λ haskell-ci --config cabal.haskell-ci cabal.project
```

## running

```shell
$ stack run proofb

$ stack run nb

$ stack run untyped-lambda
```
