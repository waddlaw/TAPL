# Types and Programming Languages (型システム入門 プログラミング言語と型の理論)

[![Netlify Status](https://api.netlify.com/api/v1/badges/8c7df8ad-e448-40fd-821e-9338ad72482b/deploy-status)](https://app.netlify.com/sites/tapl/deploys)

- [オーム社の公式ページ](https://www.ohmsha.co.jp/book/9784274069116/)
- [日本語版サポートページ](http://tapl.proofcafe.org/)
  - [正誤表](http://tapl.proofcafe.org/errata)
- [原著サポートページ](http://www.cis.upenn.edu/~bcpierce/)
  - [Errata](http://www.cis.upenn.edu/~bcpierce/tapl/index.html)

## Available Systems

chapter | package name        | parse | eval | typecheck | prettypring | note
--------|---------------------|-------|------|-----------|-------------|------
ch03    | bool                |   ✅  |  ✅  |    N/A    |     ✅     |
ch03    | arith               |   ✅  |  ✅  |    N/A    |     ✅     |
ch05    | lambda-Untyped      |   ✅  |  ✅  |    N/A    |     ✅     |
ch09    | lambda-simple       |   ✅  |  ✅  |     ✅    |     ✅     |
ch11    | lambda-fullsimple   |   ✅  |  ✅  |     ✅    |     ✅     |
ch19    | featherweight-java  |   🚧  |  ✅  |     🚧    |     ✅     |
ch22    | recon               |   🚧  |  ✅  |     🚧    |     ✅     |
ch23    | systemf             |   🚧  |  🚧  |     🚧    |     ✅     |

### components

1. bool
2. arith
3. lambda-untyped
4. lambda-simple
5. lambda-fullsimple
6. featherweight-java
7. recon
8. systemf

Fig      | Name                                             |1 |2 |3 |4 |5 |6 |7 |8 |
---------|--------------------------------------------------|--|--|--|--|--|--|--|--|
3-1   | Booleans (B)                                        |✅|  |  |✅|✅|  |✅|✅|
3-2   | Arithmetic expressions (NB)                         |  |✅|  |  |✅|  |✅|✅|
5-3   | Untyped lambda-calculus (λ)                         |  |  |✅|  |  |  |  |  |
8-1   | Typing rules for booleans (B)                       |  |  |  |✅|✅|  |✅|✅|
8-2   | Typing rules for numbers (NB)                       |  |  |  |  |✅|  |✅|✅|
9-1   | Pure simply typed lambda-calculus (λ->)             |  |  |  |✅|✅|  |✅|✅|
11-1  | Uninterpreted base types                            |  |  |  |  |  |  |  |  |
11-2  | Unit type                                           |  |  |  |  |✅|  |  |  |
11-3  | Ascription                                          |  |  |  |  |✅|  |  |  |
11-4  | Let binding                                         |  |  |  |  |✅|  |  |  |
11-5  | Pairs                                               |  |  |  |  |✅|  |  |  |
11-6  | Tuples                                              |  |  |  |  |✅|  |  |  |
11-7  | Records                                             |  |  |  |  |✅|  |  |  |
11-8  | (Untyped) record patterns                           |  |  |  |  |✅|  |  |  |
11-9  | Sums                                                |  |  |  |  |✅|  |  |  |
11-10 | Sums (with unique typing)                           |  |  |  |  |🚧|  |  |  |
11-11 | Variants                                            |  |  |  |  |🚧|  |  |  |
11-12 | General recursion                                   |  |  |  |  |🚧|  |✅|✅|
11-13 | Lists                                               |  |  |  |  |🚧|  |  |✅|
13-1  | References                                          |  |  |  |  |  |  |  |  |
14-1  | Errors                                              |  |  |  |  |  |  |  |  |
14-2  | Error handling                                      |  |  |  |  |  |  |  |  |
14-3  | Exceptions carrying values                          |  |  |  |  |  |  |  |  |
15-1  | Simply typed lambda-calculus with subtyping (λ<:)   |  |  |  |  |  |  |  |  |
15-2  | Records (same as Figure 11-7)                       |  |  |  |  |  |  |  |  |
15-3  | Records and subtyping                               |  |  |  |  |  |  |  |  |
15-4  | Bottom type                                         |  |  |  |  |  |  |  |  |
15-5  | Variants and subtyping                              |  |  |  |  |  |  |  |  |
16-1  | Subtype relation with records (compact version)     |  |  |  |  |  |  |  |  |
16-2  | Algorithmic subtyping                               |  |  |  |  |  |  |  |  |
16-3  | Algorithmic typing                                  |  |  |  |  |  |  |  |  |
19-1  | Featherweight Java (syntax and subtyping)           |  |  |  |  |  |✅|  |  |
19-2  | Featherweight Java (auxiliary definitions)          |  |  |  |  |  |✅|  |  |
19-3  | Featherweight Java (evaluation)                     |  |  |  |  |  |✅|  |  |
19-4  | Featherweight Java (typing)                         |  |  |  |  |  |🚧|  |  |
20-1  | Iso-recursive types (λμ)                            |  |  |  |  |  |  |  |  |
22-1  | Constraint typing rules                             |  |  |  |  |  |  |✅|  |
22-2  | Unification algorithm                               |  |  |  |  |  |  |✅|  |
23-1  | Polymorphic lambda-calculus (System F)              |  |  |  |  |  |  |  |✅|
24-1  | Existential types                                   |  |  |  |  |  |  |  |  |
26-1  | Bounded quantification (kernel F<:)                 |  |  |  |  |  |  |  |  |
26-2  | "Full" bounded quantification                       |  |  |  |  |  |  |  |  |
26-3  | Bounded existential quantification (kernel variant) |  |  |  |  |  |  |  |  |
28-1  | Exposure Algorithm for F<:                          |  |  |  |  |  |  |  |  |
28-2  | Algorithmic typing for F<:                          |  |  |  |  |  |  |  |  |
28-3  | Algorithmic subtyping for kernel F<:                |  |  |  |  |  |  |  |  |
28-4  | Algorithmic subtyping for full F<:                  |  |  |  |  |  |  |  |  |
28-5  | Join and meet algorithms for kernel F<:             |  |  |  |  |  |  |  |  |
29-1  | Type operators and kinding (λω)                     |  |  |  |  |  |  |  |  |
30-1  | Higher-order polymorphic lambda-calculus (Fω)       |  |  |  |  |  |  |  |  |
30-2  | Higher-order existential types                      |  |  |  |  |  |  |  |  |
30-3  | Parallel reduction on types                         |  |  |  |  |  |  |  |  |
32-1  | Polymorphic update                                  |  |  |  |  |  |  |  |  |

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
