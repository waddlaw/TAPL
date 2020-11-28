# Types and Programming Languages

![stack](https://github.com/waddlaw/TAPL/workflows/stack/badge.svg)
![cabal](https://github.com/waddlaw/TAPL/workflows/cabal/badge.svg)
![lint](https://github.com/waddlaw/TAPL/workflows/lint/badge.svg)
![format](https://github.com/waddlaw/TAPL/workflows/format/badge.svg)
[![Netlify Status](https://api.netlify.com/api/v1/badges/8c7df8ad-e448-40fd-821e-9338ad72482b/deploy-status)](https://app.netlify.com/sites/tapl/deploys)

## Quick Start Guide

```shell
$ git clone https://github.com/waddlaw/TAPL.git
$ cd TAPL
```

app | versionn
------|-------
cabal-install | 3.2.0.0
stack | 2.5.1
cabal-fmt | 0.1.5.1
ormolu | 0.1.4.1

## Available Systems

id | chapter | package name        | parse | eval | typecheck | prettyprint | run application
---|---------|---------------------|:-----:|:----:|:---------:|:-----------:|---------------
1  | ch03    | bool                |   ✅  |  ✅  |    N/A    |     ✅     | `stack run bool`
2  | ch03    | arith               |   ✅  |  ✅  |    N/A    |     ✅     | `stack run arith`
3  | ch05    | lambda-untyped      |   ✅  |  ✅  |    N/A    |     ✅     | `stack run untyped-lambda`
4  | ch09    | lambda-simple       |   ✅  |  ✅  |     ✅    |     ✅     | `stack run simple-lambda`
5  | ch11    | lambda-fullsimple   |   ✅  |  ✅  |     ✅    |     ✅     | `stack run fullsimple-lambda`
6  | ch19    | featherweight-java  |   ✅  |  ✅  |     🚧    |     ✅     | `stack run fj`
7  | ch22    | recon               |   🚧  |  ✅  |     🚧    |     ✅     | 🚧
8  | ch23    | systemf             |   🚧  |  🚧  |     🚧    |     ✅     | 🚧

### Components

Fig   | System Name                                         |1 |2 |3 |4 |5 |6 |7 |8 |
:----:|-----------------------------------------------------|--|--|--|--|--|--|--|--|
3-1   | Booleans (B)                                        |✅|✅|  |✅|✅|  |✅|✅|
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

## Development

```shell
# build
$ stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -qg -RTS"

# test
$ stack test --fast --file-watch

## generate haddock document
$ stack clean && stack haddock

## benchmark
$ stack bench --benchmark-arguments "--small"
```

### Other commands

```shell
# pedantic
$ stack clean && stack test --fast --pedantic --file-watch --no-run-tests

# hlint
$ make lint

# format
$ make fmt
$ make cabal-fmt
```

## Japanese book information

型システム入門 プログラミング言語と型の理論

- [オーム社の公式ページ](https://www.ohmsha.co.jp/book/9784274069116/)
- [日本語版サポートページ](http://tapl.proofcafe.org/)
  - [正誤表](http://tapl.proofcafe.org/errata)
- [原著サポートページ](http://www.cis.upenn.edu/~bcpierce/)
  - [Errata](http://www.cis.upenn.edu/~bcpierce/tapl/index.html)
- [住井英二郎（監訳者）＆翻訳チーム 「いまこそ学ぶ！ 型システム ～ TAPL日本語出版までの道のり　～」 - YouTube](https://www.youtube.com/watch?v=JELIVTVuFHo)
