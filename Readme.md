# 型システム入門 プログラミング言語と型の理論

- [オーム社の公式ページ](https://www.ohmsha.co.jp/book/9784274069116/)
- [日本語版サポートページ](http://tapl.proofcafe.org/)
- [原著サポートページ](http://www.cis.upenn.edu/~bcpierce/)

## 開発

- [haddock](https://waddlaw.github.io/TAPL/)

### ビルド

```shell
# 通常
$ stack test --fast --file-watch

# haddock
$ stack haddock --fast --file-watch
```

haddock の生成

```shell
$ stack clean && stack haddock --haddock-arguments "--odir docs"
```

ベンチマーク

```shell
#all
$ stack bench --benchmark-arguments "--small"

# gauge
$ stack bench tapl:bench:gauge --benchmark-arguments "--small"

# criterion
$ stack bench tapl:bench:criterion --benchmark-arguments "--output bench.html"
```

### チェック

```shell
$ stack clean && stack test --fast --pedantic
$ hlint .
```

## 新たに使ってみたパッケージ

- [vincenthz/hs-gauge](https://github.com/vincenthz/hs-gauge)