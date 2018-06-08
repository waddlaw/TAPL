# 型システム入門 プログラミング言語と型の理論

- [オーム社の公式ページ](https://www.ohmsha.co.jp/book/9784274069116/)
- [日本語版サポートページ](http://tapl.proofcafe.org/)
- [原著サポートページ](http://www.cis.upenn.edu/~bcpierce/)

## 開発

- [haddock](https://waddlaw.github.io/TAPL/)

### GHC 8.2.2

```shell
$ stack test --fast --file-watch

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

### GHC 8.4.3

```shell
$ stack test --fast --file-watch --stack-yaml stack-nightly.yaml

## haddock 生成
$ stack clean && stack haddock --stack-yaml stack-nightly.yaml
```

### チェック

```shell
$ stack clean && stack test --fast --pedantic --file-watch
$ hlint .
```

## ディレクトリ構成

ディレクトリ名 | 内容
---------------|----------
app   | アプリケーション
bench | ベンチマーク
src   | ライブラリ
test  | テスト
docs  | haddock で生成されたもの
note  | メモ

## 新たに使ってみたパッケージ

- [vincenthz/hs-gauge](https://github.com/vincenthz/hs-gauge)