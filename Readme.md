# 型システム入門 プログラミング言語と型の理論

## 開発

### ビルド

```shell
# 通常
$ stack test --fast --file-watch

# haddock
$ stack haddock --fast --file-watch
```

haddock の生成

```shell
$ stack clean && stack haddock --haddock-arguments "--odir haddock"
```

###チェック

```shell
$ stack clean && stack test --fast --pedantic
$ hlint .
```