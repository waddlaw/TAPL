# 型システム入門 プログラミング言語と型の理論

## 開発

### ビルド

```shell
$ stack test --fast --file-watch
```

haddock の生成

```shell
$ stack haddock
```

###チェック

```shell
$ stack clean && stack test --fast --pedantic
$ hlint .
```