#!/bin/bash

style="ormolu -c ormolu.yaml --mode inplace"

# app
for file in `\find app -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# src
for file in `\find src -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# bench
for file in `\find bench -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# arith (src)
for file in `\find subs/arith/src -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# arith (test)
for file in `\find subs/arith/test -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# bool (src)
for file in `\find subs/bool/src -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# bool (test)
for file in `\find subs/bool/test -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# lambda-core (src)
for file in `\find subs/lambda-core/src -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# lambda-fullsimple (src)
for file in `\find subs/lambda-fullsimple/src -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# lambda-fullsimple (test)
for file in `\find subs/lambda-fullsimple/test -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# lambda-simple (src)
for file in `\find subs/lambda-simple/src -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# lambda-simple (test)
for file in `\find subs/lambda-simple/test -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# lambda-untyped (src)
for file in `\find subs/lambda-untyped/src -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# lambda-untyped (test)
for file in `\find subs/lambda-untyped/test -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# term (src)
for file in `\find subs/term/src -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done

# term (test)
for file in `\find subs/term/test -type f -name "*.hs"`; do
    echo ${file}
    ${style} ${file}
done