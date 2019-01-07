#!/bin/sh -e

PACKAGE=hpack
VERSION=0.31.1
echo "Downloading $PACKAGE now ..."

RELEASES=$(curl --silent https://github.com/sol/$PACKAGE/releases)
URL=https://github.com/$(echo "$RELEASES" | grep -o '\"[^\"]*_linux\.gz\"' | sed s/\"//g | head -n1)

curl --progress-bar --location -o"$PACKAGE.gz" "$URL"
gzip -d "$PACKAGE.gz"