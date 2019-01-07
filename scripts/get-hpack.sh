#!/bin/sh -e

PACKAGE=hpack
echo "Downloading $PACKAGE now ..."

RELEASES=$(curl --silent https://github.com/sol/$PACKAGE/releases)
URL=https://github.com/$(echo "$RELEASES" | grep -o '\"[^\"]*_linux\.gz\"' | sed s/\"//g | head -n1)
VERSION=$(echo "$URL" | sed -e 's/.*-\([\.0-9]\+\)_linux\.gz/\1/')

curl --progress-bar --location -o"$PACKAGE.gz" "$URL"
gzip -d "$PACKAGE.gz"