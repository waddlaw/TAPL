#!/bin/sh -e

PACKAGE=hlint
echo "Downloading $PACKAGE now ..."

RELEASES=$(curl --silent https://github.com/ndmitchell/$PACKAGE/releases)
URL=https://github.com/$(echo "$RELEASES" | grep -o '\"[^\"]*-x86_64-linux\.tar\.gz\"' | sed s/\"//g | head -n1)
VERSION=$(echo "$URL" | sed -e 's/.*-\([\.0-9]\+\)-x86_64-linux\.tar\.gz/\1/')

curl --progress-bar --location -o"$PACKAGE.tar.gz" "$URL"
tar -xzf "$PACKAGE.tar.gz" -C .
mv "$PACKAGE-$VERSION" "$PACKAGE"