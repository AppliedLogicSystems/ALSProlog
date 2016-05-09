#!/bin/bash

set -eux

TMP=$(mktemp -d)

tar xzf als-prolog-linux.tgz -C $TMP
SRC=$TMP/als-prolog

mkdir $TMP/usr
DST=$TMP/usr

mkdir $DST/bin
mkdir $DST/lib
mkdir $DST/include
mkdir -p $DST/share/als-prolog

cp -p $SRC/alspro $DST/bin
cp -p $SRC/alsdev $DST/bin
cp -rp $SRC/alsdir $DST/lib
cp -p $SRC/libalspro.so $DST/lib
cp -p $SRC/libalspro.a $DST/lib
cp -p $SRC/ALS_Prolog_Foreign_SDK/include/alspi.h $DST/include
cp -p $SRC/ALS_Prolog_Foreign_SDK/include/alspi_slib.h $DST/include
cp -p $SRC/als-prolog-manual.pdf $DST/share/als-prolog

VERSION=$(git describe --dirty)

fpm --force -s dir -t deb -C $DST \
--name als-prolog --version ${VERSION:1} --architecture i386 --prefix /usr \
--depends "tk (> 8.5)"

rm -r $TMP
