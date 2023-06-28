#!/bin/bash

set -eux

TMP=$(mktemp -d)

tar xzf als-prolog-linux.tgz -C $TMP
SRC=$TMP/als-prolog

mkdir $TMP/usr
DST=$TMP/usr

mkdir $DST/bin
mkdir -p $DST/lib/als-prolog
mkdir $DST/include
mkdir -p $DST/share/man/man1
mkdir -p $DST/share/doc/als-prolog

# Binaries symlinks from bin

ln -s ../lib/als-prolog/alspro $DST/bin
ln -s ../lib/als-prolog/alsdev $DST/bin

# Executable files and support in lib

cp -p $SRC/alspro $DST/lib/als-prolog
cp -p $SRC/alspro.pst $DST/lib/als-prolog
cp -p $SRC/alsdev $DST/lib/als-prolog
cp -p $SRC/alsdev.pst $DST/lib/als-prolog
cp -rp $SRC/alsdir $DST/lib/als-prolog

# Headers and libraries

cp -p $SRC/ALS_Prolog_Foreign_SDK/include/alspi.h $DST/include
cp -p $SRC/ALS_Prolog_Foreign_SDK/include/alspi_slib.h $DST/include
cp -p $SRC/libalspro.so $DST/lib
cp -p $SRC/libalspro.a $DST/lib

# Man pages, documentation and examples:

cp -p $SRC/alspro.1 $DST/share/man/man1/

cp -p $SRC/README.txt $DST/share/doc/als-prolog
../format-subst $SRC/LICENSE.txt $DST/share/doc/als-prolog
cp -p $SRC/als-prolog-manual.pdf $DST/share/doc/als-prolog
cp -p $SRC/als-ref-manual.pdf $DST/share/doc/als-prolog
cp -p $SRC/docs $DST/share/doc/als-prolog
cp -rp $SRC/examples $DST/share/doc/als-prolog

VERSION=$(../version)

for FORMAT in deb rpm
do
fpm --force -s dir -t $FORMAT -C $DST \
--name als-prolog --version ${VERSION:1} --architecture i386 --prefix /usr \
--depends "tk >= 8.5"
done

rm -r $TMP
