#!/bin/sh

set -eu

case `uname -rs` in
    "SunOS 4"*)	ARCH=sunos   ; SOEXT=so    ;;
    "SunOS 5"*)	ARCH=solaris ; SOEXT=so    ;;
    Linux*) 	ARCH=linux   ; SOEXT=so    ;;
    "HP-UX"*)	ARCH=hpux    ; SOEXT=sl    ;;
    "IRIX"*)	ARCH=irix    ; SOEXT=so    ;;
    *"_NT"*)  ARCH=win32  ; SOEXT=dll   ;;
    "Darwin"*)	ARCH=darwin  ; SOEXT=dylib ;;
    *) 		echo "Unknown machine type..."; exit 1 ;;
esac

ALS_PROLOG=..
BIN=$ALS_PROLOG/core/$ARCH
ALS_BUILD_SUPPORT=/mingw32

LIB=$ALS_PROLOG/core/alsp_src/library
EXAMPLES=$ALS_PROLOG/examples
MAN=$ALS_PROLOG/manual
DOCS=$ALS_PROLOG/docs/_local_site

if test $# -ne 1
then
    echo "Usage: $0 [standard]" 1>&2
    exit 2
fi

EDITION=$1

case $EDITION in
standard)
DISTNAME=als-prolog
DISTDIR=$ARCH/$DISTNAME ;
EXE=alsdev ;
EXET=alsdev ;
EXAMPLE_SET="als pxs more objectpro visual chat80 Prolog1000" ;
MANUAL=als_man.pdf ; # standard manual is missing.
REFMANUAL=ref_man.pdf ; 
MANUALNAME=als-prolog-manual.pdf ;
REFMANUALNAME=als-ref-manual.pdf ;
;;
esac

rm -rf "$DISTDIR"
mkdir -p "$DISTDIR"

cp -pr "$BIN/$EXE" "$DISTDIR/$EXET"
if test -f "$BIN/$EXE.pst"
then
    cp -pr "$BIN/$EXE.pst" "$DISTDIR/$EXET.pst"
elif test -f "$BIN/$EXE.exe.pst"
then
    cp -pr "$BIN/$EXE.exe.pst" "$DISTDIR"
fi

# Use -L to force dereferences of symbolic links
cp -pRL "$BIN/alsdir" "$DISTDIR"
rm -f "$DISTDIR/alsdir/builtins/blt_shl.pro"
rm -f "$DISTDIR/alsdir/builtins/blt_dvsh.pro"
rm -f "$DISTDIR/alsdir/builtins/ra_basis.pro"
rm -f "$DISTDIR/alsdir/builtins/int_cstr.pro"

# MinGW64 Tcl/Tk libs
cp -p "$ALS_BUILD_SUPPORT"/bin/tcl86.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/tk86.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libgcc_s_dw2-1.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/zlib1.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libwinpthread-1.dll "$DISTDIR"

#MinGW64 Curl libs
cp -p "$ALS_BUILD_SUPPORT"/bin/libcurl-4.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libwinpthread-1.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libgcc_s_dw2-1.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libbrotlicommon.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libbrotlidec.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libiconv-2.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libidn2-0.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libintl-8.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libunistring-5.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libcrypto-3.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libidn2-0.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libnghttp2-14.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libpsl-5.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libssh2-1.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libssl-3.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/zlib1.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libzstd.dll "$DISTDIR"

mkdir "$DISTDIR"/lib
cp -pr "$ALS_BUILD_SUPPORT"/lib/tcl8.6 "$DISTDIR"/lib
cp -pr "$ALS_BUILD_SUPPORT"/lib/tk8.6 "$DISTDIR"/lib

mkdir "$DISTDIR/examples"
for E in $EXAMPLE_SET ; do
	cp -pr "$EXAMPLES/$E" "$DISTDIR/examples"
done

../format-subst "$ALS_PROLOG/LICENSE.txt" "$DISTDIR/LICENSE.txt"
cp -p "$MAN/welcome_standard.txt" "$DISTDIR/README.txt"
cp -p $MAN/$MANUAL "$DISTDIR/$MANUALNAME"
cp -p $MAN/$REFMANUAL "$DISTDIR/$REFMANUALNAME"
cp -pr $DOCS "$DISTDIR/docs"

#mkdir "$DISTDIR/alsdir/library"
#cp -p $LIB/*.pro "$DISTDIR/alsdir/library"
#cp -p $LIB/*.alb "$DISTDIR/alsdir/library"

if test $EDITION = standard
then
	cp -pr "$ALS_PROLOG/foreign_sdk/win32/ALS_Prolog_Foreign_SDK" "$DISTDIR"
	cp -pr "$BIN/alspro" "$DISTDIR"
	if test -f "$BIN/alspro.pst"
	then
	    cp -pr "$BIN/alspro.pst" "$DISTDIR"
	elif test -f "$BIN/alspro.exe.pst"
	then
	    cp -pr "$BIN/alspro.exe.pst" "$DISTDIR"

	fi
	cp -pr "$BIN/libalspro.a" "$DISTDIR"
	cp -pr "$BIN/libalspro.$SOEXT" "$DISTDIR"
fi

rm -f $DISTNAME-$ARCH.zip
pushd $ARCH
zip --quiet --recurse-paths ../$DISTNAME-$ARCH.zip $DISTNAME
popd
