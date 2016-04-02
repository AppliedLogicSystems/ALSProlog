#!/bin/sh

set -eu

case `uname -rs` in
    "SunOS 4"*)	ARCH=sunos   ; SOEXT=so    ;;
    "SunOS 5"*)	ARCH=solaris ; SOEXT=so    ;;
    Linux*) 	ARCH=linux   ; SOEXT=so    ;;
    "HP-UX"*)	ARCH=hpux    ; SOEXT=sl    ;;
    "IRIX"*)	ARCH=irix    ; SOEXT=so    ;;
    "CYGWIN"*)  ARCH=win32  ; SOEXT=dll   ;;
    "Darwin"*)	ARCH=darwin  ; SOEXT=dylib ;;
    *) 		echo "Unknown machine type..."; exit 1 ;;
esac

ALS_PROLOG=..
BIN=$ALS_PROLOG/core/$ARCH
ALS_BUILD_SUPPORT=/usr/i686-w64-mingw32/sys-root/mingw/

LIB=$ALS_PROLOG/core/alsp_src/library
EXAMPLES=$ALS_PROLOG/examples
MAN=$ALS_PROLOG/manual

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
HELP="alshelp" ;
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
cp -pr "$BIN/lib" "$DISTDIR"

# MinGW64 libs
cp -p "$ALS_BUILD_SUPPORT"/bin/tcl85.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/tk85.dll "$DISTDIR"
cp -p "$ALS_BUILD_SUPPORT"/bin/libgcc_s_sjlj-1.dll "$DISTDIR"
cp -pr "$ALS_BUILD_SUPPORT"/lib/tcl8.5 "$DISTDIR"/lib
cp -pr "$ALS_BUILD_SUPPORT"/lib/tk8.5 "$DISTDIR"/lib

mkdir "$DISTDIR/examples"
for E in $EXAMPLE_SET ; do
	cp -pr "$EXAMPLES/$E" "$DISTDIR/examples"
done

cp "$ALS_PROLOG/LICENSE.txt" "$DISTDIR/LICENSE.txt"
cp -p "$MAN/welcome_standard.txt" "$DISTDIR/README.txt"
cp -p $MAN/$MANUAL "$DISTDIR/$MANUALNAME"
cp -p $MAN/$REFMANUAL "$DISTDIR/$REFMANUALNAME"
mkdir "$DISTDIR/alshelp"
cp -pr $MAN/$HELP/* "$DISTDIR/alshelp"
cp -p $MAN/als_help.html "$DISTDIR/als_help.html"
cp -p $MAN/alshelp.css "$DISTDIR/alshelp.css"
cp -p $MAN/package_nav.html "$DISTDIR/package_nav.html"


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
zip -r ../$DISTNAME-$ARCH.zip $DISTNAME
popd
