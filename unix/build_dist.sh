#!/bin/sh

set -eu

case `uname -rs` in
    "SunOS 4"*)	ARCH=sunos ;;
    "SunOS 5"*)	ARCH=solaris ;;
    Linux*) 	ARCH=linux ;;
    "HP-UX"*)	ARCH=hpux ;;
    "IRIX"*)	ARCH=irix ;;
    "Darwin"*)	ARCH=darwin ;;
    *) 		echo "Unknown machine type..."; exit 1 ;;
esac

ALS_PROLOG=..
BIN=$ALS_PROLOG/core/unix/$ARCH
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
MANUALNAME=als-prolog-manual.pdf ;
HELP="alshelp" ;
;;
esac

rm -rf "$DISTDIR"
mkdir -p "$DISTDIR"

cp -pr "$BIN/$EXE" "$DISTDIR/$EXET"
if test -f "$BIN/$EXE.pst"
then
    cp -pr "$BIN/$EXE.pst" "$DISTDIR/$EXET.pst"
fi

# Use -L to force dereferences of symbolic links
cp -pRL "$BIN/alsdir" "$DISTDIR"
rm -f "$DISTDIR/alsdir/builtins/blt_shl.pro"
rm -f "$DISTDIR/alsdir/builtins/blt_dvsh.pro"
rm -f "$DISTDIR/alsdir/builtins/ra_basis.pro"
rm -f "$DISTDIR/alsdir/builtins/int_cstr.pro"
cp -pr "$BIN/lib" "$DISTDIR"

mkdir "$DISTDIR/examples"
for E in $EXAMPLE_SET ; do
	cp -pr "$EXAMPLES/$E" "$DISTDIR/examples"
done

cp "$ALS_PROLOG/LICENSE.txt" "$DISTDIR/LICENSE.txt"
cp -p "$MAN/welcome_standard.txt" "$DISTDIR/README.txt"
cp -p $MAN/$MANUAL "$DISTDIR/$MANUALNAME"
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
	cp -pr "$ALS_PROLOG/foreign_sdk/unix/ALS_Prolog_Foreign_SDK" "$DISTDIR"
	cp -pr "$BIN/alspro" "$DISTDIR"
	if test -f "$BIN/alspro.pst"
	then
		cp -pr "$BIN/alspro.pst" "$DISTDIR"
	fi
	cp -pr "$BIN/libalspro.a" "$DISTDIR"
	if test $ARCH = hpux
	then
		cp -pr "$BIN/libalspro.sl" "$DISTDIR"
	else 
        if test $ARCH = darwin
	then
		cp -pr "$BIN/libalspro.dylib" "$DISTDIR"
	else
		cp -pr "$BIN/libalspro.so" "$DISTDIR"
        fi
	fi
fi

tar -C $ARCH -czf $DISTNAME-$ARCH.tgz $DISTNAME




