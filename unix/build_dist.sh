#!/bin/sh

set -eux

case `uname -rs` in
    "SunOS 4"*)	ARCH=sunos   ; SOEXT=so    ;;
    "SunOS 5"*)	ARCH=solaris ; SOEXT=so    ;;
    Linux*) 	ARCH=linux   ; SOEXT=so    ;;
    "HP-UX"*)	ARCH=hpux    ; SOEXT=sl    ;;
    "IRIX"*)	ARCH=irix    ; SOEXT=so    ;;
    "CYGWIN"*)  ARCH=cygwin  ; SOEXT=dll   ;;
    "Darwin"*)	ARCH=darwin  ; SOEXT=dylib ;;
    *) 		echo "Unknown machine type..."; exit 1 ;;
esac

ALS_PROLOG=..
BIN=$ALS_PROLOG/core/unix/$ARCH
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

mkdir "$DISTDIR/examples"
for E in $EXAMPLE_SET ; do
	cp -pr "$EXAMPLES/$E" "$DISTDIR/examples"
done

../format-subst "$ALS_PROLOG/LICENSE.txt" "$DISTDIR/LICENSE.txt"
cp -p "$MAN/welcome_standard.txt" "$DISTDIR/README.txt"
cp -p $MAN/$MANUAL "$DISTDIR/$MANUALNAME"
cp -p $MAN/$REFMANUAL "$DISTDIR/$REFMANUALNAME"
if test -d "$DOCS"
then
	cp -pr $DOCS "$DISTDIR/docs"
fi
cp -p "$ALS_PROLOG/core/alsp_src/doc/alspro.1" "$DISTDIR/alspro.1"


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
	elif test -f "$BIN/alspro.exe.pst"
	then
	    cp -pr "$BIN/alspro.exe.pst" "$DISTDIR"

	fi
	cp -pr "$BIN/libalspro.a" "$DISTDIR"
	cp -pr "$BIN/libalspro.$SOEXT" "$DISTDIR"
fi

tar -C $ARCH -czf $DISTNAME-$ARCH.tgz $DISTNAME




