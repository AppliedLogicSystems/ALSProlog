#!/bin/sh

set -o errexit -o nounset

case `uname -rs` in
    "SunOS 4"*)	ARCH=sunos ;;
    "SunOS 5"*)	ARCH=solaris ;;
    Linux*) 	ARCH=linux ;;
    "HP-UX"*)	ARCH=hpux ;;
    "IRIX"*)	ARCH=irix ;;
    *) 		echo "Unknown machine type..."; exit 1 ;;
esac

ALS_PROLOG=..
BIN=$ALS_PROLOG/core/unix/$ARCH
LIB=$ALS_PROLOG/core/alsp_src/library
EXAMPLES=$ALS_PROLOG/examples
MAN=$ALS_PROLOG/manual

if test $# -ne 1
then
    echo "Usage: $0 [student | standard]" 1>&2
    exit 2
fi

EDITION=$1

case $EDITION in
student)
DISTNAME=als-student-prolog
DISTDIR=$ARCH/$DISTNAME ;
EXE="studalsdev" ;
EXAMPLE_SET="als pxs" ;
WELCOME=welcome_student.als ;
MANUAL=student_man.pdf ;
MANUALNAME=student-manual.pdf ;
;;
standard)
DISTNAME=als-prolog
DISTDIR=$ARCH/$DISTNAME ;
EXE=alsdev ;
EXAMPLE_SET="als pxs more objectpro tcltk" ;
WELCOME=welcome_standard.als ;
MANUAL=student_man.pdf ; # standard manual is missing.
MANUALNAME=als-prolog-manual.pdf ;
;;
esac

rm -rf "$DISTDIR"
mkdir -p "$DISTDIR"

cp -pr "$BIN/$EXE" "$DISTDIR"
cp -pr "$BIN/$EXE.pst" "$DISTDIR"
cp -pr "$BIN/alsdir" "$DISTDIR"
cp -pr "$BIN/lib" "$DISTDIR"

mkdir "$DISTDIR/examples"
for E in $EXAMPLE_SET ; do
	cp -pr "$EXAMPLES/$E" "$DISTDIR/examples"
done

cp -p $MAN/$WELCOME "$DISTDIR/REAME"
cp -p $MAN/copying.als "$DISTDIR/copying-als"
cp -p $MAN/$MANUAL "$DISTDIR/$MANUALNAME"

#mkdir "$DISTDIR/alsdir/library"
#cp -p $LIB/*.pro "$DISTDIR/alsdir/library"
#cp -p $LIB/*.alb "$DISTDIR/alsdir/library"

if test $EDITION = standard
then
	cp -pr "$ALS_PROLOG/foreign_sdk/unix/ALS_Prolog_Foreign_SDK" "$DISTDIR"
	cp -pr "$BIN/alspro" "$DISTDIR"
	cp -pr "$BIN/alspro.pst" "$DISTDIR"
fi

tar -C $ARCH -czf $DISTNAME-$ARCH.tgz $DISTNAME




