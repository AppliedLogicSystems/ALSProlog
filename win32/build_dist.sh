:
#!/bin/sh

set -o errexit -o nounset

ALS_PROLOG=..
BIN=$ALS_PROLOG/core/win32
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
DISTDIR="ALS Student Prolog" ;
EXE="ALS Student Prolog.exe" ;
EXAMPLE_SET="als pxs" ;
WELCOME=welcome_student.als ;
MANUAL=student_man.pdf ;
MANUALNAME="Student Manual.pdf" ;
;;
standard)
DISTDIR="ALS Prolog" ;
EXE="ALS Prolog.exe" ;
EXAMPLE_SET="als pxs more objectpro tcltk" ;
WELCOME=welcome_standard.als ;
MANUAL=standard_man.pdf ;
MANUALNAME="ALS Prolog Manual.pdf" ;
;;
esac

rm -rf "$DISTDIR"
mkdir "$DISTDIR"

cp -pr "$BIN/$EXE" "$DISTDIR"
cp -pr "$BIN/msvcrt.dll" "$DISTDIR"
cp -pr "$BIN/tcl80.dll" "$DISTDIR"
cp -pr "$BIN/tk80.dll" "$DISTDIR"
cp -pr "$BIN/tclpip80.dll" "$DISTDIR"
cp -pr "$BIN/alsdir" "$DISTDIR"
cp -pr "$BIN/lib" "$DISTDIR"

mkdir "$DISTDIR/examples"
for E in $EXAMPLE_SET ; do
	cp -pr "$EXAMPLES/$E" "$DISTDIR/examples"
done

cp -p $MAN/$WELCOME "$DISTDIR/Welcome.txt"
cp -p $MAN/copying.als "$DISTDIR/Copying ALS.txt"
cp -p $MAN/$MANUAL "$DISTDIR/$MANUALNAME"

mkdir "$DISTDIR/alsdir/library"
cp -p $LIB/*.pro "$DISTDIR/alsdir/library"
cp -p $LIB/*.alb "$DISTDIR/alsdir/library"

if test $EDITION = standard
then
	cp -pr "$ALS_PROLOG/foreign_sdk/win32/ALS_Prolog_Foreign_SDK" "$DISTDIR"
	cp -pr "$BIN/alspro.exe" "$DISTDIR"
fi

echo "Build directory complete."
echo "Please add the directory to the installer script and create installer."
