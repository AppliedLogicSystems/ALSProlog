:
#!/bin/sh

set -o errexit -o nounset

ALS_PROLOG=..
BIN=$ALS_PROLOG/core/win32
LIB=$ALS_PROLOG/core/alsp_src/library
BUILTINS=$ALS_PROLOG/core/alsp_src/builtins
EXAMPLES=$ALS_PROLOG/examples
MAN=$ALS_PROLOG/manual

if test $# -ne 1
then
    echo "Usage: $0 [student | standard | demo]" 1>&2
    exit 2
fi

EDITION=$1

case $EDITION in
student)
DISTDIR="ALS Student Prolog" ;
EXE="ALS Student Prolog.exe" ;
EXET="ALS Student Prolog.exe" ;
EXAMPLE_SET="als pxs Chat80" ;
WELCOME=welcome_student.als ;
MANUAL=student_man.pdf ;
MANUALNAME="Student Manual.pdf" ;
HELP="studhelp" ;
;;
standard)
DISTDIR="ALS Prolog" ;
EXE="ALS Prolog.exe" ;
EXET="ALS Prolog.exe" ;
EXAMPLE_SET="als pxs more objectpro visual Prolog1000 Chat80" ;
WELCOME=welcome_standard.als ;
MANUAL=als_man.pdf ; 
MANUALNAME="ALS Prolog Manual.pdf" ;
HELP="alshelp" ;
;;
demo)
DISTDIR="ALS Prolog Demo" ;
EXE="ALS Prolog Demo.exe" ;
EXET="ALS Prolog.exe" ;
EXAMPLE_SET="als pxs more objectpro visual Prolog1000 Chat80" ;
WELCOME=welcome_standard.als ;
MANUAL=als_man.pdf ; 
MANUALNAME="ALS Prolog Manual.pdf" ;
HELP="alshelp" ;
;;
esac

rm -rf "$DISTDIR"
mkdir "$DISTDIR"

cp -pr "$BIN/$EXE" "$DISTDIR/$EXET"
cp -pr "$BIN/msvcrt.dll" "$DISTDIR"
cp -pr "$BIN/tcl80.dll" "$DISTDIR"
cp -pr "$BIN/tk80.dll" "$DISTDIR"
cp -pr "$BIN/tclpip80.dll" "$DISTDIR"
cp -pr "$BIN/alsdir" "$DISTDIR"
rm -f "$DISTDIR/alsdir/builitins/blt_shl.pro"
rm -f "$DISTDIR/alsdir/builitins/blt_dvsh.pro"
rm -f "$DISTDIR/alsdir/builitins/ra_basis.pro"
rm -f "$DISTDIR/alsdir/builitins/int_cstr.pro"
cp -pr "$BIN/lib" "$DISTDIR"
cp -pr "$BIN/lib/itcl3.0/itcl30.dll" "$DISTDIR"
cp -pr "$BIN/lib/itk3.0/itk30.dll" "$DISTDIR"

mkdir "$DISTDIR/examples"
for E in $EXAMPLE_SET ; do
	cp -pr "$EXAMPLES/$E" "$DISTDIR/examples"
done

cp -p $MAN/$WELCOME "$DISTDIR/Welcome.txt"
cp -p $MAN/copying.als "$DISTDIR/Copying ALS.txt"
cp -p $MAN/$MANUAL "$DISTDIR/$MANUALNAME"
mkdir "$DISTDIR/help"
cp -pr $MAN/$HELP/* "$DISTDIR/help"
cp -p $MAN/als_help.htm "$DISTDIR/als_help.htm"

mkdir "$DISTDIR/alsdir/builtins"
cp -p $BUILTINS/*.pro "$DISTDIR/alsdir/builtins"

mkdir "$DISTDIR/alsdir/library"
cp -p $LIB/*.pro "$DISTDIR/alsdir/library"
cp -p $LIB/*.alb "$DISTDIR/alsdir/library"

if test $EDITION = standard
then
	cp -pr "$ALS_PROLOG/foreign_sdk/win32/ALS_Prolog_Foreign_SDK" "$DISTDIR"
	cp -pr "$BIN/alspro.exe" "$DISTDIR"
	cp -pr "$BIN/alspro.dll" "$DISTDIR"
	cp -pr "$BIN/Generic ALS App Stub.exe" "$DISTDIR"
fi

if test $EDITION = demo
then
	cp -pr "$ALS_PROLOG/foreign_sdk/win32/ALS_Prolog_Foreign_SDK" "$DISTDIR"
	cp -pr "$BIN/alspro_demo.exe" "$DISTDIR/alspro.exe"
	cp -pr "$BIN/alspro.dll" "$DISTDIR"
	cp -pr "$BIN/Generic ALS App Stub.exe" "$DISTDIR"
fi


echo "Build directory complete."
echo "Please add the directory to the installer script and create installer."
