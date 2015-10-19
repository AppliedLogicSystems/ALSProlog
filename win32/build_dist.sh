:
#!/bin/sh

set -o errexit -o nounset

ALS_PROLOG=..
BIN=$ALS_PROLOG/core/win32
LIB=$ALS_PROLOG/core/alsp_src/library
BUILTINS=$ALS_PROLOG/core/alsp_src/builtins
EXAMPLES=$ALS_PROLOG/examples
MAN=$ALS_PROLOG/manual
EXTRAS=$ALS_PROLOG/../als_addl_software
ODBC=$ALS_PROLOG/extensions/odbc
PYTHON=$ALS_PROLOG/extensions/python

if test $# -ne 1
then
    echo "Usage: $0 [standard | demo]" 1>&2
    exit 2
fi

EDITION=$1

case $EDITION in
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
cp -pr "$BIN/lib" "$DISTDIR"
cp -pr "$BIN/lib/itcl3.0/itcl30.dll" "$DISTDIR"
cp -pr "$BIN/lib/itk3.0/itk30.dll" "$DISTDIR"

mkdir "$DISTDIR/examples"
for E in $EXAMPLE_SET ; do
	cp -pr "$EXAMPLES/$E" "$DISTDIR/examples"
done

#cp -p $MAN/$WELCOME "$DISTDIR/Welcome.txt"
#cp -p $MAN/copying.als "$DISTDIR/Copying ALS.txt"
cp "$ALS_PROLOG/LICENSE.txt" "$DISTDIR/LICENSE.txt"
cp -p $MAN/welcome_standard.md" "$DISTDIR/README.md"
cp -p $MAN/$MANUAL "$DISTDIR/$MANUALNAME"
mkdir "$DISTDIR/help"
cp -pr $MAN/$HELP/* "$DISTDIR/help"
cp -p $MAN/als_help.htm "$DISTDIR/als_help.htm"

mkdir "$DISTDIR/alsdir/builtins"
cp -p $BUILTINS/*.pro "$DISTDIR/alsdir/builtins"

mkdir "$DISTDIR/alsdir/library"
cp -p $LIB/*.pro "$DISTDIR/alsdir/library"
cp -p $LIB/*.alb "$DISTDIR/alsdir/library"

rm -f "$DISTDIR/alsdir/builtins/blt_shl.pro"
rm -f "$DISTDIR/alsdir/builtins/blt_dvsh.pro"
rm -f "$DISTDIR/alsdir/builtins/ra_basis.pro"
rm -f "$DISTDIR/alsdir/builtins/int_cstr.pro"

EXTRATCL=`ls $EXTRAS/common/tcltk`
for XX in $EXTRATCL 
do
cp -pr "$EXTRAS/common/tcltk/$XX" "$DISTDIR/lib"
done
IWIDGETS=`ls $EXTRAS/common/iwidgets`
for XX in $IWIDGETS 
do
cp -pr "$EXTRAS/common/iwidgets/$XX" "$DISTDIR/lib/iwidgets3.0/scripts"
done

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

if test $EDITION = standard -o $EDITION = demo
then
	mkdir "$DISTDIR/odbc"
	cp -pr "$ODBC/common/odbc.pro" "$DISTDIR/odbc"
	cp -pr "$ODBC/examples/odbc_samples.pro" "$DISTDIR/odbc"
	cp -pr "$ODBC/examples/sql_shell.pro" "$DISTDIR/odbc"
	cp -pr "$ODBC/examples/sql_shell.ppj" "$DISTDIR/odbc"
	cp -pr "$ODBC/examples/economics.mdb" "$DISTDIR/odbc"
	cp -pr "$ODBC/src/meta_odbc.pro" "$DISTDIR/odbc"
	cp -pr "$ODBC/src/prolog_odbc.pro" "$DISTDIR/odbc"
	cp -pr "$ODBC/i386_mswin32/odbcintf.psl" "$DISTDIR/alsdir/shared"
	cp -pr "$ODBC/doc/odbc.pdf" "$DISTDIR/odbc"
	
	cp -pr "$ALS_PROLOG/core/tcltk_interface/mswin32/tclintf.psl" "$DISTDIR/alsdir/shared"
	cp -pr "$ALS_PROLOG/core/tcltk_interface/common/tcltk.pro" "$DISTDIR/alsdir/shared"
	cp -pr "$ALS_PROLOG/core/tcltk_interface/common/tcltk_util.pro" "$DISTDIR/alsdir/shared"

	mkdir "$DISTDIR/python"

#	cp -pr "$PYTHON/i386_mswin32/python.psl" "$DISTDIR/alsdir/shared"
#	cp -pr "$PYTHON/common/als_python_guide.html" "$DISTDIR/python"
#	cp -pr "$PYTHON/common/py_test.pro" "$DISTDIR/python"
#	cp -pr "$PYTHON/common/test_util.py" "$DISTDIR/python"

fi


echo "Build directory complete."
echo "Please add the directory to the installer script and create installer."
