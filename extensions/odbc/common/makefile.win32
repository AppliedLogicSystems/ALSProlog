srcdir = \\jarrett\dakota\alsp_src
PROLOG = \\jarrett\dakota\builds\i386_mswin32\bld-port\alspro
CINTDIR = $(srcdir)\cinterf
C2PRODIR = $(CINTDIR)\c2pro
P2INTDIR = $(CINTDIR)\pro2intf
ODBCDIR = ..\src

C2PFS = $(C2PRODIR)\c2p.pro $(C2PRODIR)\c2pro $(C2PRODIR)\cexp \
        $(C2PRODIR)\cfiles  $(C2PRODIR)\cmacro $(C2PRODIR)\cout \
        $(C2PRODIR)\cparse $(C2PRODIR)\ctoken


all : odbc.pro odbc0.c odbc1.c odbc2.c

c2prolcl.pro : $(C2PFS)
	cat $(C2PFS) > c2prolcl.pro


odbc.src : $(ODBCDIR)\odbc.spc c2prolcl.pro
	SET ALS_OPTIONS=heap_size:8000
	$(PROLOG) -b c2prolcl.pro -g c2pro \
		-p -os mswin32 -filter all -srcpath $(ODBCDIR) odbc
	SET ALS_OPTIONS=
 
P2IFS = $(P2INTDIR)\p2i.pro $(P2INTDIR)\pro2intf $(P2INTDIR)\intfout \
	  $(P2INTDIR)\mytrans

p2intlcl.pro : $(P2IFS)
	cat $(P2IFS)> p2intlcl.pro

odbc.pro : odbc2.c

odbc2.c : odbc1.c

odbc1.c : odbc0.c

odbc0.c : odbc.src p2intlcl.pro
	$(PROLOG) -b p2intlcl.pro \
	-g pro2intf -p odbc -fpre odbc -Ddebug -t odbc_trans
