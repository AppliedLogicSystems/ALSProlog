srcdir = ../../../alsp_src
PROLOG = ../../../core/win32/alspro.exe
CINTDIR = ../../cinterf
C2PRODIR = $(CINTDIR)/c2pro
P2INTDIR = $(CINTDIR)/pro2intf
ODBCDIR = ../src

C2PFS = $(C2PRODIR)/c2p.pro $(C2PRODIR)/c2pro $(C2PRODIR)/cexp \
        $(C2PRODIR)/cfiles  $(C2PRODIR)/cmacro $(C2PRODIR)/cout \
        $(C2PRODIR)/cparse $(C2PRODIR)/ctoken


all : odbc.pro odbc0.c odbc1.c odbc2.c

c2prolcl.pro : $(C2PFS)
	cat $(C2PFS) > c2prolcl.pro


odbc.src : c2prolcl.pro $(ODBCDIR)/odbc.spc 
	$(PROLOG) c2prolcl \
		-p -os mswin32 -filter all -srcpath $(ODBCDIR) odbc

#	$(PROLOG) -b c2prolcl -g c2pro \
#	SET ALS_OPTIONS=heap_size:8000
#	SET ALS_OPTIONS=
 
P2IFS = $(P2INTDIR)/p2i.pro $(P2INTDIR)/pro2intf $(P2INTDIR)/intfout \
	  $(P2INTDIR)/mytrans

p2intlcl.pro : $(P2IFS)
	cat $(P2IFS)> p2intlcl.pro

odbc.pro : odbc2.c

odbc2.c : odbc1.c

odbc1.c : odbc0.c

odbc0.c : odbc.src p2intlcl.pro
	$(PROLOG) p2intlcl.pro \
	-p odbc -fpre odbc -Ddebug -t odbc_trans

#	$(PROLOG) -b p2intlcl.pro \
#	-g pro2intf -p odbc -fpre odbc -Ddebug -t odbc_trans
