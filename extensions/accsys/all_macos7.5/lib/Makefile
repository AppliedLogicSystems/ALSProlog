
all	Ä	accsys.ppc.lib accsys.68k.lib

GUSIIncludes =   {MPW}Interfaces:GUSIIncludes:

ALSIncludes	= mailbox3:builds:all_macos7.5:

CC68k =   MWC68K

LINK68k =   MWLINK68K

CCPPC =   MWCPPC

LINKPPC =   MWLINKPPC

CFLAGS =   -opt all -mpw_chars -requireprotos -w all -nosyspath -sym full

CFLAGS68K =   -model farData -model codesmart

CFLAGSPPC =   

CPPFLAGS =  -i :::lib_src: -i {ALSIncludes} -i {GUSIIncludes}

:	Ä	:::lib_src:

.68k.o Ä .c
	{CC68k} {DepDir}{Default}.c -o {TargDir}{Default}.68k.o {CPPFLAGS} {CFLAGS} {CFLAGS68K}

.ppc.o Ä .c
	{CCPPC} {DepDir}{Default}.c -o {TargDir}{Default}.ppc.o {CPPFLAGS} {CFLAGS} {CFLAGSPPC} 


ACCSYSOBJPPC = ¶
	addllmod.ppc.o ¶
	compmsc.ppc.o ¶
	dbasedat.ppc.o ¶
	ddapart.ppc.o ¶
	ddapprec.ppc.o ¶
	ddcat.ppc.o ¶
	ddchmod.ppc.o ¶
	ddcmpstr.ppc.o ¶
	ddcreat.ppc.o ¶
	dddate.ppc.o ¶
	ddfields.ppc.o ¶
	ddfldnm.ppc.o ¶
	ddfldno.ppc.o ¶
	ddflush.ppc.o ¶
	ddgetrec.ppc.o ¶
	ddinsrec.ppc.o ¶
	ddmode.ppc.o ¶
	ddopen.ppc.o ¶
	ddreccnt.ppc.o ¶
	ddreclen.ppc.o ¶
	ddrecsta.ppc.o ¶
	ddrmrec.ppc.o ¶
	ddsort.ppc.o ¶
	ddupdrec.ppc.o ¶
	du3itodf.ppc.o ¶
	du3itodk.ppc.o ¶
	duatocf.ppc.o ¶
	duatonf.ppc.o ¶
	dudfto3i.ppc.o ¶
	dudftodk.ppc.o ¶
	dudkto3i.ppc.o ¶
	dudtonf.ppc.o ¶
	dudtonk.ppc.o ¶
	duexpnm.ppc.o ¶
	duinitmf.ppc.o ¶
	duleap.ppc.o ¶
	dunftod.ppc.o ¶
	dunftonk.ppc.o ¶
	dunktoa.ppc.o ¶
	dunktod.ppc.o ¶
	dutmpdir.ppc.o ¶
	dutoday.ppc.o ¶
	_acalloc.ppc.o ¶
	_acjulia.ppc.o ¶
	_asysio.ppc.o ¶
	_asysio2.ppc.o ¶
	_bytes.ppc.o ¶
	_dascnd.ppc.o ¶
	_dbfndwt.ppc.o ¶
	_dcalndr.ppc.o ¶
	_dchktl.ppc.o ¶
	_ddescnd.ppc.o ¶
	_ddopen.ppc.o ¶
	_dfindbf.ppc.o ¶
	_dfindnd.ppc.o ¶
	_dfreem.ppc.o ¶
	_dgetbf.ppc.o ¶
	_dgrbnod.ppc.o ¶
	_dhdnkey.ppc.o ¶
	_dkeyrm.ppc.o ¶
	_dmemcmp.ppc.o ¶
	_dmmseek.ppc.o ¶
	_dopn.ppc.o ¶
	_dpotato.ppc.o ¶
	_dprvkey.ppc.o ¶
	_dputkey.ppc.o ¶
	_drewfwd.ppc.o ¶
	_drmkey.ppc.o ¶
	_dutil.ppc.o ¶
	_dzcmp.ppc.o ¶
	_dzero.ppc.o ¶
	_dzisort.ppc.o ¶
	_dzmain.ppc.o ¶
	_dzmem.ppc.o ¶
	_dzmerge.ppc.o ¶
	_dzutil.ppc.o

ACCSYSOBJ68K = ¶
	addllmod.68k.o ¶
	compmsc.68k.o ¶
	dbasedat.68k.o ¶
	ddapart.68k.o ¶
	ddapprec.68k.o ¶
	ddcat.68k.o ¶
	ddchmod.68k.o ¶
	ddcmpstr.68k.o ¶
	ddcreat.68k.o ¶
	dddate.68k.o ¶
	ddfields.68k.o ¶
	ddfldnm.68k.o ¶
	ddfldno.68k.o ¶
	ddflush.68k.o ¶
	ddgetrec.68k.o ¶
	ddinsrec.68k.o ¶
	ddmode.68k.o ¶
	ddopen.68k.o ¶
	ddreccnt.68k.o ¶
	ddreclen.68k.o ¶
	ddrecsta.68k.o ¶
	ddrmrec.68k.o ¶
	ddsort.68k.o ¶
	ddupdrec.68k.o ¶
	du3itodf.68k.o ¶
	du3itodk.68k.o ¶
	duatocf.68k.o ¶
	duatonf.68k.o ¶
	dudfto3i.68k.o ¶
	dudftodk.68k.o ¶
	dudkto3i.68k.o ¶
	dudtonf.68k.o ¶
	dudtonk.68k.o ¶
	duexpnm.68k.o ¶
	duinitmf.68k.o ¶
	duleap.68k.o ¶
	dunftod.68k.o ¶
	dunftonk.68k.o ¶
	dunktoa.68k.o ¶
	dunktod.68k.o ¶
	dutmpdir.68k.o ¶
	dutoday.68k.o ¶
	_acalloc.68k.o ¶
	_acjulia.68k.o ¶
	_asysio.68k.o ¶
	_asysio2.68k.o ¶
	_bytes.68k.o ¶
	_dascnd.68k.o ¶
	_dbfndwt.68k.o ¶
	_dcalndr.68k.o ¶
	_dchktl.68k.o ¶
	_ddescnd.68k.o ¶
	_ddopen.68k.o ¶
	_dfindbf.68k.o ¶
	_dfindnd.68k.o ¶
	_dfreem.68k.o ¶
	_dgetbf.68k.o ¶
	_dgrbnod.68k.o ¶
	_dhdnkey.68k.o ¶
	_dkeyrm.68k.o ¶
	_dmemcmp.68k.o ¶
	_dmmseek.68k.o ¶
	_dopn.68k.o ¶
	_dpotato.68k.o ¶
	_dprvkey.68k.o ¶
	_dputkey.68k.o ¶
	_drewfwd.68k.o ¶
	_drmkey.68k.o ¶
	_dutil.68k.o ¶
	_dzcmp.68k.o ¶
	_dzero.68k.o ¶
	_dzisort.68k.o ¶
	_dzmain.68k.o ¶
	_dzmem.68k.o ¶
	_dzmerge.68k.o ¶
	_dzutil.68k.o

accsys.ppc.lib	Ä	{ACCSYSOBJPPC}
	{LINKPPC} -library -o accsys.ppc.lib {ACCSYSOBJPPC}
	
accsys.68k.lib	Ä	{ACCSYSOBJ68K}
	{LINK68K} -library -o accsys.68k.lib {ACCSYSOBJ68K}
	
clean	Ä
	Delete -i {ACCSYSOBJPPC} {ACCSYSOBJ68K}

super_clean Ä clean
	Delete -i accsys.ppc.lib accsys.68k.lib
	