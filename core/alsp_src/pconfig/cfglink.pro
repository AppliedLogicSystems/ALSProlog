/*===================================================================
 |		cfglink.pro
 |	Copyright (c) 1994 Applied Logic Systems, Inc.
 |
 |	Setup directories and make files to interface
 |	to a C library
 |
 |	Author: Ken Bowen
 |	Date begun: 8 Aug 94
 *==================================================================*/

module mklink.
use sconfig.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% BUILDING AN ARBITRY "LINKING" DIRECTORY
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
export tml/0.
tml :-
	mklinker(accsys, '/mailbox3/als_libs', '/mailbox3/alsp_src',
			 [	srcmake = true,
				protype = natv,
				probld	= '/mailbox3/builds',
				fprefix = acc,
			 	isdir='/jarrett/dbi/dbase' ]).

/*!---------------------------------------------------------------
 | mklinker(LLibName, LDPath
 | mklinker(LLibName, LDPath
 | mklinker(LLibName, LDPath
 |
 |	-- create a directory for build a library interface
 |
 | Arguments:
 | ---------
 | LLibName		- an atom naming the library; used as name of  the dir
 |				  where lib interface (sub)dirs will (do) live;
 | LDPath		- an atom which is a path name to where the LLibName either
 |		  		  exists or should be created;
 | ALSSRC		- the path to the top of the ALS source tree
 | Options 		- a list of option pairs as described below
 |
 |
 |	Options:
 |	-------
 |	arch 	= the architecture to use; default is the value of arch
 |			  gotten from the current image via als_system/1;
 |	os 		= the operating system to use; default is the value of os_variation
 |			  gotten from the current image via als_system/1;
 |	srctrue	= flag whose value is set to true
 |			  only when the various initial Makefile's (.in) are
 |			  NOT to be take from the ALSDIR of the running image;
 |			  The initial Makefile for creating the raw underlying
 |			  library is called makelink.in, and the initial Makefile
 |			  for creating the interface library (from ALS to the raw
 |			  library) is called makeintf.in.  Both can be found in
 |			  the "linking" subdir of ALSDIR, in an installation,
 |			  or in subdir pconfig in the source tree.
 *!--------------------------------------------------------------*/

export mklinker/4.
mklinker(LLibName, LDPath, ALSSRC, Options)
	:-
	(exists_file(LDPath) ->
		true ;
		als_advise('Error: Path %t does not exist.\n', [LDPath]),
		fail
	),
	get_cwd(OrigDir),
	change_cwd(LDPath),
	existsmake_subdir(LLibName,LDPath),

	ilsrcd(Options, ISDIR, LSRCDIR, LSRCSRC),
	change_cwd(LLibName),
	open(info,write,InfoOutS,[]),
	write_clause(InfoOutS, [isdir=ISDIR]),
	close(InfoOutS),

	als_system(SysVals),
	(dmember(arch=Arch, Options) ->
		true
		;
		dmember(processor=Arch, SysVals)
	),
	(dmember(os_variation=OSVar, Options) ->
		true
		;
		dmember(os_variation=OSVar, SysVals)
	),
	catenate([Arch,'_',OSVar], ArchOS),
	existsmake_subdir(ArchOS,LDPath/LLibName),
	
	change_cwd(ArchOS),
	existsmake_subdir(intf,LDPath/LLibName/ArchOS),
	existsmake_subdir(lib,LDPath/LLibName/ArchOS),

	(dmember(srcmake=true, Options) ->
		extendPath(ALSSRC,pconfig,PCONFIGPath),
		pathPlusFile(PCONFIGPath, 'makelink.in', InitLibMakefile),
		pathPlusFile(PCONFIGPath, 'makeintf.in', InitIntfMakefile)
		;
		builtins:sys_searchdir(ALSDIRPath),
		extendPath(ALSDIRPath,linking,LinkingPath),
		pathPlusFile(LinkingPath, 'makelink.in', InitLibMakefile),
		pathPlusFile(LinkingPath, 'makeintf.in', InitIntfMakefile)
	),

	change_cwd(lib),

	pathPlusFile(LSRCSRC, 'libspc.pro', LibSPEC),
	consult(LibSPEC),

	lib_link_makefile(LLibName,Arch,OSVar,ISDIR,InitLibMakefile),

	change_cwd('../intf'),
	lib_intf_makefile(LLibName,Arch,OSVar,ALSSRC,ISDIR,InitIntfMakefile,Options).


lib_link_makefile(LLibName,Arch,OSVar,ISDIR,InitLibMakefile)
	:-
	open('Makefile',write,OutS,[]),
	printf(OutS,'#\n# Makefile for the %t library\n#\n\n',[LLibName]),

	protoflags(PROTOFLAGS),
	x_cflags(X_CFLAGS),
	xlibs(X_LIBS),
	x_extra_libs(X_EXTRA_LIBS),
	xincludes(XINCLUDES),
	write_make_eqns([
		'ARCH'			= Arch,
		'OS' 			= OSVar,
		'INTF_NM'		= LLibName,
		isdir			= ISDIR,
		'PROTOFLAGS'		= PROTOFLAGS,
		'X_CFLAGS'		= X_CFLAGS,
		'X_LIBS'		= X_LIBS,
		'X_EXTRA_LIBS'		= X_EXTRA_LIBS,
		'XINCLUDES'		= XINCLUDES],  OutS),

	general_os(Arch, OSVar, GOS), 
	open(InitLibMakefile, read, SrcS, []),
	copy_stream_nl(SrcS, OutS, GOS),
	close(SrcS),

	lib_source_files(FileNamesList),
	printf(OutS,'BASEOBJS\t= ',[]),
	writeout_ofiles(FileNamesList, OutS),

	printf(OutS,'\n$(BASELIB): $(BASEOBJS)\n',[]),
	printf(OutS,'\techo BASEOBJS=$(BASEOBJS)\n',[]),
	printf(OutS,'\tar ruv $(BASELIB) $(BASEOBJS)\n',[]),
	printf(OutS,'\tranlib $(BASELIB)\n\n',[]),

	printf(OutS,'\n# Dependencies:\n\n',[]),
	writeout_deps(FileNamesList, OutS),
	close(OutS).

lib_intf_makefile(LLibName,Arch,OSVar,ALSSRC,ISDIR,InitIntfMakefile,Options)
	:-
	open('Makefile',write,OutS,[]),
	printf(OutS,'#\n# Makefile for the interface to the %t library\n#\n\n',[LLibName]),

	general_os(Arch, OSVar, GOS), 
	(dmember(fprefix=FPrefix, Options) ->
		true ; 
		FPrefix = LLibName),
	(dmember(protype=PROTYPE, Options) ->
		true ; 
		PROTYPE = port),
	(dmember(probld=PROBLD, Options) ->
		true 
		; 
		builtins:sys_searchdir(PROBLD)),
	(dmember(srcmake=true, Options) ->
		extendPath(ALSSRC,cinterf,CINTDIR),
		extendPath(CINTDIR,c2pro,C2PDIR),
		extendPath(CINTDIR,pro2intf,P2IDIR)
		; 
		builtins:sys_searchdir(CINTDIR),
		C2PDIR = CINTDIR,
		P2IDIR = CINTDIR
	),

	intf_cflags(INTF_CFLAGS),
	intf_c2pflags(INTF_C2PFLAGS),
	intf_p2iflags(INTF_P2IFLAGS),
	intf_includes(INTF_INCLUDES),
	intf_defines(INTF_DEFINES),

	prolog_lcns(Options,Arch,OSVar,PROTYPE,PrologStuff),

	write_make_eqns([
		'ARCH'			= Arch,
		'OS' 			= OSVar,
		'GOS' 			= GOS,
		'INTF_NM'		= LLibName,
		isdir			= ISDIR,
		protype			= PROTYPE,
		'FPREFIX'		= FPrefix,
		srcdir			= ALSSRC,
		'CINTDIR'		= CINTDIR,
		'C2PDIR'		= C2PDIR,
		'P2IDIR'		= P2IDIR,
		'INTF_CFLAGS'		= INTF_CFLAGS,
		'INTF_C2PFLAGS'		= INTF_C2PFLAGS,
		'INTF_P2IFLAGS'		= INTF_P2IFLAGS,
		'INTF_INCLUDES'		= INTF_INCLUDES,
		'INTF_DEFINES'		= INTF_DEFINES 
		| PrologStuff ],  OutS),

	open(InitIntfMakefile, read, SrcS, []),
	copy_stream_nl(SrcS, OutS, GOS),
	close(SrcS).


prolog_lcns(Options,Arch,OS,PROTYPE,PrologStuff)
	:-
	dmember(prolib=PROLIB, Options),
	dmember(prolog=PROLOG, Options),
	!,
	PrologStuff = [ 'PROLIB' = PROLIB, 'PROLOG' = PROLOG ].

prolog_lcns(Options,Arch,OS,PROTYPE,PrologStuff)
	:-
	dmember(probld=PROBLD, Options),
	!,
	catenate([Arch,'_',OS],ArchOS),
	catenate('bld-',PROTYPE,WHATTYPE),
	extendPath(PROBLD,ArchOS,IP1),
	extendPath(IP1,WHATTYPE, PROBLDD),
	pathPlusFile(PROBLDD,'alspro.a',PROLIB),
	pathPlusFile(PROBLDD,alspro,PROLOG),
	PrologStuff = [ 
		'PROBLD' = PROBLD, 
		'PROBLDD' = PROBLDD, 
		'PROLIB' = PROLIB, 
		'PROLOG' = PROLOG 
		].

	%% NEED a case to get everything out of an
	%% INSTALL setup [i.e., from the ALSDIR associated with
	%% the current running image.]
/*
prolog_lcns(Options,Arch,OS,PROTYPE,PrologStuff)
	:-
*/

existsmake_subdir(Name,Path)
	:-
	exists_file(Name),
	!,
	als_advise('%t exists...\n', [Path/Name]).

existsmake_subdir(Name,Path)
	:-
	als_advise('Creating %t ...\n', [Path/Name]),
	make_subdir(Name, 511).

ilsrcd(Options, ISDIR, LSRCDIR, LSRCSRC)
	:-
	dmember(isdir=ISDIR, Options),
	!,
	extendPath(ISDIR,lib,LSRCDIR),
	extendPath(ISDIR,src,LSRCSRC).
ilsrcd(Options, ISDIR, LSRCDIR, LSRCSRC)
	:-
	als_advise('Cant determine library source directories!\n',[]),
	fail.

write_make_eqns([], OutS).
write_make_eqns([Left=Right | RestEqns], OutS)
	:-
	printf(OutS,'%t\t= %t\n',[Left,Right]),
	write_make_eqns(RestEqns, OutS).


writeout_ofiles([], OutS).
writeout_ofiles([File], OutS)
	:-!,
	printf(OutS,'%t.o \n',[File]).
writeout_ofiles([File | FileNamesList], OutS)
	:-
	printf(OutS,'%t.o \\\n',[File]),
	writeout_ofiles(FileNamesList, OutS).

writeout_deps([], OutS).
writeout_deps([File], OutS)
	:-!,
	printf(OutS,'%t.o : %t.c \n',[File,File]).
writeout_deps([File | FileNamesList], OutS)
	:-
	printf(OutS,'%t.o : %t.c \n',[File,File]),
	writeout_deps(FileNamesList, OutS).




endmod.
