/*===================================================================
 |		cfgimg.pro
 |	Copyright (c) 1994 Applied Logic Systems, Inc.
 |
 |	Setup directories and make files to link an extended
 |	ALS Prolog image
 |
 |	Author: Ken Bowen
 |	Date begun: 18 Aug 94
 |
 |	Generall assumes that it is linking (interfaced) libraries
 |	with directories setup by mklink.
 *==================================================================*/

module cfgimg.
use sconfig.

export tci/0.
tci :-
	cfg_img(hitk,	[[name=accsys]],
			[
				default_top = '/mailbox3',
				img_path	= '/mailbox3/hi_app', 
				srcdir	= '/mailbox3/alsp_src',
		 		libsdir	= '/mailbox3/als_libs',
		 		probld	= '/mailbox3/builds',
		 		protype	= natv
			]	
		).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% BUILDING AN ARBITRY "IMAGE" DIRECTORY
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*!---------------------------------------------------------------
 | cfg_img/5
 | cfg_img(ImgName, ImgDirPath, LinkLibs, Options)
 | cfg_img(+, +, +, +, +)
 |
 |	-- create a directory for linking an extended ALS Prolog image
 |
 | Arguments:
 | ---------
 | ImgName		- an atom naming the image to be created; also used as
 |				  name of dir where image (architecture-specific) 
 |				  subdirs will (do) live;
 | IMPath		- an atom which is a path name to where the subdir ImgName 
 |				  either exists or should be created;
 | LinkLibs		- a list of expressions defining the libraries to be added
 |				  to ALS Prolog to make up the image;
 | Options 		- a list of option pairs as described below
 |
 |	Options:
 |	-------
 |	arch 	= the architecture to use; default is the value of arch
 |			  gotten from the current image via als_system/1;
 |	os 		= the operating system to use; default is the value of os_variation
 |			  gotten from the current image via als_system/1;
 |	srcdir	= the path to the top of the ALS source tree;
 |			  used only when the various initial Makefile's (.in) are
 |			  NOT to be take from the ALSDIR of the running image;
 |			  The initial Makefile for creating the raw underlying
 |			  library is called makelink.in, and the initial Makefile
 |			  for creating the interface library (from ALS to the raw
 |			  library) is called makeintf.in.  Both can be found in
 |			  the "linking" subdir of ALSDIR, in an installation,
 |			  or in subdir pconfig in the source tree.
 *!--------------------------------------------------------------*/

export cfg_img/4.
cfg_img(ImgName, IMPath, LinkLibs, Options)
	:-
	get_cwd(OrigDir),
	check_default(Options, default_top, OrigDir, DefltTop),
	(exists_file(DefltTop) -> true ;
		als_advise('Error: Path %t does not exist.\n', [DefltTop]),
		fail),

	check_default(Options, img_path, DefltTop, IMPath), 

	(exists_file(IMPath) -> true ;
		als_advise('Error: Path %t does not exist.\n', [IMPath]),
		fail),
	change_cwd(IMPath),
	existsmake_subdir(ImgName,IMPath),
	change_cwd(ImgName),

	als_system(SysVals),
	check_default(Options, arch, 
					Arch^dmember(processor=Arch, SysVals), Arch),
	check_default(Options, os_variation, 
					OSVar^dmember(os_variation=OSVar, SysVals), OSVar),

	catenate([Arch,'_',OSVar], ArchOS),
	existsmake_subdir(ArchOS,IMPath/ImgName),
	change_cwd(ArchOS),

	check_default(Options, protype, port, PROTYPE),
	check_default(Options, probld, 
					PROBLD^extendPath(DefltTop, builds, PROBLD), PROBLD),

				srcdir	= '/mailbox3/alsp_src',
		 		libsdir	= '/mailbox3/als_libs',

	check_default(Options, libsdir, 
					LIBSDIR^extendPath(DefltTop, als_libs, LIBSDIR), LIBSDIR),

	(dmember(srcdir=ALSSRC, Options) ->
		extendPath(ALSSRC,pconfig,PCONFIGPath),
		pathPlusFile(PCONFIGPath, 'makeimg.in', InitImgMakefile),
		Extra = [srcdir=ALSSRC]
		;
		builtins:sys_searchdir(ALSDIRPath),
		extendPath(ALSDIRPath,linking,LinkingPath),
		pathPlusFile(LinkingPath, 'makeimg.in', InitImgMakefile),
		Extra = []
	),

/*
	ilsrcd(Options, ISDIR, LSRCDIR, LSRCSRC),
	change_cwd(lib),

	pathPlusFile(LSRCSRC, 'libspc.pro', LibSPEC),
	consult(LibSPEC),
*/
	open('Makefile',write,OutS,[]),
	printf(OutS,'#\n# Makefile for the %t image\n#\n\n',[ImgName]),
	general_os(Arch, OSVar, GOS), 

	write_make_eqns([
		'ARCH'			= Arch,
		'OS' 			= OSVar,
		'GOS' 			= GOS,
		probld 			= PROBLD,
		protype			= PROTYPE,
		'TSYSN' 		= ImgName
		| Extra   ], OutS),

	img_makefile(LinkLibs,0,Arch,OSVar,LIBSDIR,Options,OutS, LinkList, Inits),

	open(InitImgMakefile, read, MSrc, []),
	copy_stream_nl(MSrc, OutS, GOS),
	close(MSrc),

	write_link_elts(LinkList, OutS),
	printf(OutS,'\t\t$(CLIBS)\n\n',[]),
	close(OutS),

	open('pi_init.c',write,PISt,[]),
	write_inits(Inits, PISt),
	close(PISt),

	open('pi_cfg.h',write,PICFGSt,[]),
	printf(PICFGSt,'\n',[]),
	close(PICFGSt),

	change_cwd(OrigDir).



img_makefile([],_,_,_,_,_,_, [], []).
img_makefile([CompLib | LinkLibs], CurN,Arch,OS, LIBSDIR, Options, OutS, 
				[CLI | LinkList], [CLInit | Inits])
	:-
	img_cmp_make(CompLib, CurN,Arch,OS, LIBSDIR, Options, OutS, CLI, CLInit),
	NextN is CurN + 1,
	img_makefile(LinkLibs,NextN,Arch,OS,LIBSDIR, Options,OutS, LinkList, Inits).

img_cmp_make(CompLib, CurN, Arch,OS, LIBSDIR, Options, OutS, CLI, CLInit)
	:-
	dmember(name=CmpLibName, CompLib),
	catenate(['LIB', CurN, '_NM'], LNameName),

	extendPath(LIBSDIR, CmpLibName, LibBld),
	pathPlusFile(LibBld,info,LibInfoFile),
	open(LibInfoFile,read,LIS,[]),
	read_term(LIS,LIList,[]),
	close(LIS),
	dmember(isdir=SRCDIR,LIList),
	catenate(['LIB', CurN, sd], LsdName),

	catenate([Arch,'_',OS], ArchOS),
	extendPath(LibBld,ArchOS,LBAOS),
	extendPath(LBAOS,lib,LBAOSl),
	lib_extension(OS,LExt),
	filePlusExt(CmpLibName,LExt,RawLibFileName),
	pathPlusFile(LBAOSl,RawLibFileName,RawLibFile),
	catenate(['LIB', CurN, '_rl'], RLNameName),
	
	extendPath(LBAOS,intf,LBAOSi),
	catenate(CmpLibName,interf,LIF),
	filePlusExt(LIF,LExt,IntfFileName),
	pathPlusFile(LBAOSi,IntfFileName,IntfLibFile),
	catenate(['LIB', CurN, '_il'], ILNameName),

	write_make_eqns([
		LNameName	= CmpLibName,
		LsdName		= SRCDIR,
		RLNameName	= RawLibFile,
		ILNameName	= IntfLibFile ], OutS),

	CLI = [IntfLibFile, RawLibFile],
	catenate(CmpLibName,'_init();',CLInit).
		


write_link_elts([], OutS).
write_link_elts([CLL | LinkList], OutS)
	:-
	printf(OutS,'\t\t',[]),
	write_cl_elts(CLL,OutS),
	printf(OutS,'\\\n',[]),
	write_link_elts(LinkList, OutS).

write_cl_elts([],OutS).
write_cl_elts([L | CLL],OutS)
	:-
	printf(OutS,'%t ',[L]),
	write_cl_elts(CLL,OutS).



write_inits([], PISt).
write_inits([Init | Inits], PISt)
	:-
	printf(PISt,'\t%t\n',[Init]),
	write_inits(Inits, PISt).






















lib_link_makefile(ImgName,Arch,OSVar,ISDIR,InitLibMakefile)
	:-
	open('Makefile',write,OutS,[]),
	printf(OutS,'#\n# Makefile for the %t library\n#\n\n',[ImgName]),

	protoflags(PROTOFLAGS),
	x_cflags(X_CFLAGS),
	xlibs(X_LIBS),
	x_extra_libs(X_EXTRA_LIBS),
	xincludes(XINCLUDES),
	write_make_eqns([
		'ARCH'			= Arch,
		'OS' 			= OSVar,
		'INTF_NM'		= ImgName,
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

lib_intf_makefile(ImgName,Arch,OSVar,ALSSRC,ISDIR,InitIntfMakefile,Options)
	:-
	open('Makefile',write,OutS,[]),
	printf(OutS,'#\n# Makefile for the interface to the %t library\n#\n\n',[ImgName]),

	general_os(Arch, OSVar, GOS), 
	(dmember(fprefix=FPrefix, Options) ->
		true ; 
		FPrefix = ImgName),
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
		'INTF_NM'		= ImgName,
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
