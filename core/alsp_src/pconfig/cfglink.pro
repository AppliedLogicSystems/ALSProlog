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

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% BUILDING AN ARBITRY "LINKING" DIRECTORY
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
export tml/0.
tml :-
	mklinker(accsys, '/mailbox3/als_libs', '/mailbox3/alsp_src',
			 [	srcmake = true,
				protype = natv,
				probld	= '/mailbox3/builds',
				fprefix = acc,
			 	isdir='/jarrett/dbi/dbase' ]).
****/
export tmj/0.
tmj :-
	mklinker(accsys).

export mklinker/1.
mklinker(LLibName)
	:-
	'$getenv'('ALSTOP', ALSTOPStr),
	name(ALSTOP, ALSTOPStr),
	extendPath(ALSTOP, als_libs, ALSLIBS),
	extendPath(ALSTOP, alsp_src, ALSSRC),
	mklinker(LLibName, ALSLIBS, ALSSRC).

export mklinker/3.
mklinker(LLibName, ALSLIBS, ALSSRC)
	:-
	extendPath(ALSLIBS,LLibName,LLIBTopDir),
	extendPath(LLIBTopDir,spec, LIBSPEC),
	open(LIBSPEC,read,LSStrm,[]),
	read_term(LSStrm,LIBEqns,[]),
	close(LSStrm),
	mklinker(LLibName, ALSLIBS, ALSSRC, LIBEqns).

/*!---------------------------------------------------------------
 | mklinker/4
 | mklinker(LLibName, LDPath, ALSSRC, Options)
 | mklinker(+,+,+,+)
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

	(dmember(codetrees=CodeTrees, Options) ->
		check_default(Options, includes, [], Includes),
		get_cwd(TreeStart),
		mk_codetrees(CodeTrees, Includes, Options, '')
		;
		true
	),

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

	setup_lib_from_filenames(FileNamesList, OutS),

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

mk_codetrees(CodeTrees, Includes, Options, ParentDir)
	:-
	dmember(isdir=LibSrcDir, Options),
	(sub_atom(LibSrcDir,1,1,'/') ->
		%% path is already absolute
		APath = LibSrcDir
		;
		%% lib src path is relative to the dir we are now in;
		%% make an absolute path to that dir:
		get_cwd(CurPath),
		extendPath(CurPath,LibSrcDir, APath)
	),
	mk_codetrees(CodeTrees, Includes, [aisdir=APath | Options], ParentDir, '.').

mk_codetrees([], Includes, Options, _,_).
mk_codetrees([CTree | CodeTrees], Includes, Options, ParentPath, P2Top)
	:-
	get_cwd(CurDir),
	name_it(CTree, CTreeName),
	make_codetree(CTree, CTreeName, Includes, Options, ParentPath, P2Top),
	change_cwd(CurDir),
	mk_codetrees(CodeTrees, Includes, Options, ParentPath, P2Top).

name_it(CTreeCore-_, CTree)
	:-!,
	name_it(CTreeCore, CTree).

name_it(CTreeCore, CTreeCore)
	:-
	atom(CTreeCore),
	!.
name_it(CTreeCore, CTree)
	:-
	dmember(name=CTree,CTreeCore).

make_codetree(CTree-SubTrees, CTreeName, Includes, Options, ParentPath, P2Top)
	:-!,
	extendPath(ParentPath, CTreeName, NewPath),
	extendPath(P2Top, '..', NewP2Top),
	subtree_names(SubTrees, SubTreeNames),
	create_codedir(CTree, CTreeName, Includes, Options, SubTreeNames, NewPath, NewP2Top),
	mk_codetrees(SubTrees, Includes, Options, NewPath, NewP2Top).

make_codetree(CTree, CTreeName, Includes, Options, ParentPath, P2Top)
	:-
	extendPath(ParentPath, CTreeName, NewPath),
	extendPath(P2Top, '..', NewP2Top),
	create_codedir(CTree, CTreeName, Includes, Options, [], NewPath, NewP2Top).

create_codedir(CTree, CTreeName, Includes, Options, SubTreeNames, Path2Me, MePath2Top)
	:-
	existsmake_subdir(CTreeName,'.'),
	change_cwd(CTreeName),
printf('Dir >> %t\n',[CTreeName]),
	lcl_tree_lib_makefile(CTree, CTreeName, Includes, Options, SubTreeNames, Path2Me, MePath2Top).

lcl_tree_lib_makefile(CTree, CTreeName, Includes, Options, SubTreeNames, Path2Me, MePath2Top)
	:-
		%% we know this path is absolute:
	dmember(aisdir=LibSrcDir, Options),
	dmember(system=System, Options),

	open(makefile, write, MFStr, []),
	printf(MFStr,'#  \n#  Makefile for %t library: %t\n# \n',[System,Path2Me]),

	extendPath(LibSrcDir,Path2Me,ParallelSrcPath),
	printf(MFStr,'\nVPATH=%t\n\n',[ParallelSrcPath]),
	
	prefix_dir(Includes, LibSrcDir, IncludesList),
	bagOf(IIncl, D^(member(D,IncludesList),catenate('-I',D,IIncl)),LclIncs),
	cat_together_spaced(LclIncs, LclIncsStr),
	printf(MFStr,'\nLclIncs= %t\n',[LclIncsStr]),

	check_default(Options, lclflags, '', LclFlags),
	printf(MFStr, 'LclFlags = %t\n', [LclFlags]),

	files(ParallelSrcPath,'*.c',InitCFiles),
	reduction(Options, InitCFiles, CFiles),
	strip_suffixes(CFiles, FileNamesList),

	check_default(CTree, libname, CTreeName, BaseLibName),
	catenate(BaseLibName,'.a',BLA),
	printf(MFStr, '\nBASELIB=%t\n\n',[BLA]),
	cat_together_spaced(SubTreeNames, SubDeps),
	printf(MFStr,'\n\nall: library %t\n',[SubDeps]),
	setup_lib_from_filenames(FileNamesList, MFStr),
	subdir_rules(SubTreeNames, MFStr),

	close(MFStr).

setup_lib_from_filenames(FileNamesList, OutStr)
	:-
	printf(OutStr,'\nCC = gcc\nLINK = $(CC)\n',[]),
	printf(OutStr,'CFLAGS = -O -g -Wall -Wshadow -Wconversion $(PROTOFLAGS)\n',[]),
	printf(OutStr,'LIBS = -lnsl -lm\n',[]),
	printf(OutStr,'#\n#\nCPPFLAGS = -I. $(LclIncs) $(XINCLUDES) $(LclFlags)\n',[]),
	printf(OutStr,'.c.o:\n\t$(CC) -c $(CPPFLAGS) $(CFLAGS) $(X_CFLAGS) $<\n\n',[]),

	printf(OutStr,'BASEOBJS\t= ',[]),
	writeout_ofiles(FileNamesList, OutStr),

	printf(OutStr,'\nlibrary: $(BASELIB)\n\n',[]),
	
	printf(OutStr,'\n$(BASELIB): $(BASEOBJS)\n',[]),
	printf(OutStr,'\techo BASEOBJS=$(BASEOBJS)\n',[]),
	printf(OutStr,'\tar ruv $(BASELIB) $(BASEOBJS)\n',[]),
	printf(OutStr,'\tranlib $(BASELIB)\n\n',[]),

	printf(OutStr,'\n# Dependencies:\n\n',[]),
	writeout_deps(FileNamesList, OutStr).



strip_suffixes([], []).
strip_suffixes([File | Files], [Name | NamesList])
	:-
	strip_suffix(File, Name),
	strip_suffixes(Files, NamesList).

strip_suffix(File, Name)
	:-
	asplit(File, 0'., Left, Right),
	!,
	strip_suffix0(File, Name).

strip_suffix(Name, Name).

strip_suffix0(File, Name)
	:-
	bang_apart(File, PartsLessLast),
	catenate(PartsLessLast, Name).

reduction(Options, InitCFiles, CFiles)
	:-
	dmember(omitlist=OL, Options),
	!,
	list_diff(InitCFiles, OL, CFiles).

reduction(Options, InitCFiles, CFiles)
	:-
	dmember(omitpattern=OP, Options),
	!,
	match_pat(InitCFiles, OP, _, CFiles).

match_pat([], _, [], []).
match_pat([Atom | Atoms], PatList, [Atom | Matches], NoMatches)
	:-
	mpat(PatList, Atom),
	!,
	match_pat(Atoms, PatList, Matches, NoMatches).
match_pat([Atom | Atoms], PatList, Matches, [Atom | NoMatches])
	:-
	match_pat(Atoms, PatList, Matches, NoMatches).

%% Handle only a special case now; need general reg exp:
%%mpat(PatList, Atom)
mpat([HeadAtm, '*'], Atom)
	:-
	atom_length(HeadAtm, N),
	sub_atom(Atom,1,N,HeadAtm).

bang_apart(File, PartsLessLast)
	:-
	asplit(File, 0'., Left, Right),
	!,
	bang_apart(Right, RestPartsLessLast),
	(RestPartsLessLast = [] ->
		PartsLessLast = [Left]
		;
		PartsLessLast = [Left, '.' | PartsLessLast]
	).

bang_apart(File, []).

subtree_names([], []).
subtree_names([ST | SubTrees], [SN | SubTreeNames])
	:-
	stname(ST, SN),
	subtree_names(SubTrees, SubTreeNames).

stname(ST-_, ST)
	:-
	atom(ST), !.
stname(ST-_, SN)
	:-
	dmember(name=SN, ST), !.
stname(ST, ST)
	:-
	atom(ST), !.
stname(ST, SN)
	:-
	dmember(name=SN, ST), !.

subdir_rules([], MFStr).
subdir_rules([Name | SubTreeNames], MFStr)
	:-
	printf(MFStr, '\n%t:\n\t(cd %t ; make all)\n\n',[Name,Name]),
	subdir_rules(SubTreeNames, MFStr).

endmod.
