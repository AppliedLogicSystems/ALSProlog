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

export tcj/0.
tcj :-
	cfg_img(hitk).

/*************
export tci/0.
tci :-
	cfg_img(hitk, [[name=accsys],
				[name=tiff,
				 libs = ['/mailbox3/Tiff/tiff/libtiff/libtiff.a'],
				 ofiles = ['/mailbox3/Tiff/intf/showTIFF.o']
				],
				[name=motif]
				  ],

			[
				default_top = '/mailbox3',
				img_path = '/mailbox3', 
				srcdir	= '/mailbox3/alsp_src',
		 		libsdir	= '/mailbox3/als_libs',
		 		probld	= '/mailbox3/builds',
		 		protype	= natv
			]	
		).
*************/

export cfg_img/1.
cfg_img(ImgName)
	:-
	'$getenv'('ALSTOP', ALSTOPStr),
	name(ALSTOP, ALSTOPStr),
	cfg_img(ImgName, [default_top = ALSTOP] ).


export cfg_img/2.
cfg_img(ImgName, InitOptions)
	:-
	dmember(default_top = ALSTOP, InitOptions),
	extendPath(ALSTOP, als_libs, ALSLIBS),
	extendPath(ALSLIBS, images, IMGLIB),
	extendPath(IMGLIB,ImgName,ImgInfo),
	open(ImgInfo,read,IIStrm,[]),
	read_term(IIStrm,CFGIMGInfo,[]),
	close(IIStrm),
		%% CFGIMGInfo = cfg_img(ImgName, LinkLibs, Options)
		%% FIX Later: Make it merge InitOptions with the Options
		%% just read, given things from InitOpions priority;
	call(CFGIMGInfo).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% BUILDING AN ARBITRY "IMAGE" DIRECTORY
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*!---------------------------------------------------------------
 | cfg_img/3
 | cfg_img(ImgName, LinkLibs, Options)
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
 |
 |	FIX:  * Should correctly determine whether or not a natv version
 |			of the appropriate Arch_OS exists, and if not, gracefully
 |			fall back on the port version.
 |
 *!--------------------------------------------------------------*/

export cfg_img/3.
cfg_img(ImgName, LinkLibs, Options)
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
	general_os(Arch, OSVar, _, GOS), 
	existsmake_subdir(ArchOS,IMPath/ImgName),
	change_cwd(ArchOS),

	check_default(Options, protype, port, PROTYPE),
	check_default(Options, probld, 
					PROBLD^extendPath(DefltTop, builds, PROBLD), PROBLD),
	extendPath(PROBLD,ArchOS,AOSDir),
	catenate('bld-',PROTYPE,PROSYSNAME),
	extendPath(AOSDir,PROSYSNAME,InitPROSYSDir),
	(exists_file(InitPROSYSDir) ->
		PROSYSDir = InitPROSYSDir
		;
		printf('Warning!! Directory %t does not exist!\n',[InitPROSYSDir]),
		(PROSYSNAME = natv ->
			extendPath(AOSDir,'bld-port',PROSYSDir),
			(exists_file(PROSYSDir) ->
				true
				;
				printf('!!!ERROR: Neither bld-natv nor bld-port exists...ABORTING!!!\n\n',[]),
				fail
			),
			printf('Warning!! Switching to bld-port (portable ALS) library\n',[])
			;
			printf('!!!ERROR: %t \\= bld-natv and %t doesn\'t exist...ABORTING!!!\n\n',[])
		)
	),
	pathPlusFile(PROSYSDir, 'alspro.a', PROLIB),

		%% Make this "link or copy: -- and independent of OS:"
	catenate(['ln -s ',PROBLD,'/',Arch,'_',OSVar,'/bld-',PROTYPE,'/alsdir .'], 
				LNBLDALSDIR),
	system(LNBLDALSDIR),

		%% Directory where component libs are (generally) kept:
	check_default(Options, libsdir, 
					LIBSDIR^extendPath(DefltTop, als_libs, LIBSDIR), LIBSDIR),

	(dmember(srcdir=ALSSRC, Options) ->
		extendPath(ALSSRC,pconfig,PCONFIGPath),
		pathPlusFile(PCONFIGPath, 'makeimg.in', InitImgMakefile),
		Extra = [srcdir=ALSSRC]
		;
		builtins:sys_searchdir(ALSDIRPath),
		extendPath(ALSDIRPath,linking,LinkingPath),
		pathPlusFile(LinkingPath, 'makeimg.in', InitImgMakefile0),
		(exists_file(InitImgMakefile) ->
			InitImgMakefile = InitImgMakefile0,
			Extra = [linking=LinkingPath]
			;
			extendPath(DefltTop, alsp_src, ALSSRC), 
			extendPath(ALSSRC,pconfig,PCONFIGPath),
			pathPlusFile(PCONFIGPath, 'makeimg.in', InitImgMakefile),
			Extra = [srcdir=ALSSRC]
		)
	),

	open('Makefile',write,OutS,[]),
	printf(OutS,'#\n# Makefile for the %t image\n#\n\n',[ImgName]),

StdLibs = ' -lnsl -lm',

	write_make_eqns([
		'ARCH'			= Arch,
		'OS' 			= OSVar,
		'GOS' 			= GOS,
		probld 			= PROBLD,
		protype			= PROTYPE,
		prolib			= PROLIB,
		'TSYSN' 		= ImgName,
		'STDLIBS'		= StdLibs
		| Extra   ], OutS),

append(Extra, Options, XOpts),

	img_makefile(LinkLibs,0,ArchOS,PROBLD,LIBSDIR,XOpts,OutS, 
			LinkList, Lib_ls, TopOFILES, Inits, Cfgs, Profls, XLines),

	make_list_var(LinkList, 'CPTLIBS', OutS),
	make_list_var(TopOFILES, 'XOBJS', OutS),
	make_list_var(Profls, 'PROFLS', OutS),
	make_list_var(Lib_ls, 'SysLibs', OutS),

	open(InitImgMakefile, read, MSrc, []),
	copy_stream_nl(MSrc, OutS, GOS),
	close(MSrc),

	printf(OutS,'\n$(TSYSN)_basis : $(TOPOBJS) $(PROLIB) $(CPTLIBS)\n',[]),
	printf(OutS,'\techo starting $(TSYSN)_basis\n', 	[]),
	printf(OutS,'\t$(CC) -o $(TSYSN)_basis $(TOPOBJS) \\\n', 	[]),
	printf(OutS,'\t\t$(PROLIB)   \\\n', 			[]),

	write_link_elts(LinkList, OutS),
	printf(OutS,'\t\t$(SysLibs) \\\n',[]),
	printf(OutS,'\t\t$(STDLIBS)\n\n',[]),

	printf_list(XLines, OutS),
	close(OutS),

	pi_files(Inits, Cfgs),

	change_cwd(OrigDir).

	/*---------------------------------------------------------------------------
	 | img_makefile/13
	 | img_makefile(LinkLibs,Cntr,ArchOS,PROBLD,LIBSDIR,Options,OutS, 
	 |				LinkList, Lib_ls, TopOFiles, Inits, Cfgs, XLines)
	 | img_makefile(+,+,+,+,+,+,+, -, -, -, -)
	 |
	 | - writes comp lib info into Makefile and creates LinkList & Inits
	 |
	 |	+LinkLibs:	List of component library descriptions;
	 |	+Cntr:		Counter used in generated output;
	 |	+ArchOS:	Architecture+OS;
	 |	+PROBLD:	ALS Prolog builds directory;
	 |	+LIBSDIR:	Directory containing (prev. built) libraries & interfaces;
	 |	+Options:	The original input options list
	 |	+OutS:		The output stream (to the Makefile)
	 |	-LinkList:	List of lists of (paths to) (C) libs to be linked into image;
	 |	-Lib_ls:	List of lists of system (-lxxx) libraries to include;
	 |	-TopOFiles:	List of lists of (paths to) .o files to be linked into image;
	 |	-Inits:		List of init expressions for libraries to be put in pimain.c
	 |	-Cfgs:		List of lists of misc expressions for inclusion in pimain by pi_cfg.h
	 |	-Profls:	List of lists of prolog files needed to be loaded in final image
	 |	-XLines:	List of lists of misc extra Makefile lines
	 *--------------------------------------------------------------------------*/

img_makefile([],_,_,_,_,_,_,_, [], [], [], [], [], []).
img_makefile([CompLib | LinkLibs], CurN,ArchOS, PROBLD, LIBSDIR, Options, OutS, 
				[CLI | LinkList], [L_ll | Lib_ls], [ OFs | OFList], [CLInit | Inits], 
				[CLCfg | Cfgs], [CLPfls | Profls], [XLines | RestXLines])
	:-
	img_cmp_make(CompLib, CurN,ArchOS, PROBLD, LIBSDIR, Options, OutS, 
					CLI, L_ll, OFs, CLInit, CLCfg, CLPfls, XLines),
	NextN is CurN + 1,
	img_makefile(LinkLibs,NextN,ArchOS,PROBLD, LIBSDIR, Options,OutS, 
					LinkList, Lib_ls, OFList, Inits, Cfgs, Profls, RestXLines).

	/*---------------------------------------------------------------------------
	 |	img_cmp_make/11
	 |	img_cmp_make(CompLib, Cntr, ArchOS, PROBLD, LIBSDIR, Options, OutS, 
	 |				 CLI, L_ll, OFs, CLInit, CLCfg, Profls, XLines)
	 |	img_cmp_make(+, +, +,+, +, +, +, -, -, -)
	 |
	 |	+CompLib:	Component library description;
	 |	+Cntr:		Counter used in generated output;
	 |	+ArchOS:	Architecture+OS;
	 |	+PROBLD:	ALS Prolog builds directory;
	 |	+LIBSDIR:	Directory containing (prev. built) libraries & interfaces;
	 |	+Options:	The original input options list
	 |	+OutS:		The output stream (to the Makefile)
	 |	-CLI:		List of full names, with paths, of the Clibs in this component, 
	 |					to be linked into image;
	 |	-L_ll:		List of atoms describing system libraries (-lxxx) to be included;
	 |	-OFs:		List of (paths to) .o files to be linked into image;
	 |	-CLInit:	Init expression for pimain.c for this library;
	 |	-Cfgs:		List of (possible) misc expressions for inclusion in pimain by pi_cfg.h
	 |	-Profls:	List of (possible) prolog files needed to be loaded in final image
	 |	-XLines:	List of misc extra Makefile lines
	 *--------------------------------------------------------------------------*/
	%% Explicit description of what to include & where to get it:
img_cmp_make(CompLib, CurN, ArchOS, PROBLD, LIBSDIR, Options, OutS, 
				CLI, L_ll, OFs, CLInit, CLCfg, Profls, XLines)
	:-
	dmember(libs=CLI, CompLib),
	!,
	check_default(CompLib,syslibs,[], L_ll),
	dmember(name=CmpLibName, CompLib),
	check_default(CompLib,init,CLInit^catenate(CmpLibName,'_init();',CLInit), CLInit),
	check_default(CompLib, ofiles, [], OFs),
	check_default(CompLib, profls, [], Profls),
	check_default(CompLib, cfg, [], CLCfg),
	check_default(CompLib, xlines, [], XLines).

	%% Lib comp. is a window system library
img_cmp_make(CompLib, CurN, ArchOS, PROBLD, LIBSDIR, Options, OutS, 
				CLI, L_ll, OFs, CLInit, CLCfg, Profls, XLines)
	:-
	dmember(name=CmpLibName, CompLib),
	winsystems_for(ArchOS, WSL),
	dmember(CmpLibName, WSL),
	!,
	extendPath(PROBLD,ArchOS, AOSDir),
	extendPath(AOSDir,'bld-wins',BWinsDir),
	extendPath(BWinsDir, CmpLibName, WSDir),
	catenate(CmpLibName,'interf.a',WSLibName),
	pathPlusFile(WSDir,WSLibName,CmpLib),

	ws_vars(CmpLibName, ArchOS, WSHeaderLines),
	dmember('$(WIN)LIBS' = LibsLine, WSHeaderLines),
	dmember('ADDL_LIBS' = AddlLibsList, WSHeaderLines),
	prefix_dir(AddlLibsList, WSDir, XAddlLibsList),
	flatten_to_atom(XAddlLibsList, AddlLibsListAtom),

%	extendPath(WSDir, AddlLibsLine, FullAddlLibs),
	L_ll = [LibsLine],


%	CLI = [CmpLib, FullAddlLibs],
	CLI = [CmpLib, AddlLibsListAtom],


	dmember('ADDL_CS' = ACsList0, WSHeaderLines),
	catenate(CmpLibName, 'aux.c', AuxFile),
	ACsList = [AuxFile | ACsList0],
	(dmember(srcdir=SRCDIR, Options) ->
		extendPath(SRCDIR,wins,BWDir),
		extendPath(BWDir,src,WinSrcDir)
		;
		dmember(linking=WinSrcDir, Options)
	),
	deps_and_ofs(ACsList, WinSrcDir, AddlCOFs, AddlCODeps),
	XLines = AddlCODeps,

	dmember('ADDL_INITS' = AInitsList0, WSHeaderLines),

	check_default(CompLib,init,CLInit0^catenate(CmpLibName,'_init();',CLInit0), CLInit0),
	CLInit1 = [CLInit0 | AInitsList0],
	cat_together_seplines(CLInit1, CLInit),



	check_default(CompLib, ofiles, AddlCOFs, OFs),

	dmember('CFG' = DefaultCfg, WSHeaderLines),
	check_default(CompLib, cfg, DefaultCfg, CLCfg),

	dmember('ADDL_PROFS' = AddlProfsList, WSHeaderLines),
	extendPath(WSDir,'*.pro',AProfls),
		%% BProfls will be an atom (quoted), and
		%% Any necessary paths will be included, and
		%% Addl profs is normally only one file:
	check_default(CompLib, profls, '', BProfls),

	prefix_dir(AddlProfsList, WSDir, XAddlProfList),
	cat_together_spaced([AProfls, BProfls | XAddlProfList], ProflsStr),
	Profls = [ProflsStr].


	%% Lib comp. is built using "standard" ALS machinery & organization:
img_cmp_make(CompLib, CurN, ArchOS, PROBLD, LIBSDIR, Options, OutS, 
				CLI, L_ll, OFs, CLInit, CLCfg, Profls, XLines)
	:-
	dmember(name=CmpLibName, CompLib),
	extendPath(LIBSDIR, CmpLibName, LibBld),
	extendPath(LibBld,ArchOS,LBAOS),
	extendPath(LBAOS,lib,LBAOSl),
	lib_extension(OS,LExt),
	filePlusExt(CmpLibName,LExt,RawLibFileName),
	pathPlusFile(LBAOSl,RawLibFileName,RawLibFile),
	
	extendPath(LBAOS,intf,LBAOSi),
	catenate(CmpLibName,interf,LIF),
	filePlusExt(LIF,LExt,IntfFileName),
	pathPlusFile(LBAOSi,IntfFileName,IntfLibFile),

	CLI = [IntfLibFile, RawLibFile],
	check_default(CompLib,syslibs,[], L_ll),
	check_default(CompLib,init,CLInit^catenate(CmpLibName,'_init();',CLInit), CLInit),
	check_default(CompLib, ofiles, [], OFs),

	extendPath('..',addlp,UpToLBAOS),
	extendPath(LBAOS,UpToLBAOS,AddlpDir),
	check_default(CompLib, profls, 
					Profls^((files(AddlpDir,'*.pro',RawProfls), 
								(cfgimg:prefix_dir(RawProfls, AddlpDir, Profls))) ),
					Profls),

	check_default(CompLib, cfg, [], CLCfg),
	check_default(CompLib, xlines, [], XLines).

write_link_elts([], OutS).
write_link_elts([CLL | LinkList], OutS)
	:-
	write_cl_elts(CLL,OutS),
	write_link_elts(LinkList, OutS).

write_cl_elts([],OutS).
write_cl_elts([L | CLL],OutS)
	:-
	printf(OutS,'\t\t%t \\\n',[L]),
	write_cl_elts(CLL,OutS).

write_inits([], PISt).
write_inits([Init | Inits], PISt)
	:-
	printf(PISt,'\t%t\n',[Init]),
	write_inits(Inits, PISt).

pi_files(Inits, Cfgs)
	:-
	open('pi_init.c',write,PISt,[]),
	printf(PISt,'pi_init()\n{\n',[]),
	write_inits(Inits, PISt),
	printf(PISt,'}\n',[]),
	close(PISt),

	append(Cfgs, Cfgs0),
	cat_together_seplines(Cfgs0, CfgAtm),
	open('pi_cfg.h',write,PICFGSt,[]),
	printf(PICFGSt,'%t\n',[CfgAtm]),
	close(PICFGSt).

existsmake_subdir(Name,Path)
	:-
	exists_file(Name),
	!,
	als_advise('%t exists...\n', [Path/Name]).

existsmake_subdir(Name,Path)
	:-
	als_advise('Creating %t ...\n', [Path/Name]),
	make_subdir(Name, 511).

write_make_eqns([], OutS).
write_make_eqns([Left=Right | RestEqns], OutS)
	:-
	printf(OutS,'%t\t= %t\n',[Left,Right]),
	write_make_eqns(RestEqns, OutS).

	%% Assume ListList is a list of lists:
make_list_var([], VarName, OutS) :-!.
make_list_var(ListList, VarName, OutS)
	:-
	append(ListList, FlatList),
	FlatList = [First| Rest],
	printf(OutS, '\n%t = %t ', [VarName, First]),
	make_dep_var(Rest, OutS).

make_dep_var([], OutS)
	:-
	printf(OutS, '\n\n', [Lib]).
make_dep_var([Lib | LinkList], OutS)
	:-
	printf(OutS, '\\\n\t%t ', [Lib]),
	make_dep_var(LinkList, OutS).

/*
cat_together_seplines([], '').
cat_together_seplines([Item | Rest], Result)
	:-
	cat_together_seplines(Rest, RestResult),
	catenate([Item, '\n', RestResult], Result).
*/

/*
cat_together_spaced([], '').
cat_together_spaced([Item | Rest], Result)
	:-
	cat_together_spaced(Rest, RestResult),
	catenate([Item, ' ', RestResult], Result).

prefix_dir([], _, []).
prefix_dir([Item | List], WSDir, [XItem | XList])
	:-
	extendPath(WSDir, Item, XItem),
	prefix_dir(List, WSDir, XList).

deps_and_ofs([], WinSrcDir, [], []).
deps_and_ofs([CFl | ACsList], SrcDir, [OFl | AddlCOFs], [Dep1,Dep2 | AddlCODeps])
	:-
	extendPath(SrcDir, CFl, FullCF),
	asplit(CFl, 0'., File, Ext),
	filePlusExt(File, o, OFl),
	catenate([OFl,': ',FullCF], Dep1),
	catenate('\t$(CC) -c $(CPPFLAGS) $(CFLAGS) $(INTF_CFLAGS) ',
		  FullCF, Dep2),
	deps_and_ofs(ACsList, SrcDir, AddlCOFs, AddlCODeps).

printf_list([], _).
printf_list([[] | XLines], OutS)
	:-!,
	printf_list(XLines, OutS).
printf_list([Line | XLines], OutS)
	:-
	(Line = [_|_] ->
		printf_list(Line, OutS)
		;
		printf(OutS, '%t\n',[Line])
	),
	printf_list(XLines, OutS).

endmod.
