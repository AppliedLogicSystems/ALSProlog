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
 |	Generally assumes that it is linking (interfaced) libraries
 |	with directories setup by mklink.
 |
 |	If the image in which this program was running was started with
 |	the switch
 |		-ws WinSys
 |	then the image is configured for the window system WinSys;
 |	In the absence of the switch or other determining data, the
 |	default configuration for WinSys is motif.
 *==================================================================*/
module il_aliases.

:- dynamic(alias/2).
export alias/2.

endmod.


module cfgimg.
use sconfig.
use il_aliases.

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
	extendPath(ALSLIBS, images, IMGSCAT),
	extendPath(IMGSCAT,ImgName,ImgInfo),
	open(ImgInfo,read,IIStrm,[]),
	read_term(IIStrm,SrcCFGIMGInfo,[]),
	close(IIStrm),
		%% CFGIMGInfo = cfg_img(ImgName, LinkLibs, Options)
		%% FIX Later: Make it merge InitOptions with the Options
		%% just read, given things from InitOpions priority;
	SrcCFGIMGInfo = cfg_img(ImgName, LinkLibs, SrcOptions),
	merge_plists([imgcat=IMGSCAT | InitOptions], SrcOptions, CfgOptions0),

	get_cmdline_vals(SwitchVals),
	(dmember(['-ws',WinSys], SwitchVals) ->
		merge_plists([ws=WinSys], CfgOptions0, CfgOptions)
		;
		CfgOptions = CfgOptions0
	),
	CFGIMGInfo = cfg_img(ImgName, LinkLibs, CfgOptions),
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
		%% Does default_top exist?
	check_default(Options, default_top, OrigDir, DefltTop),
	exists_or_error(DefltTop),

		%% Does img_path exist?
	check_default(Options, img_path, DefltTop, IMPath), 
	exists_or_error(IMPath),
		%% Change to IMPath and make ImgName subdir, if necessary:
	change_cwd(IMPath),
	existsmake_subdir(ImgName,IMPath),
	change_cwd(ImgName),

		%% Get directory where component libs are (generally) kept:
	check_default(Options, libsdir, 
					LIBSDIR^extendPath(DefltTop, als_libs, LIBSDIR), LIBSDIR),

	
	(dmember(imgcat=IMGSCAT, Options) ->
		true
		;
		extendPath(LIBSDIR, images, IMGSCAT)
	),
		
		%% Load aliases (if it exists):
	pathPlusFile(IMGSCAT,aliases,ALIASES),
	(exists_file(ALIASES) ->
		consult(ALIASES)
		;
		true
	),
		%% Now process an aliases = ... entry from Options,
		%% if it exists:
	(dmember(aliases = LclAliases, Options) ->
		install_aliases(LclAliases)
		;
		true
	),

		%% Determine architecture & os we are running under:
	als_system(SysVals),
	check_default(Options, arch, 
					Arch^dmember(processor=Arch, SysVals), Arch),
	check_default(Options, os_variation, 
					OSVar^dmember(os_variation=OSVar, SysVals), OSVar),

	check_ws_name(Options, WSName),

		%% Setup arch_os subdir and change to it:
	catenate([Arch,'_',OSVar], ArchOS),
	catenate([Arch,'_',OSVar,'_',WSName], ArchOSWS),
	general_os(Arch, OSVar, _, GOS), 
	(WSName = no_wins ->
		existsmake_subdir(ArchOS,IMPath/ImgName),
		change_cwd(ArchOS)
		;
		existsmake_subdir(ArchOSWS,IMPath/ImgName),
		change_cwd(ArchOSWS)
	),

	find_als_image(Options, DefltTop, ArchOS, PROTYPE, PROBLD, PROLIB),

		%% Make an alsidr accessible in this dir:
	attach_alsdir(PROBLD,ArchOS,PROTYPE),

		%% Get path to source initial image makefile:
	img_makefile_etc(Options, ALSSRC, InitImgMakefile, Extra),

	standard_libs(OSVar, GOS, StdLibs),

		%% Now start writing the makefile we are generating:
	open('makefile',write,OutS,[]),
	printf(OutS,'#\n# Makefile for the %t image\n#\n\n',[ImgName]),

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

		%% extended options for dealing with link libraries:
	append(Extra, Options, XOpts),

	img_makefile(LinkLibs,0,ArchOS,PROBLD,LIBSDIR,XOpts,OutS, 
			LinkList, Lib_ls, TopOFILES, Inits, Cfgs, LibProfls, 
			InitSYSL, XLines),

		%% Add on any "pure" prolog components to the image:
	extend_profl(LibProfls, Options, WSName, DefltTop, Profls),

		%% These combine items from the various individual lib comps:
	make_list_var(LinkList, 'CPTLIBS', OutS),
	make_list_var(TopOFILES, 'XOBJS', OutS),
	make_list_var(Profls, 'PROFLS', OutS),
	make_list_var(Lib_ls, 'SysLibs', OutS),

	setOf(LF, L^(member(L,InitSYSL), member(LF, L)), LFs),
	imgopts(LFs, Options, OutS),

		%% Now copy over the initial makefile; note that this
		%% contains the lines for making the (ultimate) target
		%% $(TSYSN) in terms  of $(TSYSN)_basis which is defined
		%% below:
	open(InitImgMakefile, read, MSrc, []),
	copy_stream_nl(MSrc, OutS, GOS),
	close(MSrc),

		%% Write the initial dependency line(s) and primary link line
		%% for $(TSYSN)_basis, including the PROLIB line:
	printf(OutS,'\n$(TSYSN)_basis : $(TOPOBJS) $(PROLIB) $(CPTLIBS)\n',[]),
	printf(OutS,'\techo starting $(TSYSN)_basis\n', 	[]),
	printf(OutS,'\t$(CC) -o $(TSYSN)_basis $(TOPOBJS) \\\n', 	[]),
	printf(OutS,'\t\t$(PROLIB)   \\\n', 			[]),

		%% Now write the link lines corresponding to the
		%% individual library components:
	write_link_elts(LinkList, OutS),
	printf(OutS,'\t\t$(SysLibs) \\\n',[]),
	printf(OutS,'\t\t$(STDLIBS)\n\n',[]),

		%% Print out any required additional lines (e.g., dependencies)
		%% and close the output makefile:
	printf_list(XLines, OutS),
	close(OutS),

		%% Setup the files pi_init.c and pi_cfg.h:
	pi_files(Inits, Cfgs),
		%% Go back to our starting directory:

	change_cwd(OrigDir).

	/*---------------------------------------------------------------------------
	 | img_makefile/15
	 | img_makefile(LinkLibs,Cntr,ArchOS,PROBLD,LIBSDIR,Options,OutS, 
	 |				LinkList, Lib_ls, TopOFiles, Inits, Cfgs, LibProfls, 
					SysLibFs, XLines)
	 | img_makefile(+,+,+,+,+,+,+, -, -, -, -, -, -)
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
	 |	-SysLibFs:	List of lists of system library (prolog) files to be loaded in final image
	 |	-XLines:	List of lists of misc extra Makefile lines
	 *--------------------------------------------------------------------------*/

img_makefile([],_,_,_,_,_,_,_, [], [], [], [], [], [], []).
img_makefile([CompLib | LinkLibs], CurN,ArchOS, PROBLD, LIBSDIR, Options, OutS, 
				[CLI | LinkList], [L_ll | Lib_ls], [ OFs | OFList], [CLInit | Inits], 
				[CLCfg | Cfgs], [CLPfls | Profls], 
				[SYSL | SysLibFs], [XLines | RestXLines])
	:-
	img_cmp_make(CompLib, CurN,ArchOS, PROBLD, LIBSDIR, Options, OutS, 
					CLI, L_ll, OFs, CLInit, CLCfg, CLPfls, SYSL, XLines),
	NextN is CurN + 1,
	img_makefile(LinkLibs,NextN,ArchOS,PROBLD, LIBSDIR, Options,OutS, 
					LinkList, Lib_ls, OFList, Inits, Cfgs, Profls, 
					SysLibFs, RestXLines).

	/*---------------------------------------------------------------------------
	 |	img_cmp_make/15
	 |	img_cmp_make(CompLib, Cntr, ArchOS, PROBLD, LIBSDIR, Options, OutS, 
	 |				 CLI, L_ll, OFs, CLInit, CLCfg, Profls, SYSL, XLines)
	 |	img_cmp_make(+, +, +, +, +, +, +, -, -, -, -, -, -, -)
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
	 |	-SYSL:		List of system library (prolog) files to be loaded in final image
	 |	-XLines:	List of misc extra Makefile lines
	 *--------------------------------------------------------------------------*/
	%% Explicit description of what to include & where to get it:
img_cmp_make(CompLib, CurN, ArchOS, PROBLD, LIBSDIR, Options, OutS, 
				CLI, L_ll, OFs, CLInit, CLCfg, Profls, [], XLines)
	:-
	dmember(libs=CLI, CompLib),
	!,
	check_default(CompLib,syslibs,[], L_ll),
	dmember(name=CmpLibName, CompLib),
	check_default(CompLib,init,CLInit^catenate(CmpLibName,'_init();',CLInit), CLInit),
	check_default(CompLib, ofiles, [], InitOFs),
	fix_paths(InitOFs, OFs),
	check_default(CompLib, profls, [], Profls),
	check_default(CompLib, cfg, [], CLCfg),
	check_default(CompLib, xlines, [], XLines).

	%% Lib comp. is a window system library
img_cmp_make(CompLib, CurN, ArchOS, PROBLD, LIBSDIR, Options, OutS, 
				CLI, L_ll, OFs, CLInit, CLCfg, Profls, [], XLines)
	:-
	check_for_windows(CompLib, ArchOS, Options, CmpLibName),
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

	L_ll = [LibsLine],
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

	ws_addl_profls(WSHeaderLines,WSDir,CompLib,CmpLibName,LIBSDIR,Profls).

	%% Lib comp. is built using "standard" ALS machinery & organization:
img_cmp_make(CompLib, CurN, ArchOS, PROBLD, LIBSDIR, Options, OutS, 
				CLI, L_ll, OFs, CLInit, CLCfg, Profls, [], XLines)
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


check_for_windows(CompLib, ArchOS, Options, CmpLibName)
	:-
	dmember(name=windows, CompLib),
	!,
	(dmember(ws=CmpLibName, Options) ->
		true
		;
		builtins:als_system(SysList),
		((dmember(wins=CmpLibName, SysList),CmpLibName \= nowins) ->
			true
			;
			winsystems_for(ArchOS, WSL),
			dmember(CmpLibName, WSL)
		)
	).
	/*
	builtins:als_system(SysList),
	((dmember(wins=CmpLibName, SysList),CmpLibName \= nowins) ->
		true
		;
			;
			winsystems_for(ArchOS, WSL),
			dmember(CmpLibName, WSL)
		)
	).
	*/

check_for_windows(CompLib, ArchOS, Options, CmpLibName)
	:-
	dmember(name=CmpLibName, CompLib),
	winsystems_for(ArchOS, WSL),
	dmember(CmpLibName, WSL).

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

	(Cfgs = [] ->
		CfgAtm = '#define WIN_STR NO_WIN_STR'
		;
		append(Cfgs, Cfgs0),
		cat_together_seplines(Cfgs0, CfgAtm)
	),
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
*/

fix_paths(Dir+BareOFs, OFs)
	:-!,
	prefix_dir(BareOFs, Dir, OFs).
	
fix_paths(OFs, OFs).

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

exists_or_error(FileOrDir)
	:-
	exists_file(FileOrDir),
	!.
exists_or_error(FileOrDir)
	:-
	als_advise('Error: Path %t does not exist.\n', [FileOrDir]),
	!,
	fail.

find_als_image(Options, DefltTop, ArchOS, PROTYPE, PROBLD, PROLIB)
	:-
		%% Determine desired type of image (native, threaded, byte):
	check_default(Options, protype, port, PROTYPE),
		%% Where are the build directories for ALS Prolog images:
	check_default(Options, probld, 
					PROBLD^extendPath(DefltTop, builds, PROBLD), PROBLD),

		%% Locate the desired ALS Prolog image, if it exists;
		%% If it doesn't, fall back to threaded (or byte):
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
	pathPlusFile(PROSYSDir, 'alspro.a', PROLIB).

attach_alsdir(PROBLD,ArchOS,PROTYPE)
	:-
		%% Make this "link or copy: -- and independent of OS:"
	catenate(['ln -s ',PROBLD,'/',ArchOS,'/bld-',PROTYPE,'/alsdir .'], 
				LNBLDALSDIR),
	system(LNBLDALSDIR).


	%% srcdir is specified, so get initial makefile
	%% from the source dir:
img_makefile_etc(Options, ALSSRC, InitImgMakefile, Extra)
	:-
	dmember(srcdir=ALSSRC, Options),
	!,
	extendPath(ALSSRC,pconfig,PCONFIGPath),
	pathPlusFile(PCONFIGPath, 'makeimg.in', InitImgMakefile),
	Extra = [srcdir=ALSSRC].

	%% srcdir is not specified, but there is a 
	%% linking directory in alsdir, and it contains
	%% a makeimg.in initial makefile:
img_makefile_etc(Options, ALSSRC, InitImgMakefile, Extra)
	:-
	builtins:sys_searchdir(ALSDIRPath),
	extendPath(ALSDIRPath,linking,LinkingPath),
	pathPlusFile(LinkingPath, 'makeimg.in', InitImgMakefile0),
	exists_file(InitImgMakefile0),
	!,
	InitImgMakefile = InitImgMakefile0,
	Extra = [linking=LinkingPath].

	%% Fallback default: try for als source directory
	%% residing beneath the default top directory:
img_makefile_etc(Options, ALSSRC, InitImgMakefile, Extra)
	:-
	('$getenv'('ALSTOP', ALSTOPStr) ->
		name(ALSTOP, ALSTOPStr)
		;
		dmember(default_top = ALSTOP, InitOptions)
	),
	extendPath(ALSTOP, alsp_src, ALSSRC), 
	extendPath(ALSSRC,pconfig,PCONFIGPath),
	pathPlusFile(PCONFIGPath, 'makeimg.in', InitImgMakefile),
	exists_file(InitImgMakefile),
	Extra = [srcdir=ALSSRC].



	%% Unix default:
standard_libs(_, unix, StdLibs)
	:-
	StdLibs = ' -lnsl -lm'.

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% Adds on (in front) of LibProfls  all groups (if any)
		%% of "additional" prolog source files required by the 
		%% Options:
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%% Options specifies additional files via eqn
	%% 	aprofls = PFList
extend_profl(LibProfls, Options, WSName, DefltTop, Profls)
	:-	
	dmember(aprofls=PFList, Options),
	!,
	xtnd_pfls(PFList, DefltTop, WSName, LibProfls, Profls).

	%% No specified additional files:
extend_profl(LibProfls, DefltTop, _, _, LibProfls).

	%% Recurse down PFList to pick up the files specified
	%% by each element:
xtnd_pfls([], DefltTop, WSName, Profls, Profls).
xtnd_pfls([PflItem | PFList], DefltTop, WSName, AccumProfls, Profls)
	:-
	add_pfls(PflItem, DefltTop, WSName, AccumProfls, NewAccumProfls),
	xtnd_pfls(PFList, DefltTop, WSName, NewAccumProfls, Profls).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Add on the files required by a single element of PFList:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_pfls(SpecPath+InitFilesSpec, DefltTop, WSName, AccumProfls, NewAccumProfls)
	:-!,
	organize_path(SpecPath, DefltTop, WSName, InitPath),
	(InitFilesSpec = ('$ws'+FilesSpec) ->
		extendPath(InitPath,WSName,Path)
		;
		FilesSpec = InitFilesSpec,
		Path = InitPath
	),
	obtain_comp_files(FilesSpec, InitPath, Path, AddlFiles),
	NewAccumProfls = [AddlFiles | AccumProfls]. 
	
organize_path(SpecPath/ContPath, DefltTop, WSName, Path)
	:-!,
	organize_path(SpecPath, DefltTop, WSName, InitPath),
	(ContPath = '$ws' ->
		CP = WSName
		;
		CP = ContPath
	),
	extendPath(InitPath, CP, Path).

		%% Path spec is absolute - use as is:
organize_path(SpecPath, DefltTop, WSName, SpecPath)
	:-
	absolute_path(SpecPath),
	!.
	
organize_path(SpecPath, DefltTop, WSName, Path)
	:-
	il_aliases:alias(SpecPath, Path),
	!.

organize_path(SpecPath, DefltTop, WSName, Path)
	:-
	extendPath(DefltTop, SpecPath, Path).

	%% Given Path (which is absolute now), obtain the files:

	%% Read the list from the file named 'spec':
	%% Entry in spec is single list term (many lines ok, of course):
	%% [........].
obtain_comp_files(spec, InitPath, FilesPath, AddlFiles)
	:-!,
	pathPlusFile(InitPath, spec, SpecFile),
	open(SpecFile, read, RStr, []),
	read_term(RStr, SrcAddlFiles, []),
	close(RStr),
	prefix_dir(SrcAddlFiles, FilesPath, AddlFiles).
	
obtain_comp_files(files(FileName), InitPath, FilesPath, AddlFiles)
	:-!,
	pathPlusFile(InitPath, FileName, SpecFile),
	open(SpecFile, read, RStr, []),
	read_term(RStr, SrcAddlFiles, []),
	close(RStr),
	prefix_dir(SrcAddlFiles, FilesPath, AddlFiles).
	
	%% Got a list of filenames: must refer to files in
	%% this directory (Path):
obtain_comp_files(FilesSpec, InitPath, FilesPath, AddlFiles)
	:-
	FilesSpec = [_|_],
	!,
	prefix_dir(FilesSpec, FilesPath, AddlFiles).
	
	%% Got a simple atom; this is a single file in the 
	%% present directory (Path); of course, it could
	%% be a "loader" file consulting things, but that
	%% might be dangerous as far a path names go:
obtain_comp_files(FilesSpec, InitPath, FilesPath, AddlFiles)
	:-
	atom(FilesSpec),
	pathPlusFile(FilesPath, FilesSpec, XPath),
	AddlFiles = [XPath].


		%% MOVE THIS TO fsunix/fsdos/fsmac, etc:
		%% Unix version:
absolute_path(Path)
	:-
	sub_atom(Path, 1, 1, '/').

install_aliases([]).
install_aliases([AL | LclAliases])
	:-
	il_aliases:asserta(AL),
	install_aliases(LclAliases).

ws_addl_profls(WSHeaderLines,WSDir,CompLib,CmpLibName,LIBSDIR,Profls)
	:-
	dmember('ADDL_PROFS' = AddlProfsList, WSHeaderLines),
	extendPath(WSDir,'*.pro',AProfls),
		%% BProfls will be an atom (quoted), and
		%% Any necessary paths will be included, and
		%% Addl profs is normally only one file:
	check_default(CompLib, profls, '', BProfls0),
	(BProfls0 = '' ->
		BProfls = []
		;
		BProfls = [BProfls0]
	),
	prefix_dir(AddlProfsList, WSDir, XAddlProfList),

	extendPath(LIBSDIR, windows, LIBSWINSCAT),
	pathPlusFile(LIBSWINSCAT, CmpLibName, WSUpper),
	(exists_file(WSUpper) ->
		open(WSUpper, read, WSUStr, []),
		read_terms(WSUStr, WSUTms),
		close(WSUStr),
		dmember(dir=WSUDir, WSUTms),
		dmember(files=WSUFiles, WSUTms),
		prefix_dir(WSUFiles, WSUDir, UpperProfls0)
		;
		UpperProfls0 = []
	),
	(UpperProfls0 = [] ->
		UpperProfls1 = []
		;
		pathPlusFile(LIBSWINSCAT, gui, GUICat),
		(exists_file(GUICat) ->
			open(GUICat, read, GUIStr, []),
			read_terms(GUIStr, GUITms),
			close(GUIStr),
			dmember(dir=GUIDir, GUITms),
			dmember(files=GUIFiles0, GUITms),
			adjust_ws(GUIFiles0, CmpLibName, GUIFiles),
			prefix_dir(GUIFiles, GUIDir, UpperProfls1)
			;
			UpperProfls1 = []
		)
	),
	append([[AProfls], BProfls, XAddlProfList, 
			UpperProfls0, UpperProfls1],Profls).

imgopts(LFs, Options, OutS)
	:-
	((dmember(all,LFs) ; dmember(libs=all, Options) ) ->
		Lib = [libload(true)]
		;
		(dmember(libs=LibReq, Options) ->
			union(LFs, LibReq, LIBFs)
			;
			LIBFs = LFs
		),
		(LIBFs = [] ->
			Lib = []
			;
			Lib = [select_lib(IBFs)]
		)
	),
	(dmember(begin=Begin, Options) ->
		append(Lib, [start_goal(Begin)], ImgLine)
		;
		ImgLine = Lib
	),
	printf(OutS, 'IMGOPTS = %t\n', [ImgLine]).


check_ws_name(Options, WSName)
	:-
	dmember(ws=WSName, Options),
	!.

check_ws_name(Options, WSName)
	:-
	member(L,Options),
	dmember(name=Name,Options),
	check_ws_name0(Name, WSName),
	!.

%check_ws_name(Options, no_wins).
	%% Default is motif:
check_ws_name(Options, motif).

check_ws_name0(windows, WSName)
	:-!,
	determine_default_ws(WSName).
	
check_ws_name0(WSName, WSName).

adjust_ws([], _, []).
adjust_ws(['$ws'+F | GUIFiles0], CmpLibName, [FF | GUIFiles])
	:-
	extendPath(CmpLibName, F, FF),
	adjust_ws(GUIFiles0, CmpLibName, GUIFiles).
adjust_ws([F | GUIFiles0], CmpLibName, [F | GUIFiles])
	:-
	adjust_ws(GUIFiles0, CmpLibName, GUIFiles).

endmod.
