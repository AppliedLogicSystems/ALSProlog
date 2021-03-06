/*===================================================================
 |		pconfig.pro
 |	Copyright (c) 1994-6 Applied Logic Systems, Inc.
 |
 |	Prolog-driven windowing stage configuration during builds
 |
 |	This is even uglier than autoconf for two reasons:
 |	i)   autoconf doesn't handle things like where Motif1.2
 |		 lives, etc.
 |	ii)  autoconf only works on unix platforms, and we are handling
 |	 	 more than that.
 |	iii) we have to create files such as pi_init.c and pi_cfg.h.
 |
 |	Called after 'make all' has been executed in bld-port and
 |	bld-natv (if native code supported);  creates the bld-wins
 |	directory, together with appropriate subdirectories (e.g.,
 |	x, motif, etc.) and makefiles.
 |
 |	Author: Ken Bowen
 |	Date begun: 8 Aug 94  [Forever under construction??]
 *==================================================================*/

module pconfig.
use  mkdist.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Top-level entry to the prolog-level configuration process
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%------------------------------------------------------
	%  Main entry point:
	%------------------------------------------------------
export pconfig/0.

pconfig 
	:-
	acquire_options(SwitchInfo),
	abolish(cfg,2),
	build_base_config,
	abolish(lcl_config,1),
	setup_lcl_config,
	setup_wins(SwitchInfo).

	%------------------------------------------------------
	% Obtains command line objections and verifies existence of
	% a target window system, or obtains one from ws.tgt at top
	% level in the build directory;
	%------------------------------------------------------
acquire_options(SwitchInfo)
	:-
	get_cmdline_vals(SwitchVals),
	parse_switches(SwitchVals, SwitchInfo0),
	ws_opt_file(SwitchInfo0, SwitchInfo).

parse_switches([], []).
parse_switches([['-sd', SrcDir] | SwitchVals], SwitchInfo)
	:-!,
	assert(cl_srcdir(SrcDir)),
	parse_switches(SwitchVals, SwitchInfo).
parse_switches([['-tgtws', TgtWS] | SwitchVals], [tgtws=TgtWS | SwitchInfo])
	:-!,
	parse_switches(SwitchVals, SwitchInfo).
parse_switches([_ | SwitchVals], SwitchInfo)
	:-
	parse_switches(SwitchVals, SwitchInfo).

ws_opt_file(SwitchInfo, SwitchInfo)
	:-
	dmember(tgtws=_,SwitchInfo),
	!.

ws_opt_file(SwitchInfo0, [tgtws=TGTWS | SwitchInfo0])
	:-
	exists_file('ws.tgt'),
	open('ws.tgt',read,WSTS,[]),
	read(WSTS,TGTWS),
	!,
	close(WSTS).

ws_opt_file(SwitchInfo, SwitchInfo).

	%------------------------------------------------------

setup_lcl_config
	:-
	exists_file('local.cfg'),
	!,
	open('local.cfg', read, IS, []),
	read_terms(IS, LclFacts),
	close(IS),
	lcl_install(LclFacts).

setup_lcl_config
	:-
%	cfg(srcdir,SRCDIR),
	cl_srcdir(SRCDIR),
	cfg('ARCH',ARCH),
	cfg('SOS',SOS),
	catenate([ARCH,'-',SOS],DN),
	extendPath(SRCDIR,local,LP1),
	extendPath(LP1,DN,LP2),
	extendPath(LP2,'local.cfg',LocalFile),
	exists_file(LocalFile),
	!,
	open(LocalFile, read, IS, []),
	read_terms(IS, LclFacts),
	close(IS),
	lcl_install(LclFacts).

setup_lcl_config.

lcl_install([]).
lcl_install([Clause | LclFacts])
	:-
	assert(lcl_config(Clause)),
	lcl_install(LclFacts).

	%------------------------------------------------------
	% Initial accumulation of information; extract from:
	% 1. (xtr_cfg_info):
	%	 bld-natv/makefile, if this exists;
	%    else from bld-port/makefile;
	% 2. (read_config_h): config.h
	% 3. prologVersion from als_system list.
	%
	% Writes everything to cfg.pro and asserts everything:
	%------------------------------------------------------
build_base_config
	:-
	exists_file('cfg.pro'),
	(exists_file('config.h') ->
			%% cfg.pro is newer than config.h:
		comp_file_times('config.h','cfg.pro')
		;
			%% config.h not present:
		true
	),
	!,
	consult_to(pconfig, cfg).

build_base_config
	:-
	xtr_cfg_info('./',BaseInfo),
	read_config_h(CfgInfo),
	append(BaseInfo,CfgInfo,Info),
	als_system(SysList),
	dmember(prologVersion = PVer, SysList),


	open('cfg.pro',write,OutStr,[]),
	write_clauses(OutStr, [cfg(prologVersion,PVer) | Info]),
	close(OutStr),
	assert_all(Info).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Read the config.h file in the top-level build directory
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export read_config_h/1.
read_config_h(Items)
	:-
	open('config.h',read,InStr,[]),
	read_config_h(InStr, Items),
	close(InStr).

read_config_h(InStr, Items)
	:-
	get_line(InStr,Line),
	!,
	name(Line,LCs),
	disp_read_config_h(LCs,Items,InStr).

read_config_h(InStr,[]).

disp_read_config_h(LCs,Items,InStr)
	:-
	start_cmnt(LCs),
	!,
	disp_to_end_cmt(LCs,InStr),
	read_config_h(InStr,Items).

disp_read_config_h([],Items,InStr)
	:-!,
	read_config_h(InStr,Items).

disp_read_config_h(LCs,[Item | RestItems],InStr)
	:-
	cfg_process(LCs,Item),
	!,
	read_config_h(InStr, RestItems).

disp_read_config_h(LCs, Items, InStr)
	:-
	read_config_h(InStr, Items).

	%------------------------------------------------------
	%% Read across /* -- */ - C-sytle comments
	%------------------------------------------------------

to_end_cmt(InStr)
	:-
	get_line(InStr,Line),
	!,
	name(Line,LCs),
	disp_to_end_cmt(LCs,InStr).

to_end_cmt(InStr).

disp_to_end_cmt(LCs,InStr)
	:-
	end_cmnt(LCs), !.

disp_to_end_cmt(LCs,InStr)
	:-
	to_end_cmt(InStr).

start_cmnt(LCs)
	:-
	append(X,[0'/,0'* | Y], LCs), !.

end_cmnt(LCs)
	:-
	append(X,[0'*,0'/ | Y], LCs), !.

	%------------------------------------------------------
	%% Convert a string "#define <Symb> <Val>" to 
	%% a fact: '#define'('<Symb>', <PrologVal>)
	%------------------------------------------------------

cfg_process(LCs, '#define'(Expr, Value))
	:-
	append("#define",Tail1, LCs),
	strip_white(Tail1, Tail1a),
	asplit00(Tail1a, [0' , 0'	], Middle, Tail2),	% space, tab 
	strip_white(Tail2, Tail2a),
	strip_tail_white(Tail2a, Tail2b),
	name(Expr, Middle),
	!,
	name(Value, Tail2b).
cfg_process(LCs,Line).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Extract configuration information from 
	%% the Makefile in bld-natv (this should be there, even
	%% when the native version doesn't exist.
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export xtr_cfg_info/2.

xtr_cfg_info(BLD_Dir,Info)
	:-
	xtr_cfg_info(BLD_Dir, 
			["ARCH","OS","SOS","srcdir","CC","LIBS" ,
			 "X_CFLAGS","X_LIBS", "X_EXTRA_LIBS"], 
	 		Info).

xtr_cfg_info(BLD_Dir,TagsList, Info)
	:-
	slash2list(BLD_Dir, BLDDirList),
	append(BLDDirList,['bld-natv'],NTVDirList),
	rootPathFile('', NTVDirList, 'makefile', MakefileAtm),
	(exists_file(MakefileAtm) ->
		open(MakefileAtm,read,MFStrm,[])
		;
		append(BLDDirList,['bld-port'],PORTDirList),
		rootPathFile('', PORTDirList, 'makefile', PMakefileAtm),
		open(PMakefileAtm,read,MFStrm,[])
	),
	xtr_cfgi(TagsList, MFStrm, Info),
	close(MFStrm).


	%------------------------------------------------------
	%% Extract configuration information from a given Makefile
	%------------------------------------------------------

xtr_cfgi([], MFStrm, []) :-!.

xtr_cfgi(TagsList, MFStrm, Info)
	:-
	get_line(MFStrm, Line),
	!,
	name(Line, LCs),
	disp_xtr_cfgi(TagsList, LCs, MFStrm, Info).

xtr_cfgi(TagsList, MFStrm, Info).

disp_xtr_cfgi(TagsList, LCs, MFStrm, [cfg(Tag,Value) | RestInfo])
	:-
	match_tag(TagsList, LCs, (Tag,Value), RestTagsList),
	!,
	xtr_cfgi(RestTagsList, MFStrm, RestInfo).

disp_xtr_cfgi(TagsList, _, MFStrm, Info)
	:-
	xtr_cfgi(TagsList, MFStrm, Info).

match_tag(TagsList, LCs, (Tag,Value), RestTagsList)
	:-
	trial_mem(TG, TagsList,RestTagsList),
	append(TG, TailCs, LCs),
	strip_white(TailCs,[0'=,0' |ValCsa0]),
	strip_white(ValCsa0,ValCsa),
	dreverse(ValCsa, RValCsa),
	strip_white(RValCsa, RValCs),
	dreverse(RValCs, ValCs),
	name(Value, ValCs),
	!,
	name(Tag,TG).

trial_mem(TG, [TG | RestTagsList], RestTagsList).

trial_mem(TG, [Other | TailTagsList], [Other | RestTagsList])
	:-
	trial_mem(TG, TailTagsList, RestTagsList).

	/*----------------------------------------------------
	 |	Determining which bld dirs exist
	 *---------------------------------------------------*/

get_bld_dirs(BDList)
	:-
	ToCheck = ['bld-natv','bld-port','bld-byte'],
	get_bld_dirs(ToCheck,BDList).

get_bld_dirs([],[]).
get_bld_dirs([BD | ToCheck],[BD0 | BDList])
	:-
	exists_file(BD),
	pathPlusFile(BD,'alspro.a',ProLib),
	exists_file(ProLib),
	!,
	pathPlusFile('../..',BD,BD0),
	get_bld_dirs(ToCheck,BDList).

get_bld_dirs([BD | ToCheck],BDList)
	:-
	get_bld_dirs(ToCheck,BDList).

	/*----------------------------------------------------
	 *----------------------------------------------------
	 ||	 Setting up the bld-wins subdir
	 ||
	 ||  "Main" entry to most of the hard work; creates
	 ||  the bld-wins dir, and all the various subdirs,
	 ||  together with makefiles, etc.
	 *---------------------------------------------------*
	 *---------------------------------------------------*/

setup_wins(SwitchInfo)
	:-
	check_make_subdir('./bld-wins'),
	cfg('ARCH',ARCH),
	cfg('OS',OS),
			%% Use library info from: sconfig.pro:
	(dmember(tgtws=TGTWS, SwitchInfo) ->
		builtins:specif_winsystems_for(_,TGTWS,SubdirList)
		;
		winsystems_for(ARCH, OS, SubdirList)
	),
	(SubdirList = [] ->
		printf(user,'No win interface dirs to create! \n',[])
		;
		get_bld_dirs(BDList),
		change_cwd('./bld-wins'),
		make_each_subdir(SubdirList),

			% Path: <srcdir>/bld-natv/
		cfg(srcdir,BLD_NATV_SRC_PATH_Atm),

			% Path: <srcdir>/bld-natv/../wins/build
		extendPath(BLD_NATV_SRC_PATH_Atm,'wins/build',WINBLD ),

			% Path: <srcdir>/bld-natv/../wins/build/makefile.in
		pathPlusFile(WINBLD, 'makefile.in', WINMKPATH),

		general_os(ARCH,OS,GOS),
		interleave(SubdirList, ' ', TL),
		catenate(['LIBRARIES = ' | TL], Header),
		clean_lines(SubdirList, CleanLines),
		trans_xtnd_makefile(WINMKPATH, 
				    [Header | CleanLines], 
				    './makefile', GOS),

		subPath(BLD_NATV_SRC_PATH,BLD_NATV_SRC_PATH_Atm),

			%% Create each ws subdir & makefile:
		create_makefiles_and_subdirs(SubdirList, ARCH, OS, SwitchInfo,
									 BLD_NATV_SRC_PATH,BDList),
		change_cwd('..')
	),
	change_cwd('..').


	%
	% Simple creation of each subdir in a list:
	%
make_each_subdir([]).
make_each_subdir([Subdir | SubdirList])
	:-
	check_make_subdir(Subdir),
	make_each_subdir(SubdirList).

check_make_subdir(Subdir)
	:-
	exists_file(Subdir),
	!,
	printf(user,'Subdir %t exists\n',[Subdir]).
check_make_subdir(Subdir)
	:-
	make_subdir(Subdir),
	printf(user,'   Created subdir %t \n',[Subdir]).

	%%------------------------------------------------------------
	%%	Recurse down list, creating each individual windowing
	%%	makefile:
	%%------------------------------------------------------------
create_makefiles_and_subdirs( [], _, _, _, _,_).

create_makefiles_and_subdirs( [Subdir | Subdirs], ARCH, OS, SwitchInfo,
								BLD_NATV_SRC_PATH,BDList)
	:-
		% Path: <srcdir>/bld-natv/../wins/build/<Subdir>
	append(BLD_NATV_SRC_PATH,[wins,build,Subdir], BldSubdirPath),
	subPath(BldSubdirPath, BldSubdirPathAtm),

		% Path: <srcdir>/bld-natv/../wins/build
	append(BLD_NATV_SRC_PATH,[wins,build], BldPath),
	subPath(BldPath, BldPathAtm),

	general_os(ARCH,OS,GOS),

	create_wsi_makefile( Subdir, ARCH, OS, BLD_NATV_SRC_PATH, BDList,
				SwitchInfo, BldPathAtm, BldSubdirPathAtm, GOS),

	create_makefiles_and_subdirs( Subdirs, ARCH, OS,SwitchInfo, BLD_NATV_SRC_PATH,BDList).

	%%------------------------------------------------------------
	%% Build a given (windowing) 'build' subdirectory, including
	%% the makefile, the alsdir and tests subdirs, and the
	%% files:
	%%		tconfig.h   pi_cfg.h   pi_init.c
	%%------------------------------------------------------------
create_wsi_makefile( Subdir, ARCH, OS, BLD_NATV_SRC_PATH,BDList, 
					 SwitchInfo, BldPathAtm, BldSubdirPathAtm, GOS)
	:-
	adjust_path_depth(BLD_NATV_SRC_PATH,1,BNSP1),
		%% General header vars:
	grl_vars(ARCH, OS, BNSP1, SubGrlHeaderLines),
	general_os(ARCH,OS,BaseOS,_),
		%% Specific header vars:
	ws_vars(Subdir, ARCH, BaseOS, SwitchInfo, WSHeaderItems),
	flatten_ws_lists(WSHeaderItems, WSHeaderLines),
	append(SubGrlHeaderLines, WSHeaderLines, SubHeaderLines0),

	cfg('LIBS', LIBS),
	cfg('X_CFLAGS', INI_X_CFLAGS),
	cfg('X_LIBS', INI_X_LIBS),
	cfg('X_EXTRA_LIBS', INI_X_EXTRA_LIBS), 


		%% Handle things like location of Motif include
		%% files and libraries:

	xx_cfg(Subdir, 'X_CFLAGS', BaseOS, INI_X_CFLAGS, X_CFLAGS),
	xx_cfg(Subdir, 'X_LIBS', BaseOS, INI_X_LIBS, X_LIBS),
	xx_cfg(Subdir, 'X_EXTRA_LIBS', BaseOS, INI_X_EXTRA_LIBS, X_EXTRA_LIBS), 

	SubHeaderLines = [
		'LIBS' = LIBS,
		'X_CFLAGS' = X_CFLAGS,
		'X_LIBS' = X_LIBS,
		'X_EXTRA_LIBS' = X_EXTRA_LIBS,		
		'BDINCS' = BDINCS
		|  SubHeaderLines0 ],

	mk_bd_incs(BDList, BDINCS),

		%% Handle the  makefile:

		%% Get the path to the source makefile.in:
		%%
	extendPath(BldSubdirPathAtm, 'makefile.in', SubDirMakefile),
	(exists_file(SubDirMakefile) ->
		SrcMKFTail = SubDirMakefile
		;
		extendPath(BldPathAtm, 'mf-cmn.in', SrcMKFTail)
	),

		%% Path to target makefile to create:
	pathPlusFile(Subdir,'makefile',SubdirMakefile),

		%% Write header lines into target makefile, followed by
		%% copying all of source makefile:
	trans_xtnd_makefile(SrcMKFTail, SubHeaderLines, SubdirMakefile, GOS), 

		%% Copy tconfig.h to this directory, from
		%% the parallel bld-port/tconfig.h:
		%%
	catenate('cp ../bld-port/tconfig.h ', Subdir, CpCmd),
	system_call(CpCmd),

		%% Handle pi_cfg.h:
	check_default(WSHeaderItems, 'CFG', '', Cfgs),
	cat_together_seplines(Cfgs, CfgAtm),
	pathPlusFile(Subdir,'pi_cfg.h', TgtPICfg),
	open(TgtPICfg,write,PICFGSt,[]),
	printf(PICFGSt,'%t\n',[CfgAtm]),
	close(PICFGSt),

		%% Handle pi_init.c:
	pathPlusFile(BldSubdirPathAtm, 'pi_init.c', SrcPIInit),
	pathPlusFile(Subdir,'pi_init.c', TgtPIInit),
	copy_file_nl(SrcPIInit, TgtPIInit, GOS),

	install_alsdir(Subdir, BLD_NATV_SRC_PATH, GOS),
	install_tests(Subdir, BLD_NATV_SRC_PATH, GOS).


adjust_path_depth(['' | Tail],_,['' | Tail]) :-!.
adjust_path_depth(BLD_NATV_SRC_PATH, N, BNSP)
	:-
	add_depth(N, BLD_NATV_SRC_PATH, BNSP).

add_depth(0, BNSP, BNSP) :-!.
add_depth(N, BP, BNSP)
	:-
	M is N-1,
	add_depth(M, ['..' | BP], BNSP).

	%%---------------------------------------------
	%% Work out, if possible where to find various
	%% ws (e.g., Motif) include files, libraries,
	%% etc.;  Note that the X files are already
	%% taken care of, when appropriate:
	%%---------------------------------------------

	%% Include files:
xx_cfg(Subdir, 'X_CFLAGS', OS, INI_X_CFLAGS, X_CFLAGS)
	:-!,
	search_includes(Subdir,OS, SpecialIncs),
	Initial = [INI_X_CFLAGS |  SpecialIncs],
	subsid_includes(Subdir, OS, SubsidIncs),
	append(Initial, SubsidIncs, TheseIncs),
	interleave(TheseIncs, ' ', Second),
	catenate(Second, X_CFLAGS).

	%% Library files:
xx_cfg(Subdir, 'X_LIBS', OS, INI_X_LIBS, X_LIBS)
	:-!,
	search_libs(Subdir,OS, BasicLibs),
	Initial = [INI_X_LIBS | BasicLibs],
	subsid_libs(Subdir, OS, SubsidLibs),
	union(SubsidLibs, Initial, TheseLibs),
	interleave(TheseLibs, ' ', Second),
	catenate(Second, X_LIBS).

xx_cfg(_, _, _, X_EXTRA_LIBS, X_EXTRA_LIBS).

subsid_includes(Subdir, OS, SubsidIncs)
	:-
	bagof(Subsub, incl_req(Subdir, OS, Subsub), SubSL),
	!,
	s_i_l(SubSL, OS, SubsidIncs).

subsid_includes(Subdir, OS, []).

s_i_l([], OS, []).
s_i_l([Subsub | SubSL], OS, SSI)
	:-
	search_includes(Subsub,OS, TheseSS),
	s_i_l(SubSL, OS, SubsidIncs),
	append(SubsidIncs, TheseSS, SSI).

subsid_libs(Subdir, OS, SubsidIncs)
	:-
	bagof(Subsub, incl_req(Subdir, OS, Subsub), SubSL),
	!,
	s_l_l(SubSL, OS, SubsidIncs).

subsid_libs(Subdir, OS, []).

s_l_l([], OS, []).
s_l_l([Subsub | SubSL], OS, SSI)
	:-
	search_libs(Subsub,OS, TheseSS),
	s_l_l(SubSL, OS, SubsidIncs),
	append(TheseSS, SubsidIncs, SSI).

	%%-------------------------------------
	%% Locate the libraries for this subdir:
	%%-------------------------------------

	%% Try different places for the includes corresponding
	%% to this subdir (e.g., motif):

search_includes(Subdir, OS, Incs)
	:-
	characteristic_files(Subdir, include, CharactFiles),
	places_to_look(CharactFiles, Subdir, include, OS, IncDirs),
	prefix_to(IncDirs, ' -I ', Incs).

/*
	%% May be nothing to do for X:
search_includes(x,_,[]) :-!.
*/

	%% Return empty list otherwise:
search_includes(_,_,[]).

places_to_look([], Subdir, Type, OS, []).
places_to_look([CFile | CharactFiles], Subdir, Type, OS, Incs)
	:-
	search_dir_for(CFile, Subdir, Type, OS, TheseIncDirs),
	!,
	append(TheseIncDirs, RestIncs, Incs),
	places_to_look(CharactFiles, Subdir, Type, OS, RestIncs).

search_dir_for(CFile, Subdir, Type, OS, TheseIncDirs)
	:-
	find_possible_dir_for(Subdir,Type,OS,InitIncDir),
	fin_search_dir_for(CFile, Subdir, Type, OS, InitIncDir, TheseIncDirs).

fin_search_dir_for(CFile, Subdir, Type, OS, IncDir, [IncDir])
	:-
	extendPath(IncDir,CFile,TestPath),
	files(TestPath, [_|_]).
	
:- dynamic(lcl_config/1).

find_possible_dir_for(Subdir,Type,OS,InitIncDir)
	:-
	lcl_config(possible_dir_for(Subdir,Type,OS,InitIncDir)).
	
find_possible_dir_for(Subdir,Type,OS,InitIncDir)
	:-
	possible_dir_for(Subdir,Type,OS,InitIncDir).

	%%-------------------------------------
	%% Locate the libraries for this subdir:
	%%-------------------------------------

search_libs(x,_,[]) :-!.

search_libs(Subdir,OS, Libs)
	:-
	characteristic_files(Subdir, lib, CharactFiles),
	places_to_look(CharactFiles, Subdir, lib, OS, LibsDirs),
	prefix_to(LibsDirs, ' -L', Libs).

search_libs(_,_,[]).

filt_for_atom(InList, Atom, OutList)
	:-
	atom_length(Atom, AtomLen),
	filt_for_atom(InList, Atom, AtomLen, OutList).

filt_for_atom([], _, _, []).

filt_for_atom([Item | InList], Atom, AtomLen, [Item | OutList])
	:-
	sub_atom(Item, 1, AtomLen, Atom),
	!,
	filt_for_atom(InList, Atom, OutList).

filt_for_atom([Item | InList], Atom, OutList)
	:-
	filt_for_atom(InList, Atom, OutList).


mk_bd_incs(BDList, BDINCS)
	:-
	mk_bd_incs0(BDList, BDINCS0),
	catenate(BDINCS0, BDINCS).

mk_bd_incs0([], []).
mk_bd_incs0([BD | BDList], [IBD | BDINCS])
	:-
	catenate(' -I',BD,IBD),
	mk_bd_incs0(BDList, BDINCS).

trans_xtnd_makefile(SrcMFile, HeaderLines, TgtMFile, GOS)
	:-
	open(SrcMFile,read,IStrm,[]),
	open(TgtMFile,write,OStrm, []),
	write_makefile(HeaderLines,IStrm,OStrm,GOS),
	close(OStrm),
	close(IStrm).

install_alsdir(Subdir, BLD_NATV_SRC_PATH, GOS)
	:-
		%% create alsdir subdirectory:
	pathPlusFile(Subdir,alsdir,ALSDIRPATH),
	check_make_subdir(ALSDIRPATH),

		%% create alsdir/builtins subdirectory:
	pathPlusFile(ALSDIRPATH, builtins, BI_PATH),
	check_make_subdir(BI_PATH),
		%% create alsdir/library subdirectory:
	pathPlusFile(ALSDIRPATH, library, LI_PATH),
	check_make_subdir(LI_PATH),

	adjust_path_depth(BLD_NATV_SRC_PATH, 3, DeepPath),
		%% path to original builtins files:
	append(DeepPath,[builtins],BI_BLDSRCLIST),
	subPath(BI_BLDSRCLIST, BI_SRC_DIR),
		%% path to original library files:
	append(DeepPath,[library],LI_BLDSRCLIST),
	subPath(LI_BLDSRCLIST, LI_SRC_DIR),

	get_cwd(CurDir),
	( '#define'('HAVE_SYMLINK',1) ->
		link_files(BI_PATH, BI_SRC_DIR, GOS),
		link_files(LI_PATH, LI_SRC_DIR, GOS)
		;
		copy_dir_files_nl(BI_SRC_DIR, BI_PATH, GOS),
		copy_dir_files_nl(LI_SRC_DIR, LI_PATH, GOS)
	),
	change_cwd(CurDir),

		%% copy als-mics:
	mics_copy(ALSDIRPATH),

		%% Copy C interface generator to alsdir:
	adjust_path_depth(BLD_NATV_SRC_PATH, 2, ShallowPath),
	append(BLD_NATV_SRC_PATH, ['cinterf'], CINTPathList),
	subPath(CINTPathList, CIPATH),

	extendPath(CIPATH, c2pro, C2PPath),
	catenate(['tc2p',GOS,'.pro'],Tc2p),
	pathPlusFile(C2PPath, Tc2p, TC2P),
	pathPlusFile(ALSDIRPATH, Tc2p, ADTC2P),

	extendPath(CIPATH, pro2intf, Pro2IntPath),
	Tp2i = 'tp2intf.pro',
	pathPlusFile(Pro2IntPath, Tp2i, P2IP),
	pathPlusFile(ALSDIRPATH, Tp2i, ADP2IP),

	get_cwd(CurDir),
	( '#define'('HAVE_SYMLINK',1) ->
		change_cwd(ALSDIRPATH),
		create_sym_link(GOS, TC2P, Tc2p),
		create_sym_link(GOS, P2IP, Tp2i)
		;
		copy_file_nl(TC2P, ADTC2P, GOS),
		copy_file_nl(P2IP, ADP2IP, GOS)
	),
	change_cwd(CurDir).
	
copy_command('copy ')
	:-
	sys_env(unix,djgpp,_),
	!.
copy_command('cp ').

mics_copy(ALSDIRPATH)
	:-
	sys_env(unix,djgpp,_),
	!,
	pathPlusFile(ALSDIRPATH, 'als-mics', NewALSMICSPATH0),
	sl2sl(NewALSMICSPATH0, NewALSMICSPATH),
	OldMICS = '..\\bld-port\\alsdir\\als-mics',
	catenate(['copy ',OldMICS,' ',NewALSMICSPATH],Cmd),
	system_call(Cmd).

mics_copy(ALSDIRPATH)
	:-
	pathPlusFile(ALSDIRPATH, 'als-mics', NewALSMICSPATH),
	OldMICS = '../bld-port/alsdir/als-mics',
	catenate(['cp ',OldMICS,' ',NewALSMICSPATH],Cmd),
	system_call(Cmd).

sl2sl(In, Out)
	:-
	name(In, InCs),
	s2s(InCs,OutCs),
	name(Out, OutCs).

s2s([],[]).
s2s([0'/ | InCs], [92 | OutCs])
	:-!,
	s2s(InCs,OutCs).
s2s([C | InCs], [C | OutCs])
	:-
	s2s(InCs,OutCs).

install_tests(Subdir, BLD_NATV_SRC_PATH, GOS)
	:-
	get_cwd(CurDir),
	change_cwd(Subdir),
	check_make_subdir(tests),
	change_cwd(tests),
	adjust_path_depth(BLD_NATV_SRC_PATH, 2, ShallowPath),
	append(ShallowPath,[wins,build,Subdir],WBPath),
	rootPathFile('',WBPath,'wintests.pro',WTFile),
	(exists_file(WTFile) ->
		( '#define'('HAVE_SYMLINK',1) ->
			create_sym_link(GOS, WTFile, 'wintests.pro')
			;
			copy_file_nl(WTFile, 'wintests.pro', GOS)
		)
		;
		true
	),
	change_cwd(CurDir).

link_files(LinkDir, SrcDir, GOS)
	:-
	get_cwd(CurDir),
	change_cwd(LinkDir),
	pathPlusFile(SrcDir, '*', SourcePattern),
	files(SourcePattern, FileList),
	install_links0(FileList, '.', SrcDir, GOS),
	change_cwd(CurDir).


install_links0([], _, _, _).
install_links0([File | FileList],LinkDir, SrcDir, GOS)
	:-
	pathPlusFile(SrcDir,File,SrcFile),
	pathPlusFile(LinkDir,File, LinkFile),
	create_sym_link(GOS, SrcFile, LinkFile),
	install_links0(FileList,LinkDir, SrcDir, GOS).

create_sym_link(unix, SrcFile, LinkFile)
	:-
%	make_symlink(SrcFile,LinkFile),
	catenate(['ln -s ',SrcFile, ' ',LinkFile],Cmd),
	system_call(Cmd).

grl_vars(ARCH_NATV, OS, BLD_NATV_SRC_PATH , GrlHeaderLines)
	:-
	append(BLD_NATV_SRC_PATH, [wins,src], WISPathList),
	subPath(WISPathList, WISPath),
	append(BLD_NATV_SRC_PATH, ['generic'], GPathList),
	subPath(GPathList, GPath),
	catenate([WISPath,':',GPath], VPATH),
	append(BLD_NATV_SRC_PATH,['cinterf'],CINTPathList),
	subPath(CINTPathList, CINTPath),
	general_os(ARCH,OS,GOS),

	subPath(BLD_NATV_SRC_PATH, BLD_NATV_SRC_PATHAtm),

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
		%% This code should run after makes have been done in both
		%% ../bld-natv and ../bld-port;  its purpose is
		%% to check whether a native code version exists
		%%    NOTE: $(ALSPRO) is used in PrologPath because the
		%% value for ALSPRO is passed on the command line of the
		%% make invocation;  this is so we can distinguish as
		%% to whether alspro_b or alspro can be used to run things
		%% The conditional below tests whether to use bld-port or
		%% bld-natv ;
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		%% This is running in:  <XXX>/bld-wins
	((exists_file('../bld-natv/alspro') ; exists_file('../bld-natv/alspro_b')) 
		->
		ProLibPath = '../../bld-natv/alspro.a',
		PrologPath = '../../bld-natv/$(ALSPRO)',
		ARCH = ARCH_NATV
		;
		ProLibPath = '../../bld-port/alspro.a',
		PrologPath = '../../bld-port/$(ALSPRO)',
		ARCH = port
	),

	cfg('CC', CC),
	cfg('LIBS', LIBS),
	cfg('X_CFLAGS', X_CFLAGS),
	cfg('X_LIBS', X_LIBS),
	cfg('X_EXTRA_LIBS', X_EXTRA_LIBS),
	cfg('SOS', SOS),

	GrlHeaderLines =
	[
		srcdir		= BLD_NATV_SRC_PATHAtm,
		'PROLIB'  	= ProLibPath,
		'PROLOG'  	= PrologPath,
		'ARCH'     	= ARCH,
		'OS'     	= OS,
		'SOS'     	= SOS,
		'GOS'     	= GOS,
		'CC'     	= CC
	].

write_makefile(HeaderLines,IStrm,OStrm,GOS)
	:-
	write_lines_nl(HeaderLines,OStrm,GOS, [quoted(false)]),
	copy_stream_nl(IStrm, OStrm, GOS).

clean_lines(SubdirList, CleanLines)
	:-
	do_clean_lines(SubdirList, cleanup, PlainCleanLines),
	do_clean_lines(SubdirList, super_clean, SuperCleanLines),
	append(['','cleanup:' | PlainCleanLines],
		['','super_clean:' | SuperCleanLines],
		  CleanLines).

do_clean_lines([], Tag, []).
do_clean_lines([SD | SubdirList], Tag, [L | CleanLines])
	:-
	sprintf(LCs, '\tcd %t; make %t', [SD,Tag]),
	name(L, LCs),
	do_clean_lines(SubdirList, Tag, CleanLines).

interleave([], _, []).
interleave([Item | ItemList], Inter, [Item,Inter | Result])
	:-
	interleave(ItemList, Inter, Result).

install_sharbld
	:-
	cfg(srcdir,SD),
	extendPath('bld-port',SD,SRC_Dir),
	cfg('ARCH', ARCH),
	cfg('OS', OS),
	general_os(ARCH, OS, GOS),
	mkdist:assert(nl_type(GOS)),
	get_cwd(CurDir),
	cp_txt( SRC_Dir/sharbld 				> CurDir ),
	cp_txt( SRC_Dir/pconfig/'sharalsp.pro'	> CurDir ).

system_call(Str) :- system(Str), !.
system_call(Str) :- printf('System Call Failed: %t\n', [Str]).


endmod.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%%	CREATING DISTRIBUTION DIRECTORIES
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module mkdist.
use pconfig.

default_dist_dir('/mailbox3/als_dist').
default_examps_dir('/mailbox3/als_dist/examples').

export setup_dist/0.
setup_dist
	:-
	get_cmdline_vals(SwitchVals),
		%% BLD_Dir is the incoming directory containing
		%% a complete build for some architecture:
	try_for_bld(SwitchVals, BLD_Dir),
	!,
		%% DSTDIR is the directory which will contain the distribution
		%% (sub)directory to be made out of BLD_Dir:
	(dmember(['-dist', DSTDIR], SwitchVals) ->
		true 
		; 
		default_dist_dir(DSTDIR)  
	),
		%% EXPDIR is the directory to find all the example (sub)dirs;
		%% almost always use the default:
	(dmember(['-exp', EXPDIR], SwitchVals) ->
		true 
		; 
		default_examps_dir(EXPDIR)  
	),
	setup_dist(BLD_Dir,DSTDIR,EXPDIR).

setup_dist
	:-
	als_advise('Error: No build directory specified!\n').

	%% BLD_Dir is explicitly set on the command line:
try_for_bld(SwitchVals, BLD_Dir)
	:-
	dmember(['-bld', BLD_Dir], SwitchVals),
	!.
	%% BLD_Dir not set on the command line; if we are located in a directory
	%% containing a cfg.pro file, assume this is the build dir:
try_for_bld(_, CurDir)
	:-
	get_cwd(CurDir),
	pathPlusFile(CurDir,'cfg.pro',CFGPRO),
	exists_file(CFGPRO).

export setup_dist/3.
setup_dist(BLD_Dir,DSTDIR,EXP_Dir)
	:-
	get_cwd(CurDir),

		%% Load the cfg.pro file for the build:
	pathPlusFile(BLD_Dir,'cfg',CFG),
	consult_to(mkdist,CFG),
		%% Get the configuration info for the incoming build:
	cfg(srcdir,SRC_Dir),
	cfg('ARCH', ARCH),
	cfg('OS', OS),
	cfg(prologVersion,PVer),
	general_os(ARCH, OS, GOS),
	assert(nl_type(GOS)),

		%% make the name of the distribution (sub)directory:
	catenate([ARCH,'_',OS,'-',PVer],DistDirName),

	DST_Dir = DSTDIR/DistDirName,
	subPath([DSTDIR,DistDirName], DST_Dir_Path),
	(exists_file(DST_Dir_Path) ->
		date(Y/M/D),
		catenate([DistDirName,'_',Y,'_',M,'_',D],SaveDistDirName),
		subPath([DSTDIR,SaveDistDirName],SavePath),
		catenate(['mv ',DST_Dir_Path,' ',SavePath],Cmd),
		system_call(Cmd)
		;
		true
	),
	setup_subdirs(DST_Dir, CurDir),
	setup_subdirs(DST_Dir/native, CurDir),
	setup_subdirs(DST_Dir/threaded, CurDir),

	extendPath(BLD_Dir, 'bld-natv', BNPath),
	pathPlusFile(BNPath,alspro,NativeALS),

	(exists_file(NativeALS) ->
		cp_bin( BLD_Dir/'bld-natv'/alspro		> DST_Dir/native ),
		cp_bin( BLD_Dir/'bld-natv'/'alspro.a'	> DST_Dir/native ),

		cp_bin( BLD_Dir/'bld-port'/alspro		> DST_Dir/threaded ),
		cp_bin( BLD_Dir/'bld-port'/'alspro.a'	> DST_Dir/threaded )
		;
		cp_bin( BLD_Dir/'bld-port'/alspro		> DST_Dir),
		cp_bin( BLD_Dir/'bld-port'/'alspro.a'	> DST_Dir)
	),

	setup_subdirs(DST_Dir/alsdir/builtins, CurDir),
	setup_subdirs(DST_Dir/alsdir/library, CurDir),
	cp_txt( BLD_Dir/'bld-natv'/alsdir/builtins/'*.pro' 	> DST_Dir/alsdir/builtins ),
	cp_txt( BLD_Dir/'bld-natv'/alsdir/library/'*.pro' 	> DST_Dir/alsdir/library ),

	setup_subdirs(DST_Dir/alsdir/linking, CurDir),
	cp_txt( SRC_Dir/pconfig/'make*.in' 	> DST_Dir/alsdir/linking ),
	cp_txt( SRC_Dir/pconfig/'cfg*.pro' 	> DST_Dir/alsdir/library ),
	cp_txt( SRC_Dir/generic/'alspi.h'	> DST_Dir/alsdir/linking ),
	cp_txt( SRC_Dir/generic/'pimain.c'	> DST_Dir/alsdir/linking ),

	cp_bin( BLD_Dir/'bld-port'/alsdir/'als-mics'	> DST_Dir/alsdir ),
	catenate(['trans_c2pro_',GOS,'.pro'],Tc2p),
	cp_txt( SRC_Dir/'cinterf'/c2pro/Tc2p		> DST_Dir/alsdir ),
	cp_txt( SRC_Dir/'cinterf'/pro2intf/'tpro2intf.pro' > DST_Dir/alsdir ),

	setup_subdirs(DST_Dir/examples, CurDir),
	setup_subdirs(DST_Dir/examples/als, CurDir),
	setup_subdirs(DST_Dir/examples/pxs, CurDir),
		
	cp_txt( EXP_Dir/examples/als/'*.pro' 	> DST_Dir/examples/als ),
	cp_txt( EXP_Dir/examples/pxs/'*.pro' 	> DST_Dir/examples/pxs ),

	winsystems_for(ARCH, OS, WSL),
	extendPath(BLD_Dir, 'bld-wins', BLD_WI),
	dist_ws_list(WSL, DST_Dir, BLD_WI).

cp_bin( SrcFilesDesc > DestDirDesc )
	:-
	slash2list(DestDirDesc, DestDirList),
	subPath(DestDirList, DestDir),

	SrcFilesDesc = (SrcPathDesc) / FilesDesc,
	slash2list(SrcPathDesc, SrcPathList),
	rootPathFile('', SrcPathList, FilesDesc, SrcFiles),

	catenate(['cp ', SrcFiles, ' ', DestDir], Cmd),
	system_call(Cmd),
	printf(user,'Copied %t --> %t\n',[SrcFiles,DestDir]).

cp_bin([], _).
cp_bin([F | Fs], (Src > Tgt) )
	:-
	cp_bin( Src/F > Tgt ),
	cp_bin(Fs, (Src > Tgt) ).
	
export cp_txt/1.
cp_txt( SrcFilesDesc > DestDirDesc )
	:-
	slash2list(DestDirDesc, DestDirList),
	subPath(DestDirList, DestDir),

	SrcFilesDesc = (SrcPathDesc) / FilesDesc,
	slash2list(SrcPathDesc, SrcPathList),
	subPath(SrcPathList,SrcDir),
	pathPlusFile(SrcDir, FilesDesc, SrcPattern),
	files(SrcPattern, SourceFiles),

	nl_type(NL_type),
	copy_fileslist_nl(SourceFiles, SrcDir, DestDir, NL_type),
	printf(user,'Copied %t --> %t\n',[SrcDir/FilesDesc,DestDir]).

export cp_txt/2.
cp_txt([], _).
cp_txt([F | Fs], (Src > Tgt) )
	:-
	cp_txt( Src/F > Tgt ),
	cp_txt(Fs, (Src > Tgt) ).
	
setup_subdirs([], _) :-!.
setup_subdirs([DirDesc | DirsDesc], StartDir)
	:-!,
	setup_subdirs(DirDesc, StartDir),
	setup_subdirs(DirsDesc, StartDir).

setup_subdirs(DirsDesc, StartDir)
	:-
	slash2list(DirsDesc, DirsPathList),
	setup_subdirs(DirsPathList, PathAccum, PathAccum, StartDir).

setup_subdirs([], PathAccum, PathTail, StartDir)
	:-!,
	change_cwd(StartDir).

setup_subdirs([Dir | DirsPathList], PathAccum, PathTail, StartDir)
	:-
	exists_file(Dir),
	!,
	change_cwd(Dir),
	not(not(( PathTail = Dir,
			  printf(user,'%t exists\n',[PathAccum]) ) )),
	PathTail = Dir/NewPathTail,
	setup_subdirs(DirsPathList, PathAccum, NewPathTail, StartDir).

setup_subdirs([Dir | DirsPathList], PathAccum, PathTail, StartDir)
	:-
	not(not(( PathTail = Dir,
			  printf(user,'Making %t\n',[PathAccum]) ) )),
%	make_subdir(Dir,511),
	make_subdir(Dir),
	change_cwd(Dir),
	PathTail = Dir/NewPathTail,
	setup_subdirs(DirsPathList, PathAccum, NewPathTail, StartDir).


dist_ws_list([], _, _).
dist_ws_list([Ws_SubDir | WSL], DST_Dir, BLD_WI)
	:-
	get_cwd(CurDir),
	setup_subdirs(DST_Dir/Ws_SubDir, CurDir),
	pathPlusFile(BLD_WI, Ws_SubDir, Ws_SrcDir),
	pathPlusFile(Ws_SrcDir, '*.pro', Ws_SrcPro),
	files(Ws_SrcPro, ProFiles),

	ws_bin_files(Ws_SubDir, BinFiles),

	cp_bin(BinFiles, BLD_WI/Ws_SubDir > DST_Dir/Ws_SubDir),

	cp_txt(ProFiles, BLD_WI/Ws_SubDir > DST_Dir/Ws_SubDir),

	dist_ws_list(WSL, DST_Dir, BLD_WI).

ws_bin_files(WS, BinFiles)
	:-
	catenate(alspro_, WS, Image),
	catenate([alspro_, WS, 0], Image0),
	catenate(WS,'interf.a', WSLib),
	BinFiles = [Image, Image0, WSLib].

endmod.

