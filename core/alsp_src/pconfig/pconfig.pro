/*===================================================================
 |		pconfig.pro
 |	Copyright (c) 1994 Applied Logic Systems, Inc.
 |
 |	Prolog-stage configuration during builds
 |
 |	Author: Ken Bowen
 |	Date begun: 8 Aug 94
 *==================================================================*/


module pconfig.
use  mkdist.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Top-level entry to the prolog-level configuration process
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export pconfig/0.

pconfig 
	:-
	build_base_config,
	setup_wins.

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

%	install_sharbld.

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

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Read across /* -- */ - C-sytle comments
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Convert a string "#define <Symb> <Val>" to 
	%% a fact: '#define'('<Symb>', <PrologVal>)
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
			["ARCH","OS","srcdir","CC","LIBS" ,
			 "X_CFLAGS","X_LIBS", "X_EXTRA_LIBS"], 
	 		Info).

xtr_cfg_info(BLD_Dir,TagsList, Info)
	:-
	slash2list(BLD_Dir, BLDDirList),
	append(BLDDirList,['bld-natv'],NTVDirList),
	rootPathFile('', NTVDirList, 'makefile', MakefileAtm),
	open(MakefileAtm,read,MFStrm,[]),
	xtr_cfgi(TagsList, MFStrm, Info),
	close(MFStrm).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Extract configuration information from a given Makefile
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
	strip_white(TailCs,[0'=,0' |ValCsa]),
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
	 |	Setting up the bld-wins subdir
	 *---------------------------------------------------*/

setup_wins
	:-
	check_make_subdir('./bld-wins'),
	cfg('ARCH',ARCH),
	cfg('OS',OS),
	winsystems_for(ARCH, OS, SubdirList),
	(SubdirList = [] ->
		printf(user,'No win interface dirs to create! \n',[])
		;
		change_cwd('./bld-wins'),
		make_each_subdir(SubdirList),
		cfg(srcdir,BLD_NATV_SRC_PATH_Atm),

		extendPath(BLD_NATV_SRC_PATH_Atm,'wins/build',WINBLD ),
		pathPlusFile(WINBLD, 'makefile.in', WINMKPATH),

		general_os(ARCH,OS,GOS),
		interleave(SubdirList, ' ', TL),
		catenate(['LIBRARIES = ' | TL], Header),
		clean_lines(SubdirList, CleanLines),
		trans_xtnd_makefile(WINMKPATH, 
				    [Header | CleanLines], 
				    './makefile', GOS),

		subPath(BLD_NATV_SRC_PATH,BLD_NATV_SRC_PATH_Atm),
		create_makefiles_and_subdirs(SubdirList, ARCH, OS, BLD_NATV_SRC_PATH),
		change_cwd('..')
	),
	change_cwd('..').

interleave([], _, []).
interleave([Item | ItemList], Inter, [Item,Inter | Result])
	:-
	interleave(ItemList, Inter, Result).


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
%	make_subdir(Subdir,511),
	make_subdir(Subdir),
	printf(user,'   Created subdir %t \n',[Subdir]).

create_makefiles_and_subdirs( [], _, _, _ ).

create_makefiles_and_subdirs( [Subdir | Subdirs], ARCH, OS, BLD_NATV_SRC_PATH)
	:-
	append(BLD_NATV_SRC_PATH,[wins,build,Subdir], BldSubdirPath),
	subPath(BldSubdirPath, BldSubdirPathAtm),

	append(BLD_NATV_SRC_PATH,[wins,build], BldPath),
	subPath(BldPath, BldPathAtm),

	general_os(ARCH,OS,GOS),

	create_wsi_makefile( Subdir, ARCH, OS, BLD_NATV_SRC_PATH, 
				BldPathAtm, BldSubdirPathAtm, GOS),

	create_makefiles_and_subdirs( Subdirs, ARCH, OS, BLD_NATV_SRC_PATH).

create_wsi_makefile( Subdir, ARCH, OS, BLD_NATV_SRC_PATH, BldPathAtm, BldSubdirPathAtm, GOS)
	:-
	adjust_path_depth(BLD_NATV_SRC_PATH,1,BNSP1),
		%% General header vars:
	grl_vars(ARCH, OS, BNSP1, SubGrlHeaderLines),
		%% Specific header vars:
	ws_vars(Subdir, ARCH, OS, WSHeaderItems),
	flatten_ws_lists(WSHeaderItems, WSHeaderLines),
	append(SubGrlHeaderLines, WSHeaderLines, SubHeaderLines0),

	cfg('LIBS', LIBS),
	cfg('X_CFLAGS', X_CFLAGS),
	cfg('X_LIBS', X_LIBS),
	cfg('X_EXTRA_LIBS', X_EXTRA_LIBS), 

	SubHeaderLines = [
		'LIBS' = LIBS,
		'X_CFLAGS' = X_CFLAGS,
		'X_LIBS' = X_LIBS,
		'X_EXTRA_LIBS' = X_EXTRA_LIBS
		|  SubHeaderLines0 ],

		%% Handle the  makefile:
	extendPath(BldSubdirPathAtm, 'makefile.in', SubDirMakefile),
	(exists_file(SubDirMakefile) ->
		SrcMKFTail = SubDirMakefile
		;
		extendPath(BldPathAtm, 'mf-cmn.in', SrcMKFTail)
	),

	pathPlusFile(Subdir,'makefile',SubdirMakefile),
	trans_xtnd_makefile(SrcMKFTail, SubHeaderLines, SubdirMakefile, GOS), 

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
%	append(ShallowPath, ['cinterf'], CINTPathList),
	append(BLD_NATV_SRC_PATH, ['cinterf'], CINTPathList),
	subPath(CINTPathList, CIPATH),

	extendPath(CIPATH, c2pro, C2PPath),
%	catenate(['trans_c2pro_',GOS,'.pro'],Tc2p),
	catenate(['tc2p',GOS,'.pro'],Tc2p),
	pathPlusFile(C2PPath, Tc2p, TC2P),
	pathPlusFile(ALSDIRPATH, Tc2p, ADTC2P),

	extendPath(CIPATH, pro2intf, Pro2IntPath),
%	Tp2i = 'tpro2intf.pro',
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
	system(Cmd).

mics_copy(ALSDIRPATH)
	:-
	pathPlusFile(ALSDIRPATH, 'als-mics', NewALSMICSPATH),
	OldMICS = '../bld-port/alsdir/als-mics',
	catenate(['cp ',OldMICS,' ',NewALSMICSPATH],Cmd),
	system(Cmd).

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
	system(Cmd).

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
		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

		%% This is running in:  <XXX>/bld-wins
	(exists_file('../bld-natv/alspro') ->
		ProLibPath = '../../bld-natv/alspro.a',
		PrologPath = '../../bld-natv/alspro',
		ARCH = ARCH_NATV
		;
		ProLibPath = '../../bld-port/alspro.a',
		PrologPath = '../../bld-port/alspro',
		ARCH = port
	),

	cfg('CC', CC),
	cfg('LIBS', LIBS),
	cfg('X_CFLAGS', X_CFLAGS),
	cfg('X_LIBS', X_LIBS),
	cfg('X_EXTRA_LIBS', X_EXTRA_LIBS),

	GrlHeaderLines =
	[
		srcdir		= BLD_NATV_SRC_PATHAtm,
%		'VPATH'		= VPATH,
%		'CINTDIR' 	= CINTPath,
%		'WSRCD'   	= WISPath,
		'PROLIB'  	= ProLibPath,
		'PROLOG'  	= PrologPath,
		'ARCH'     	= ARCH,
		'OS'     	= OS,
		'GOS'     	= GOS,
		'CC'     	= CC,
		'LIBS'		= LIBS,
		'X_CFLAGS'	= X_CFLAGS,
		'X_LIBS'	= X_LIBS,
		'X_EXTRA_LIBS'	= X_EXTRA_LIBS
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
		system(Cmd)
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
	system(Cmd),
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

