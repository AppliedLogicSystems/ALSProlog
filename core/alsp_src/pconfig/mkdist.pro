/*===================================================================
 |		mkdist.pro
 |	Copyright (c) 1994 Applied Logic Systems, Inc.
 |
 |	Prolog-stage configuration during builds
 |
 |	Author: Ken Bowen
 |	Date begun: 8 Aug 94
 *==================================================================*/

module distrib.

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
	 |	Determining bld dirs & libs
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

		%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*

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
*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%
%%%%	CREATING DISTRIBUTION DIRECTORIES
%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


export distrib/0.

distrib
	:-
		%% should wake up in the top level of a build dir:
	get_cwd(BldDir),
	get_params(ARCH,OS,Vers,Srcdir),
	change_cwd(Srcdir),
	getcwd(AbsoluteSrcdir),
	change_cwd(BldDir),
	check_build_dir(ARCH,OS,Vers,Srcdir,DistDir),
		%% fix me:
	sprintf(atom(BasicSetupCmd),'../bin/setup_basic_unix %t',[AbsoluteSrcdir]),
	change_cwd(DistDir),
	system(BasicSetupCmd),
	change_cwd(BldDir),
	
	extendPath(DistDir,alsdir,DistALSDIR),
	sprintf(atom(MICSCMD),
			'cp bld-port/alsdir/als*mic* %t', [DistALSDIR]),
	system(MICSCMD),

	extendPath(DistDir, threaded, DistThreaded),
	cp2dist('cp bld-port/%t %t', 'alspro.a', DistThreaded),
	cp2dist('cp bld-port/%t %t', alspro_b, DistThreaded),
	cp2dist('cp bld-port/%t %t', alspro,   DistThreaded),

	(exists_file('bld-natv/alspro.a') ->
		extendPath(DistDir, native, DistNative),
		(exists_file(DistNative) ->
			true
			;
			make_subdir(DistNative)
		),
		cp2dist('cp bld-natv/%t %t', 'alspro.a', DistNative),
		cp2dist('cp bld-natv/%t %t', alspro_b, DistNative),
		cp2dist('cp bld-natv/%t %t', alspro,   DistNative)
	),

	(exists_file('bld-wins') ->
		distrib_wins(AbsoluteSrcdir, DistDir)
		;
		true
	).


get_params(ARCH,OS,Vers,Srcdir)
	:-
	(exists_file('cfg.pro') ->
		consult_to(distrib,-cfg)
		;
		build_base_config
	),
	cfg('ARCH', ARCH),
	cfg('OS', OS),
	cfg(prologVersion, Vers),
	open(makefile,read,MFS,[]),
	read_to(srcdir,MFS,Line),
	close(MFS),
	atom_codes(Line,LTCs),
	nth_tail(7,LTCs,_,LTCs1),
	strip_white(LTCs1, [0'= | LTCs2]),
	strip_white(LTCs2, LTCs3),
	strip_tail_white(LTCs3, LTCs4),
	atom_codes(Srcdir, LTCs4).
	
read_to(Tag,MFS,Line)
	:-
	atom_length(Tag,TagLen),
	read_to(Tag,TagLen,MFS,Line).

read_to(Tag,TagLen,MFS,Line)
	:-
	get_line(MFS, NextLine),
	(sub_atom(NextLine, 1, TagLen, Tag) ->
		Line = NextLine
		;
		read_to(Tag,TagLen,MFS,Line)
	).









check_build_dir(ARCH,OS,Vers,Srcdir,DistDir)
	:-
	open(atom(DistDirName),write,DDNS, []),
	printf(DDNS,'%t_%t-%t',[ARCH,OS,Vers]),
	close(DDNS),
	extendPath(Srcdir,'..',TD1),
	extendPath(TD1,als_dist,DDPath),
	pathPlusFile(DDPath,DistDirName,DistDir),
	(exists_file(DistDir) ->
		true
		;
		make_subdir(DistDir)
	).


cp2dist(Fmt,Filename,TgtDir)
	:-
	sprintf(atom(CMD), Fmt, [Filename, TgtDir]),
	system(CMD).


distrib_wins(AbsoluteSrcdir, DistDir)
	:-
	open('bld-wins/makefile',read,TMFS,[]),
	read_to('LIBRARIES', TMFS, Line),
	close(TMFS),
	atom_codes(Line,LTCs),
	nth_tail(10,LTCs,_,LTCs1),
	strip_white(LTCs1, [0'= | LTCs2]),
	strip_white(LTCs2, LTCs3),
	strip_tail_white(LTCs3, LTCs4),
	split_to_atoms(LTCs4, LibsList),
	distrib_wins(LibsList, DistDir, AbsoluteSrcdir).

distrib_wins([], DistDir, AbsoluteSrcdir) :-!.
distrib_wins(LibsList, DistDir, AbsoluteSrcdir)
	:-
	extendPath(DistDir,'bld-wins', WinsDistDir),
	(exists_file(WinsDistDir) ->
		true
		;
		make_subdir(WinsDistDir)
	),

	make_each_subdir(LibsList, WinsDistDir),
%	extendPath(WinsDistDir, 'bld-wins', BWD),
	BWD = 'bld-wins',
	copy_each_subdir(LibsList, BWD, WinsDistDir).



split_to_atoms([], []).
split_to_atoms(Cs, [Lib | LibsList])
	:-
	asplit00(Cs, [0' , 0'	], Left, Right),	% space, tab 
	!,
	atom_codes(Lib, Left),
	split_to_atoms(Right, LibsList).
split_to_atoms(Cs, [Lib])
	:-
	atom_codes(Lib, Cs).

make_each_subdir([], _).
make_each_subdir([Lib | LibsList], WinsDistDir)
	:-
	extendPath(WinsDistDir, Lib, LibDistDir),
	(exists_file(LibDistDir) ->
		true
		;
		make_subdir(LibDistDir)
	),
	make_each_subdir(LibsList, WinsDistDir).

copy_each_subdir([], _, _).
copy_each_subdir([Lib | LibsList], BWD, WinsDistDir)
	:-
	extendPath(BWD, Lib, LibDir),

	catenate([Lib,interf,'.a'], File1),
	cp4dist(BWD, Lib, File1, WinsDistDir),

	filePlusExt(Lib,pro,File2),
	cp4dist(BWD, Lib, File2, WinsDistDir),

	catenate([alspro_, Lib], File3),
	cp4dist(BWD, Lib, File3, WinsDistDir),

	catenate([File3, '0'], File4),
	cp4dist(BWD, Lib, File4, WinsDistDir),

	copy_each_subdir(LibsList, BWD, WinsDistDir).

cp4dist(BWD, Lib, File, WinsDistDir)
	:-
	Fmt = 'cp %t/%t/%t %t/%t',
	sprintf(atom(CMD), Fmt, [BWD,Lib,File, WinsDistDir,Lib]),
	system(CMD).

endmod.

