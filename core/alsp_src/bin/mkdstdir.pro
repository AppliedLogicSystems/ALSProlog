/*===================================================================*
 |			mkdstdir.pro
 |		Copyright (c) 1996 Applied Logic Systems, Inc.
 |
 |	Sets up a distribution directory;  if directory already
 |	exists, freshens everything appropriately.
 |
 |	Assumes that it is waking up in the top of the distribution
 |	directory, namely
 |			~/als_dist
 |
 |	Should be called in the form:
 |	
 |		alspro -b bin/mkdstdir -g mkdstdir -p -ver <N.NN> 
 |
 |	where <N.NN> is the version number to use for the distribution.
 |
 |	This is packaged up in a script as follows:
 |
 |		bin/bld-dist <N.NN>
 |-----------------------------------------------------------------
 |		%% Macintosh  - Uses StuffIt Installer Maker
 |				-- should be run under MacOS
 |		%% MS Windows - Uses WISE Installer Maker
 |				-- should be run under Windows 95/NT
 |		%% DOS/DJ2 
 |				-- should be run by a Unix box
 *===================================================================*/

module mkdstdir.
use windows.

/*--------------------------------------------------------------------------------
 |	arch_os_list/1
 |	arch_os_list(List)
 |	arch_os_list(-)
 |
 |	- The list of <Arch>_<OS> descriptors which are supported
 *-------------------------------------------------------------------------------*/
arch_os_list([
	'sparc_solaris2.4',
	'sparc_sunos4.1.3',
	'mips_irix5.3',
	'hppa_hpux9.05',
	'all_macos7.5',		%%--fat distribution
	'powerpc_aix4.1',
	'i386_linux',
	'i386_mswin32',
	'i386_dj2'
	] ).

	%% To be used from inside the individual dist dir:
/*--------------------------------------------------------------------------------
 |	path_to(<XXX>_dir/3
 |	path_to(<XXX>_dir, Disk, PathList)
 |	path_to(+, -, -)
 |
 |	<XXX> ranges over: build, examples, source, als_dev, window
 |
 |	Provides a high-level description of the path FROM the distribution directory
 |	being created (ie, from inside that directory) to the various directories
 |	indicated.  The "Disk" is used on systems (Windows) where necessary, otherwise
 |	it is ignored by the later processing.
 *-------------------------------------------------------------------------------*/

path_to(build_dir,    '',  ['..','..',builds]).
path_to(examples_dir, '',  ['..','..',examples]).
path_to(source_dir,   '',  ['..','..',alsp_src]).
path_to(readme_dir,   '',  ['..', 'readme.files']).
path_to(als_dev_dir,  'F', [apache,als_dev]).
path_to(windows_dir,  'F', [apache,windows]).
path_to(link_exs_dir, 'F', [apache,als_embed]).



/*--------------------------------------------------------------------------------
 |	list of readme files to grab from ~als_dist/readme.files
 *-------------------------------------------------------------------------------*/
%%readme_files(['readme.top', 'copying.als', 'install.*']).
readme_files(['readme.top', 'copying.als', 'install.frame', 'install.ps', 'install.txt']).


/*--------------------------------------------------------------------------------
 |	als_executables_list/3
 |	als_executables_list(ArchOS, FileList, MainExecutable)
 |	als_executables_list(+, -, -)
 |	
 |	List of files to copy from the <builds>/ArchOS/bld-port directory, to
 |	distribution subdir 'threaded'; if one of these doesn't exist in the
 |	"source" build directory, it is simply skipped.  If Demo = true,
 |	the "source" files are the "demo_xxx" versions, while the names
 |  of the corresponding "target" versions drop the "demo_"
 *-------------------------------------------------------------------------------*/
als_executables_list(ArchOS, TheList, MainExecutable)
	:-
	basic_exec_list(ArchOS, InitList),
	main_executable(ArchOS, MainExecutable),
	list_delete(InitList, MainExecutable, TheList).

basic_exec_list(_, InitList)
	:-
	InitList = 
	/*  Unix		MacOS			Win32 */
	[   'alspro.a', 	'alspro.68k.lib', 	'alspro.lib',  /* Libraries */
				'alspro.ppc.lib',
				
							'alspro.dll',	/* Shared Libs */
							'alspro.dll.lib',
							
	    'alspro_b', 	'sioux_alspro_b', 	'alspro_b.exe',	/* Executables */
	    'alspro',		'sioux_alspro',		'alspro.exe',
	    
	/* Demo versions of the above (not all exist) */
	    
	    'demo_alspro.a', 	'demo_alspro.68k.lib',	'demo_alspro.lib',
	    			'demo_alspro.ppc.lib',
	    			
							'demo_alspro.dll',
							'demo_alspro.dll.lib',

	    'demo_alspro_b', 	'demo_sioux_alspro_b', 	'demo_alspro_b.exe',
	    'demo_alspro',	'demo_sioux_alspro',	'demo_alspro.exe'
	    						
	].

main_executable(i386_mswin32, 'alspro.exe') :-!.
main_executable('all_macos7.5', 'sioux_alspro') :-!.
main_executable(i386_dos,     'alspro.exe') :-!.
main_executable(_,   alspro).

/*--------------------------------------------------------------------------------
 | native_als_ok/2
 |	- define each case of native_als_ok/1 only when a given 
 |		architecture has native support in place
 *-------------------------------------------------------------------------------*/

:-dynamic(native_als_ok/1).

%	native_als_ok(ArchOS).

/*--------------------------------------------------------------------------------
 |	source_list/3
 |	source_list(DirDescrip, PatternList, ExcludedList)
 |	source_list(-, -, -)
 |
 |	DirDescrip = a directory description, normally a simple name, eg: 'builtins'
 |	PatternList =  a non-empy list of file names and/or patterns (eg '*.pro')
 |                 describing the files to be copied into the distribution from 
 |				   directory DirDescrip, modulo the exclusions via ExcludedList
 |	ExcludedList = a (possibly empty) list of base file names (no extensions)
 |                 whose sources are to be excluded from the distribution
 |
 |	If NN occurs on ExcludedList, and NN.Ext occurs in the source directory
 |	corresponding to DirDescrip (either in the source tree or the builds
 |	subtree, as appropriate -- no searching on this account), then NN.Ext
 |	is NOT copied to the distribution;  however, if NN.obp exists in the source 
 |	dir (so it will be a build source, eg <builds>/bld-port/alsdir/builtins),
 |	then NN.obp is copied to the distribution.
 *-------------------------------------------------------------------------------*/
source_list(c2pro,
			['c2p.pro', 'c2p.typ', c2pro, cexp, cfiles,
				cmacro, cout, cparse, ctoken, makefile],  
			[] ).

source_list(pro2intf, 
			[intfout, makefile, mytrans, 'p2i.pro', 
				'p2i.typ', pro2intf, 'tp2intf.pro' ], 
			[] ).


source_list(builtins, 
			['*.pro'], 
			[blt_shl, int_cstr, ra_basis]).

source_list(library,  
			['*.pro'], 
			[]).

source_list(linking_top, 
			['cfg.pro','cmn_info.mkf'], 
			[]).

source_list(linking_generic, 
			['pimain.c','alspi.h','cintef.h'], 
			[]).


link_examples(ArchOS, LEXDir, LinkExsPatternList)
	:-
	get_path_to(link_exs_dir, '', LEXDir),
	link_examples(ArchOS, LinkExsPatternList).

link_examples(i386_mswin32, ['*.c','*.h','*.mak','*.prj',
							 'make*','Make*', 'make*.*','Make*.*',
							 'readme*','README*','readme*.*','README*.*' ])
	:-!.

	%% Default:
link_examples(ArchOS, ['*','readme*','README*','make*','Make*']).

source_list(guilib,  	 
			['*.pro', '*.oop', '*.gui', '*.typ', makefile], 
			[]).

source_list(gui_samples, 
			['*.gui', makefile, 'makefile.mac'], 			 
			[]).

source_list(wins_intf_general, 
			['*.pro','*.h','*.c','*.src',makefile], 
			[]).

wins_ex_list(i386_mswin32, ['*.exe', '*.lib', '*.dll']) :-!.

wins_ex_list('all_macos7.5', ['alspro_macos', 'alspro_macos0',
							  'demo_alspro_macos', 'demo_alspro_macos0', '*.lib']) :-!.

wins_ex_list(_, ['*.a', 'alspro*']) :-!.

	%%%% This may change when the windows stuff is pulled out of
	%%%% the basic build process:

wins_intf_path(Path)
	:-
	get_path_to(source_dir, '', Path0),
	extendPath(Path0, wins, Path).

/*--------------------------------------------------------------------------------
 *-------------------------------------------------------------------------------*/

get_path_to(WhichDir, ArchOS, ThisDirPath)
	:-
	path_to(WhichDir, Disk,  PathList),
	determinePath(Disk, PathList, ArchOS, ThisDirPath).

	%% determinePath/4:
	%%

determinePath(Disk, [PathHead | RestPath], ArchOS, ThisBuildPath)
	:-
	sys_env(OS,_,_),
	setup_path_disk(OS, PathHead, RestPath, Disk, ThisDisk, ThisPathList),
	rootPathFile(ThisDisk, ThisPathList, ArchOS, ThisBuildPath).

setup_path_disk(dos, PathHead, RestPath, Disk, ThisDisk, ThisPathList)
	:-!,
	setup_path_disk(mswin32, PathHead, RestPath, Disk, ThisDisk, ThisPathList).

	%% Handle '..' case:

%%setup_path_disk(macos, '..', RestPath, _, '', [':', RestPath0]) 
setup_path_disk(macos, '..', ['..' | RestPath], '', '', ['::' | RestPath]).
setup_path_disk(macos, '..', RestPath, '', '', [':' | RestPath]).
setup_path_disk(macos, Head, RestPath, Disk, Head, RestPath).

setup_path_disk(_,     '..', RestPath, _, '', ['..' | RestPath]) :-!.

	%% Handle all other cases:

setup_path_disk(unix, PathHead, RestPath, _, '', ['', PathHead | RestPath]) :-!.

	%% mswin32: Disk and top element of path are synonymous: 
setup_path_disk(mswin32, PathHead, RestPath, Disk, Disk, ['' | RestPath]) :-!.

change_dots2colons([], []).
change_dots2colons(['..' | RestPath], [':' | RestPath0])
	:-!,
	change_dots2colons(RestPath, RestPath0).
change_dots2colons([X | RestPath], [X | RestPath0])
	:-
	change_dots2colons(RestPath, RestPath0).

	%-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@
	%@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-
	%-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@-@

/*--------------------------------------------------------------------------------
 |	mkdstdir/0
 |	mkdstdir
 |
 |	mkdstdir/1
 |	mkdstdir(SwitchVals)
 |	mkdstdir(+)
 |
 |	Main entry point for creating a distrib dir
 |
 |	mkdstdir/0 - used when the version number is being passed on the
 |		os command line; processes command line and calls mkdstdir/1.
 |
 |	Call to mkdstdir/1 looks like mkdstdir([['-ver', VersionAtom]])
 *-------------------------------------------------------------------------------*/
export mkdstdir/0.

mkdstdir 
	:-
	get_cmdline_vals(SwitchVals),
	pull_out_nullswitches(SwitchVals, _, ReducedSwitchVals),
	mkdstdir(ReducedSwitchVals).

mkdstdir(SwitchVals)
	:-
	dmember(['-ver', Ver], SwitchVals),
	!,
	ok_or_exit('Working with version %t',[Ver],
				cont_distdir(Ver, SwitchVals), 'Ok...exiting\n').

mkdstdir(SwitchVals)
	:-
	printf('Error: No version number supplied...exiting\n').

/*--------------------------------------------------------------------------------
 |	We have a version number; now choose the target distribution architecture:
 *-------------------------------------------------------------------------------*/
cont_distdir(Ver, SwitchVals)
	:-
	arch_os_list(ArchOSList),
	menu(_, ArchOSList, ChosenArchOS, []),
	disp_cont_distdir(ChosenArchOS, Ver, SwitchVals).

disp_cont_distdir('$noChoice', Ver, SwitchVals)
	:-
	printf('Ok .. No choice ...exiting\n').

disp_cont_distdir(ArchOS, Ver, SwitchVals) 
	:-
	check_os_match(ArchOS),
	!,
	get_path_to(build_dir, ArchOS, ThisBuildPath),
	ok_or_exit('Take build files from %t',[ThisBuildPath],
				onward(ArchOS, Ver, SwitchVals),
				'Ok...exiting\n').

disp_cont_distdir(ArchOS, Ver, SwitchVals) 
	:-
	asplit(ArchOS, 0'_, Arch,OS),
	general_os('',OS,GOS),
	printf('!!!You must be running under %t to build the %t distribution...exiting.\n',
				[GOS,ArchOS]),
	halt.

check_os_match(ArchOS)
	:-
	asplit(ArchOS, 0'_, Arch,OS),
	general_os('',OS,GOS),
	sys_env(GOS,_,_).



/*--------------------------------------------------------------------------------
 |	Now we have:
 |		- a version number 
 |		- a target distribution architecture;
 |
 |	Locate or set up the appropriate subdirectory for the distribution:
 |	Assumes we are in the top-level of "distribution home dir" (=DHD)
 *-------------------------------------------------------------------------------*/

onward(ArchOS, Ver, SwitchVals) 
	:-
	subdirs(SubDirs),
	get_cwd(DHD),
	setup_tgt_dir_1(SubDirs, ArchOS,DHD,Ver,SwitchVals,TopLevel).

/*--------------------------------------------------------------------------------
 |	Check for a 'short dir' (no version num suffix):
 |		-- WARNING: short dir stuff needs fixing/completion
 *-------------------------------------------------------------------------------*/
	%% There is a 'short dir (no version num)' already here:
setup_tgt_dir_1(SubDirs, ArchOS,DHD,Ver,SwitchVals,TopLevel)
	:-
	dmember(ArchOS, SubDirs),
	catenate([ArchOS,'-',Ver],XArchOS),
	pathPlusFile(DHD,XArchOS,FullTgtPath),
	!,
	short_dirs(SubDirs, ArchOS, XArchOS, FullTgtPath, DHD, Ver, SwitchVals,TopLevel).

	%% No 'short dir' already here:
setup_tgt_dir_1(SubDirs, ArchOS,DHD,Ver,SwitchVals,TopLevel)
	:-
	catenate([ArchOS,'-',Ver],XArchOS),
	pathPlusFile(DHD,XArchOS,FullTgtPath),
	long_dirs(SubDirs, ArchOS, XArchOS, FullTgtPath, DHD,Ver,SwitchVals,TopLevel).
setup_tgt_dir_1(SubDirs, ArchOS,DHD,Ver,SwitchVals,TopLevel)
	:-
	throw(setup_tgt_dir_1_error(SubDirs, ArchOS,DHD,Ver,
				    SwitchVals,TopLevel)).

/*--------------------------------------------------------------------------------
 |	No 'short dir' (no version num suffix) is present (this is default);
 |	Check for long dir with the incoming version number suffix, for this
 |	ArchOS: 
 |		- if not present, create it; 
 |		- if present, determine whether to update it, or move it &
 |		  create a new directory;
 *-------------------------------------------------------------------------------*/
	%% There is a 'long dir (has version num)' already here:
long_dirs(SubDirs, ArchOS, XArchOS, FullTgtPath, DHD, Ver, SwitchVals,TopLevel)
	:-
	dmember(XArchOS, SubDirs),
	!,
	printf('Distribution directory options:\n',[]),
	catenate('Update/Overwrite existing directory: ',FullTgtPath, OverwriteDir),
	catenate('Move existing directory: ',FullTgtPath, MoveDir),
	Actions = [OverwriteDir, MoveDir, 'Exit'],
	menu(_, Actions, ChosenAction, []),
	disp_long_dirs(ChosenAction, OverwriteDir, MoveDir, 
					SubDirs, ArchOS, XArchOS, FullTgtPath, 
					DHD, Ver, SwitchVals,TopLevel).

	%% No 'long dir' here, and doing long dirs:
long_dirs(SubDirs, ArchOS, XArchOS, FullTgtPath, DHD, Ver, SwitchVals,[])
	:-
	ok_or_exit('Create new directory %t',[FullTgtPath],
				basic_setup(new,XArchOS, ArchOS, DHD, Ver, SwitchVals,TopLevel),
				'Ok...exiting\n').
long_dirs(SubDirs, ArchOS, XArchOS, FullTgtPath, DHD, Ver, SwitchVals,TopLevel)
	:-
	throw(long_dirs(SubDirs, ArchOS, XArchOS, FullTgtPath, DHD, Ver, SwitchVals,TopLevel)).

disp_long_dirs('Exit', _, _, _, _, _, _, _, _, _,[])
	:-!,
	printf('Ok .. No choice ...exiting\n').

disp_long_dirs('$noChoice', _, _, _, _, _, _, _, _, _,[])
	:-!,
	printf('Ok .. No choice ...exiting\n').

disp_long_dirs(Action, OverwriteDir, MoveDir, 
				SubDirs, ArchOS, XArchOS, FullTgtPath, DHD, Ver, SwitchVals,TopLevel)
	:-
	sub_atom(Action, 1, 4, 'Move'),
	!,
	long_dirs_mv(SubDirs, ArchOS, XArchOS, FullTgtPath, DHD, Ver, SwitchVals,TopLevel).

disp_long_dirs(Action, OverwriteDir, MoveDir, 
				SubDirs, ArchOS, XArchOS, FullTgtPath, DHD, Ver, SwitchVals,TopLevel)
	:-
	sub_atom(Action, 1, 6, 'Update'),
	basic_setup(update,XArchOS, ArchOS, DHD, Ver, SwitchVals,TopLevel).

long_dirs_mv(SubDirs, ArchOS, XArchOS, FullTgtPath, DHD, Ver, SwitchVals,TopLevel)
	:-
	sprintf(atom(OldDir), 'old-%t', [XArchOS]),
	(exists_file(OldDir) ->
		kill_subdir(OldDir) ; true),
	move_file(XArchOS, OldDir),
	basic_setup(new,XArchOS, ArchOS, DHD, Ver, SwitchVals,TopLevel).

/*--------------------------------------------------------------------------------
		short dir machinery needs checking/fixing/extension....
 *-------------------------------------------------------------------------------*/

short_dirs(SubDirs, ArchOS, XArchOS, FullTgtPath, DHD, Ver, SwitchVals,TopLevel)
	:-
	dmember(ArchOS, SubDirs),
	!,
	ok_or_exit('Move existing directory %t',[FullTgtPath],
				short_dirs_mv(SubDirs, ArchOS, XArchOS, FullTgtPath, DHD, Ver, SwitchVals,TopLevel), 
				'Ok...exiting\n').

short_dirs(SubDirs, ArchOS, XArchOS, FullTgtPath, DHD, Ver, SwitchVals,TopLevel)
	:-
	sprintf(CMDcs3, 'ln -s ./%t %t', [ArchOS, XArchOS]),
	system(CMDcs3),
	basic_setup(new,ArchOS, ArchOS, DHD, Ver, SwitchVals,TopLevel).

short_dirs_mv(SubDirs, ArchOS, XArchOS, FullTgtPath, DHD, Ver, SwitchVals,TopLevel)
	:-
	sprintf(CMDcs1, 'mv %t old-%t', [ArchOS,ArchOS]),
	system(CMDcs1),
	sprintf(CMDcs2, 'rm %t ', [XArchOS]),
	system(CMDcs2),
	sprintf(CMDcs3, 'ln -s ./%t %t', [ArchOS, XArchOS]),
	system(CMDcs3),
	basic_setup(new,ArchOS, ArchOS, DHD, Ver, SwitchVals,TopLevel).

/*--------------------------------------------------------------------------------
 |	basic_setup/7
 |	basic_setup(Mode, XArchOS, ArchOS, DHD, Ver, SwitchVals,TopLevel)
 |
 |	1. If necessary, sets up a new directory for the given ArchOS and version num;
 |		this includes setting up all necessary subdirectories;
 |
 |	2. Copies basic sourcefiles (eg examples files, tools files sources, etc),
 |	   or updates existing files, as appropriate
 |
 |	3. Copies the basic ALS libraries and executables from the distribution,
 |	   including the windows items and items for linking extensions, embedding, etc.
 |
 |	4. Handles as much as possible of packing up the distribution 
 |		(only unix; Mac and Windows us StuffIt and Wise)
 *-------------------------------------------------------------------------------*/
basic_setup(new, XArchOS, ArchOS, DHD, Ver, SwitchVals, TopLevel)
	:-!,
	make_subdir(XArchOS),
	change_cwd(XArchOS),
		%% Now in ..../als_dist/ArchOS;
		%% Create (sub)directories:
	setup_basic_framework(XArchOS, ArchOS, DHD, Ver, 
					  	  SwitchVals, WSL, TopLevel, []),
	asplit(ArchOS,0'_,Arch,OS),
	general_os(Arch,OS,GOS),
		%% Get (common) source files:
	copy_basic_sourcefiles(XArchOS, ArchOS, DHD, Ver, SwitchVals, WSL, GOS, new),
		%% Get the executables:
	copy_basic_build(XArchOS, ArchOS, DHD, Ver, SwitchVals, WSL, new),
		%% Now in ..../als_dist/ArchOS;
	system('rm -f *.obp'),
	printf('Finished creating & populating: %t\n',[XArchOS]),

	handle_packing(XArchOS, ArchOS, TopLevel),

	univ_change_cwd(path(relative, [directory(parent)])),
		%% Now in ..../als_dist;
	system('rm -f *.obp').

basic_setup(update, XArchOS, ArchOS, DHD, Ver, SwitchVals, [])
	:-
	change_cwd(XArchOS),
		%% Now in ..../als_dist/ArchOS;
		%% Update (common) source files:
	asplit(ArchOS,0'_,Arch,OS),
	general_os(Arch,OS,GOS),
	(winsystems_for(ArchOS, WSL), !; WSL = []),
	copy_basic_sourcefiles(XArchOS, ArchOS, DHD, Ver, SwitchVals, WSL, GOS, update),
		%% Get the executables:
	copy_basic_build(XArchOS, ArchOS, DHD, Ver, SwitchVals, WSL, update),
		%% Now in ..../als_dist/ArchOS;
	system('rm -f *.obp'),
	printf('Finished updating: %t\n',[XArchOS]),

	files('*',TopLevelFiles),
	subdirs_red(TopLevelSubdirs),
	append(TopLevelFiles, TopLevelSubdirs, TopLevel),
	handle_packing(XArchOS, ArchOS, TopLevel),

	univ_change_cwd(path(relative, [directory(parent)])),
		%% Now in ..../als_dist;
	system('rm -f *.obp').


univ_make_subdir(path(relative, PathList))
	:-
	rootPathFile('', PathList, '', NativePath),
	make_subdir(NativePath).

univ_change_cwd(Path)
	:-
	univ_change_cwd0(Path).
univ_change_cwd(_)
	:-
	throw(univ_change_cwd_error).

parent_dir(macos, '::').
parent_dir(mswin32, '..').
parent_dir(unix, '..').

univ_change_cwd0(path(relative, [directory(parent) | Rest]))
	:-
	sys_env(OS,_,_),
	parent_dir(OS, ParentString),
	change_cwd(ParentString),
	univ_change_cwd0(path(relative, Rest)).
	
univ_change_cwd0(path(relative, [FirstElement | Rest]))
	:-
	atom(FirstElement),
	change_cwd(FirstElement),
	univ_change_cwd0(path(relative, Rest)).
univ_change_cwd0(path(relative, [])).

/*--------------------------------------------------------------------------------
 | setup_basic_framework/8
 | setup_basic_framework(XArchOS, ArchOS, DHD, Ver, SwitchVals, WSL, TLIn,TLOut)
 | setup_basic_framework(+, +, +, +, +, - +,-)
 |
 |	XArchOS	= ArchOS-Ver
 |	ArchOS	= <Arch>_<OS>
 |	DHD		= Distribution home (top level) directory
 |	Ver		= This version number (quoted atom)
 |	SwitchVals	= so far, only [['ver', Ver]]
 |	WSL		= list of window systems (atomic identifiers) for this ArchOS
 |	TLIn	= list of top level directories and files created by this rouine
 |	TLOut	= (uninstantiated) tail of TLIn
 *-------------------------------------------------------------------------------*/
setup_basic_framework(XArchOS, ArchOS, DHD, Ver, SwitchVals, WSL, TLIn,TLOut)
	:-
	asplit(ArchOS,0'_,Arch,OrigOS),
	general_os(Arch,OrigOS,OS),
	readme_files(ReadmeList),
	get_path_to(readme_dir, '', ReadmePath),
	copy_list_here(ReadmeList, ReadmePath, OS),
	append(ReadmeList, TLInter1, TLIn),

	(NewOrUpdate = new -> 
		univ_make_subdir(path(relative, [alsdir])),
		univ_make_subdir(path(relative, [alsdir,builtins])),
		univ_make_subdir(path(relative, [alsdir,library])),
		printf('alsdir created...\n',[]),

		univ_make_subdir(path(relative, [tools])),
		univ_make_subdir(path(relative, [tools,cref])),
		univ_make_subdir(path(relative, [tools,macros])),
		univ_make_subdir(path(relative, [linking])),
		univ_make_subdir(path(relative, [linking,cinterf])),
		univ_make_subdir(path(relative, [linking,cinterf,c2pro])),
		univ_make_subdir(path(relative, [linking,cinterf,pro2intf])),
		printf('tools created...\n',[]),

		univ_make_subdir(path(relative, [examples])),
		univ_make_subdir(path(relative, [examples,als])),
		univ_make_subdir(path(relative, [examples,pxs])),
		univ_make_subdir(path(relative, [examples,more])),
		printf('examples created...\n',[])
		;
		true
	),

	TLInter1 = [alsdir, examples, threaded, native, windows | TLInter2],

	(winsystems_for(ArchOS, WSL), !; WSL = []),
	append(WSL, TLOut, TLInter2),

	(NewOrUpdate = new -> 
		make_each_subdir([threaded, native, windows]),
		make_each_subdir(WSL),
		printf('threaded/native/windows dirs created...\n',[])
		;
		true
	),

	((NewOrUpdate = new, WSL \= []) ->
		univ_make_subdir(path(relative, [windows,guilib])),
		univ_make_subdir(path(relative, [windows,guigen])),
		univ_make_subdir(path(relative, [windows,samples])),
		univ_make_subdir(path(relative, [windows,utils])),

		change_cwd(windows),
		make_each_subdir(WSL),
		change_cwd(samples),
		make_each_subdir(WSL),
		univ_change_cwd(path(relative, [directory(parent),guilib])),
		make_each_subdir(WSL),
		printf('windows subdirs created...\n',[]),
		univ_change_cwd(path(relative, [directory(parent), directory(parent)]))
		;
		true
	).

/*--------------------------------------------------------------------------------
 | copy_basic_sourcefiles/8
 | copy_basic_sourcefiles(XArchOS, ArchOS, DHD, Ver, SwitchVals, WSL, GOS, NewUp)
 | copy_basic_sourcefiles(XArchOS, ArchOS, DHD, Ver, SwitchVals, WSL, GOS, NewUp)
 |
 |	XArchOS	= ArchOS-Ver
 |	ArchOS	= <Arch>_<OS>
 |	DHD		= Distribution home (top level) directory
 |	Ver		= This version number (quoted atom)
 |	SwitchVals	= so far, only [['ver', Ver]]
 |	WSL		= list of window systems (atomic identifiers) for this ArchOS
 |	GOS		= general OS of ArchOS, in sense of general_os/4 in sconfig.pro / library
 |	NewUp	= 'new' or 'update'
 *-------------------------------------------------------------------------------*/
exampleSubDir(macos, ':examples').
exampleSubDir(_, examples).

relSubPath(List, Path) :-
	sys_env(OS, _, _),
	relSubPath0(List, Path, OS).
	
relSubPath0(List, Path, unix) :- subPath(List, Path).
relSubPath0(List, Path, mswin32) :- subPath(List, Path).

relSubPath0([], '', macos).
relSubPath0([C], Path, macos) :-
	builtins:directory_separator(S),
	atom_concat(S, C, Path).
relSubPath0([C | Cs], Path, macos) :-
	builtins:directory_separator(S),
	relSubPath0(Cs,SubPath,macos),
	!,
	atom_concat(S, C, TopPath),
	atom_concat(TopPath, SubPath, Path).

copy_basic_sourcefiles(XArchOS, ArchOS, DHD, Ver, SwitchVals, WSL, GOS, NewUp)
	:-
		%% What is the general target OS:
	asplit(ArchOS,0'_,Arch,OrigOS),
	general_os(Arch,OrigOS,OS),

		%% Copy over (or update) the examples directories:
	get_path_to(examples_dir, '', ExamplesPath),
		%% examples/als:
	extendPath(ExamplesPath, als, ALSExamplesPath),
	exampleSubDir(OS, ExSubDir),
	extendPath(ExSubDir, als, TgtALSExamples),
	copy_dir_files_nl(ALSExamplesPath, TgtALSExamples, OS, NewUp),
		%% examples/pxs:
	extendPath(ExamplesPath, pxs, PXSExamplesPath),
	extendPath(ExSubDir, pxs, TgtPXSExamples),
	copy_dir_files_nl(PXSExamplesPath, TgtPXSExamples, OS, NewUp),
		%% examples/more:
	extendPath(ExamplesPath, more, MOREExamplesPath),
	extendPath(ExSubDir, more, TgtMOREExamples),
	copy_dir_files_nl(MOREExamplesPath, TgtMOREExamples, OS, NewUp),
	printf('examples files copied...\n',[]),
	
		%% Copy over (or update) the cinterf generator directories:
	get_path_to(source_dir, '', ALSSRCSourcePath),
	extendPath(ALSSRCSourcePath, cinterf, CINTFSourcePath),
	extendPath(CINTFSourcePath, c2pro, 	  C2PROSrcPath),
	extendPath(CINTFSourcePath, pro2intf, P2ISrcPath),
	relSubPath([linking,cinterf,c2pro],    TgtC2PROPath),
	relSubPath([linking,cinterf,pro2intf], TgtP2IPath),
	source_list(c2pro, 	  C2PROFiles, _),
	source_list(pro2intf, P2IFiles,   _),
		%% linking/cinterf/c2pro:
	copy_fileslist_nl(C2PROFiles, C2PROSrcPath, TgtC2PROPath, OS, NewUp),
		%% linking/cinterf/pro2intf:
	copy_fileslist_nl(P2IFiles, P2ISrcPath, TgtP2IPath, OS, NewUp),
	printf('cinterf files copied...\n',[]),

	get_path_to(als_dev_dir, '', ALSDEVSrcPath),
	extendPath(ALSDEVSrcPath, tools, TOOLSSrcPath),
	extendPath(TOOLSSrcPath, cref, CREFSrcPath),
	relSubPath([tools,cref],    TgtCREFPath),
	copy_dir_files_nl(CREFSrcPath, TgtCREFPath, OS, NewUp),

	extendPath(TOOLSSrcPath, macros, MACROSrcPath),
	relSubPath([tools,macros],    TgtMACROPath),
	copy_dir_files_nl(MACROSrcPath, TgtMACROPath, OS, NewUp),

	printf('tools files copied...\n',[]),

	copy_windev_sourcefiles(WSL,XArchOS, ArchOS, DHD, Ver, SwitchVals, OS, NewUp).

/*--------------------------------------------------------------------------------
 *-------------------------------------------------------------------------------*/
copy_windev_sourcefiles([], XArchOS, ArchOS, DHD, Ver, SwitchVals, OS, NewUp) :-!.

copy_windev_sourcefiles(WSL, XArchOS, ArchOS, DHD, Ver, SwitchVals, OS, NewUp)
	:-
	printf('Setting up for window systems: %t\n', [WSL]),
	get_path_to(windows_dir, '', WinSourcePath),

		%'windows/guilib'
	extendPath(WinSourcePath, guilib, GUILIBSrcPath),
	source_list(guilib,  GUILIBFs, GUIXCL),
	relSubPath([windows,guilib], TgtGUILIBDir),
	test_and_copy(GUILIBFs, GUIXCL, GUILIBSrcPath, TgtGUILIBDir, OS, NewUp),

	copy_each_subdir(WSL, GUILIBSrcPath, ['*.pro','*.res'],TgtGUILIBDir,OS,NewUp),

		%'windows/guigen'
	extendPath(WinSourcePath, guigen, GUIGENSrcPath),
	relSubPath([windows,guigen], TgtGUIGENDir),
	test_and_copy(['*.pro'], [], GUIGENSrcPath, TgtGUIGENDir, OS, NewUp),

		%'windows/samples'
	extendPath(WinSourcePath, samples, SAMPLESrcPath),
	source_list(gui_samples,  GUISAMPFs, SAMPXCL),
	relSubPath([windows,samples], TgtSAMPDir),
	test_and_copy(GUISAMPFs, SAMPXCL, SAMPLESrcPath, TgtSAMPDir, OS, NewUp),

	copy_each_subdir(WSL, SAMPLESrcPath, ['*.pro','*.res'], TgtSAMPDir, OS, NewUp),

		%% 'windows/utils'
	extendPath(WinSourcePath, utils, UTILSrcPath),
	relSubPath([windows,utils], TgtUTILDir),
	test_and_copy(['*.pro'], [], UTILSrcPath, TgtUTILDir, OS, NewUp),

	relSubPath([windows], TgtWinDir),
	copy_each_subdir(WSL, WinSourcePath, ['*.pro'], TgtWinDir, OS, NewUp),

	printf('windows common source copied...\n',[]).


/*--------------------------------------------------------------------------------
 *-------------------------------------------------------------------------------*/
copy_basic_build(XArchOS, ArchOS, DHD, Ver, SwitchVals, WSL, NewUp)
	:-
		%% What is the general target OS:
	asplit(ArchOS,0'_,Arch,OrigOS),
	general_os(Arch,OrigOS,OS),
	get_path_to(build_dir, ArchOS, ThisBuildPath),
	copy_builds(ArchOS, XArchOS, ThisBuildPath, DHD, Ver, SwitchVals, WSL,OS,NewUp).

/*--------------------------------------------------------------------------------
 |	copy_builds/9
 |	copy_builds(ArchOS, XArchOS, BuildPath, DHD, Ver, SwitchVals, WSL,GOS,NewUp)
 |	copy_builds(+, +, +, +, +, +, +,+,+)
 |
 |	XArchOS		= ArchOS-Ver
 |	ArchOS		= <Arch>_<OS>
 |	BuildPath	= path to the current source build directory
 |	DHD			= Distribution home (top level) directory
 |	Ver			= This version number (quoted atom)
 |	SwitchVals	= so far, only [['ver', Ver]]
 |	WSL			= list of window systems for this ArchOS
 |	GOS			= general OS of ArchOS {general_os/4 - sconfig.pro - library}
 |	NewUp		= 'new' or 'update'
 *-------------------------------------------------------------------------------*/
copy_builds(ArchOS, XArchOS, BuildPath, DHD, Ver, SwitchVals, WSL,OS,NewUp)
	:-
		%% Portable version executables & libraries:
	als_executables_list(ArchOS, PortList, MainExecutable),
	pathPlusFile(BuildPath,'bld-port',BldPortPath),
	relSubPath([threaded], ThreadedDir),
	test_and_copy(PortList, [], BldPortPath, ThreadedDir, OS, NewUp),
	(filePlusExt(BaseFile,Ext,MainExecutable) ->
		true
		;
		MainExecutable = BaseFile, Ext = no(ext)
	),
	(native_als_ok(ArchOS) ->
		test_xccmp_copy(NewUp,BaseFile,Ext,MainExecutable,BldPortPath,threaded,[],OS)
		;
		%%test_xccmp_copy(NewUp,BaseFile,Ext,MainExecutable,BldPortPath,'.',[],OS)
		test_xccmp_copy(NewUp,BaseFile,Ext,MainExecutable,BldPortPath,'',[],OS)
	),
	printf('threaded executables copied...\n',[]),

		%% Portable version als-mics:
	pathPlusFile(BldPortPath, alsdir, BldPortAlsdirPath),
	test_and_copy(['als-mics','als-mics.exe'], BldPortAlsdirPath, ThreadedDir),
	printf('threaded als-mics copied...\n',[]),

		%% Portable version alsdir/builtins:
	pathPlusFile(BldPortAlsdirPath, builtins,BldPortAlsdirBltsPath),
	source_list(builtins, BltsPatternList, BltsExcludeNamesList),
	relSubPath([alsdir,builtins], TgtBuiltins),
	test_and_copy(BltsPatternList, BltsExcludeNamesList, 
					BldPortAlsdirBltsPath, TgtBuiltins, OS, NewUp),

		%% Portable version alsdir/library:
	pathPlusFile(BldPortAlsdirPath, library,BldPortAlsdirLibPath),
	source_list(library, LibPatternList, LibExcludeNamesList),
	relSubPath([alsdir,library], TgtLibrary),
	test_and_copy(LibPatternList, LibExcludeNamesList, 
					BldPortAlsdirLibPath, TgtLibrary, OS, NewUp),

	printf('alsdir files copied...\n',[]),

		%% Files for linking/extending/embedding (linking):

	relSubPath([linking],TgtLINKDir),
	source_list(linking_top, TopLinkPatternList, TopLinkExclList),
	test_and_copy(TopLinkPatternList, TopLinkExclList, BuildPath, TgtLINKDir, OS, NewUp),

	link_examples(ArchOS, LEXDir, LinkExsPatternList),
	test_and_copy(LinkExsPatternList, [], LEXDir, TgtLINKDir, OS, NewUp),
	
	get_path_to(source_dir, '', ALSSRCSourcePath),

	extendPath(ALSSRCSourcePath,generic,GENERICPath),
	source_list(linking_generic, GenLinkPatternList, GenLinkExclList),
	test_and_copy(GenLinkPatternList, GenLinkExclList, 
					GENERICPath, TgtLINKDir, OS, NewUp),
	printf('linking files copied...\n',[]),

			%% Native version executables & libraries:
	(native_als_ok(ArchOS) ->
		als_executables_list(ArchOS, NatvList, MainExecutable),
		pathPlusFile(BuildPath,'bld-natv',BldNatvPath),
		test_and_copy(NatvList, [], BldNatvPath, native, OS, NewUp),
		test_xccmp_copy(NewUp,BaseFile,Ext,MainExecutable,BldNatvPath,'.',[],OS),
		printf('native executables copied...\n',[]),
			%% Native version als-mics:
		pathPlusFile(BldNatvPath,alsdir,BldNatvAlsdirPath),
		test_and_copy(['als-mics','als-mics.exe'], BldNatvAlsdirPath, native),
		printf('native als-mics copied...\n',[])
		;
		true
	),

	pathPlusFile(BuildPath,'bld-wins',BldWinsPath),
	copy_wins(WSL, ArchOS, XArchOS, BldWinsPath, DHD, Ver, SwitchVals, OS, NewUp).


/*--------------------------------------------------------------------------------
 | copy_wins/9
 | copy_wins(WSL, ArchOS, XArchOS, BldWinsPath, DHD, Ver, SwitchVals, OS, NewUp)
 | copy_wins(+, +, +, +, +, +, +, +, +)
 |
 |	Installs the windows executabless:
 |	Recursively handles each entry on WSL using copy_win_dir/9
 *-------------------------------------------------------------------------------*/
copy_wins([], ArchOS, XArchOS, BldWinsPath, DHD, Ver, SwitchVals, OS, NewUp).
copy_wins([WinDir | WSL], ArchOS, XArchOS, BldWinsPath, DHD, Ver, SwitchVals, OS, NewUp)
	:-
	copy_win_dir(WinDir, ArchOS, XArchOS, BldWinsPath, DHD, Ver, SwitchVals, OS, NewUp),
	copy_wins(WSL, ArchOS, XArchOS, BldWinsPath, DHD, Ver, SwitchVals, OS, NewUp).

/*--------------------------------------------------------------------------------
 | copy_win_dir/9
 | copy_win_dir(WinDir, ArchOS, XArchOS, BldWinsPath, DHD, Ver, SwitchVals, OS, NewUp)
 | copy_wins(+, +, +, +, +, +, +, +, +)
 |
 |	Sets up and installs executables and associated sources for the window
 |	system "WinDir"
 *-------------------------------------------------------------------------------*/
copy_win_dir(WinDir, ArchOS, XArchOS, BldWinsPath, DHD, Ver, SwitchVals, OS, NewUp)
	:-
		%% Check & create, if necessary, the subdirectories:
	relSubPath([WinDir], WinDirStr),
	test_and_make_subdir(WinDirStr),
	extendPath(WinDirStr,src,TgtWinDirSrc),
	test_and_make_subdir(TgtWinDirSrc),
	extendPath(WinDirStr,tests,TgtWinDirTest),
	test_and_make_subdir(TgtWinDirTest),

	extendPath(BldWinsPath,WinDir,BldWinsWinDir),

	source_list(wins_intf_general, SrcWinsList, XclXrcWinsList),
	wins_ex_list(ArchOS, WinsXecList),
	append(WinsXecList, SrcWinsList, WinsList0),
	filePlusExt(WinDir, pro, WinPro),
	WinsList = [ WinPro | WinsList0],

	test_and_copy(WinsList, XclXrcWinsList, BldWinsWinDir, WinDirStr, OS, NewUp),

	asplit(ArchOS, 0'_, ARCH, AOS),
	(ws_vars(WinDir, ARCH, AOS, [], WSHeaderLines) ->
		(dmember('ADDL_CS'= AddlWinFs, WSHeaderLines),! ; AddlWinFs = [])
		;
		AddlWinFs = []
	),
	filePlusExt(WinDir, '*', WinWild),
	SrcWinsList2 = [WinWild |AddlWinFs],

	wins_intf_path(IntfWinsPath),

	extendPath(IntfWinsPath,src,IntfSrcPath),
	test_and_copy(SrcWinsList2, [], IntfSrcPath, TgtWinDirSrc, OS, NewUp),

	extendPath(IntfWinsPath,build,IntfBldPath0),
	extendPath(IntfBldPath0,WinDir,IntfBldPathTestsDir),
	test_and_copy(['*test*.pro'], [], IntfBldPathTestsDir, TgtWinDirTest, OS, NewUp),

	printf('wins dir >> %t << copied...\n',[WinDir]).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%        UTILITIES
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*--------------------------------------------------------------------------------
 *-------------------------------------------------------------------------------*/
ok_or_exit(QString,QArgs, ContinueCall,  ExitString)
	:-
	catenate(QString, ' -- ok?(y or n)', XQString),
	printf(XQString, QArgs),
	flush_output,
	get_code(C),
	flush_input(user_input),
	disp_ok_or_exit(C,ContinueCall,  ExitString).

disp_ok_or_exit(C,ContinueCall,  ExitString)
	:-
	is_yes(C),
	!,
	call(ContinueCall).

disp_ok_or_exit(C,ContinueCall,  ExitString)
	:-
	printf(ExitString, []).

is_yes(0'y).
is_yes(0'Y).


/*--------------------------------------------------------------------------------
 *-------------------------------------------------------------------------------*/
copy_here(Path, File, OS)
	:-
	pathPlusFile(Path, File, FilePath),
	copy_file_nl(FilePath, File, OS).

copy_list_here([], Path,_).
copy_list_here([File | ReadmeList], Path,OS)
	:-
	copy_here(Path, File, OS),
	copy_list_here(ReadmeList, Path, OS).

/*
copy_files(Path, File, Target)
	:-
	pathPlusFile(Path, File, FromFile),
	pathPlusFile(Target, File, ToFile),
	copy_file(FromFile, ToFile).

copy_files(Path, File, Target)
	:-
	sprintf(CMDcs, 'cp -r %t/%t %t', [Path,File,Target]),
	system(CMDcs),
	sprintf(RMCVSCMD, 'rm -r %t/CVS', [Target]),
	system(RMCVSCMD),
	(pathPlusFile(FPath,_,File) ->
		sprintf(RMCVSCMDsub, 'rm -r %t/%t/CVS', [Target,FPath]),
		system(RMCVSCMDsub)
		;
		true
	).
*/

copy_file(FromFile, ToFile) :-
	pbi_copy_file(FromFile, ToFile), !.
copy_file(FromFile, ToFile) :-
	throw(copy_file_error(FromFile, ToFile)).


/*--------------------------------------------------------------------------------
 *-------------------------------------------------------------------------------*/
test_and_copy(Files, SrcPath, TgtPath)
	:-
	sys_env(OS,_,_),
	test_and_copy(Files, [], SrcPath, TgtPath,OS,new).


test_and_copy('', _, SrcPath, TgtPath, _,_).

test_and_copy([], _, SrcPath, TgtPath, _,_).

test_and_copy([File | Files], Exclude, SrcPath, TgtPath, OS, NewUp)
	:-
	pathPlusFile(SrcPath, File, SrcFilePath),
	exists_file(SrcPath),
	directory(SrcFilePath, [regular,symbolic_link], MatchingFiles),
	!,
	list_test_and_copy(MatchingFiles,SrcPath,TgtPath,Exclude,OS,NewUp),
	test_and_copy(Files, Exclude, SrcPath, TgtPath, OS, NewUp).

test_and_copy([File | Files], Exclude, SrcPath, TgtPath, OS, NewUp)
	:-
	printf('Warning: No files match %t in dir %t\n',[File,SrcPath]),
	test_and_copy(Files, Exclude, SrcPath, TgtPath, OS, NewUp).

/*--------------------------------------------------------------------------------
 *-------------------------------------------------------------------------------*/
test_and_copy_file(SrcFilePath, TgtPath)
	:-
	directory(SrcFilePath, [regular,symbolic_link], [_|_]),
	!,
	bin_copy_file_2dir(SrcFilePath, TgtPath).

test_and_copy_file(SrcFilePath, TgtPath).

/*--------------------------------------------------------------------------------
 *-------------------------------------------------------------------------------*/
list_test_and_copy([],_,_,_,_,_).

			%% Note: File exsists
list_test_and_copy([File|MatchingFiles],SrcPath,TgtPath,Exclude,OS,NewUp)
	:-
	(filePlusExt(BaseFile, Ext, File),!; Ext = no(ext),BaseFile=File),
	test_xccmp_copy(NewUp,BaseFile,Ext,File,SrcPath,TgtPath,Exclude,OS),
	list_test_and_copy(MatchingFiles,SrcPath,TgtPath,Exclude,OS,NewUp).

/*--------------------------------------------------------------------------------
 *-------------------------------------------------------------------------------*/
test_xccmp_copy(NewUp,BaseFile,Ext,File,SrcPath,TgtPath,Exclude,OS)
	:-
	dmember(BaseFile,Exclude),
	!,
	filePlusExt(BaseFile, obp, ObpFile),
	pathPlusFile(SrcPath,ObpFile,ObpFilePath),
	(exists_file(ObpFilePath) ->
		test_and_copy_file(ObpFilePath, TgtPath)
		;
		printf('Warning: %t [%t] excluded for target %t, BUT:\n', 
				[File,BaseFile,TgtPath]),
		printf('  %t doesn\'t exist...skipping\n\n', [ObpFilePath])
	).

test_xccmp_copy(NewUp,BaseFile,Ext,File,SrcPath,TgtPath,_,OS)
	:-
	(is_text_file(Ext); BaseFile = makefile ; BaseFile = 'Makefile'),
	!,
	pathPlusFile(SrcPath, File, SrcFile),
	pathPlusFile(TgtPath, File, TgtFile),
	check_copy_file_nl(NewUp, SrcFile, TgtFile, OS).

test_xccmp_copy(NewUp,BaseFile,Ext,File,SrcPath,TgtPath,Exclude,OS)
	:-
	pathPlusFile(SrcPath, File, SrcFilePath),
	pathPlusFile(TgtPath, File, TgtFilePath),
	(comp_file_times(SrcFilePath,TgtFilePath) ->
		true
		;
		bin_copy_file_2dir(SrcFilePath, TgtPath)
	).

/*--------------------------------------------------------------------------------
 *-------------------------------------------------------------------------------*/
is_text_file(pro).
is_text_file(c).
is_text_file(cc).
is_text_file(h).
is_text_file(mkf).
is_text_file(mak).
is_text_file(typ).
is_text_file(mac).
is_text_file(txt).
is_text_file(src).
is_text_file(lst).
is_text_file(bat).
is_text_file(html).
is_text_file(htm).

/*--------------------------------------------------------------------------------
 *-------------------------------------------------------------------------------*/

bin_copy_file_2dir(SrcFilePath, TgtPath)
	:-
	pathPlusFile(Path, File, SrcFilePath),
	pathPlusFile(TgtPath, File, TgtFilePath),
	copy_file(SrcFilePath, TgtFilePath).

/*
bin_copy_file_2dir(SrcFilePath, TgtPath)
	:-
	sprintf(CMDcs, 'cp %t %t', [SrcFilePath, TgtPath]),
	system(CMDcs).
*/
/*--------------------------------------------------------------------------------
 *-------------------------------------------------------------------------------*/
make_each_subdir([]).
make_each_subdir([SD | Subdirs])
	:-
	make_subdir(SD),
	make_each_subdir(Subdirs).

test_and_make_subdir(SubDir)
	:-
	exists_file(SubDir), !.

test_and_make_subdir(SubDir)
	:-
	make_subdir(SubDir).


copy_each_subdir([], _,_,_,_,_).
copy_each_subdir([SD | SDL], SrcParentD, FilesL, TgtParentD, OS, NewUp)
	:-
	extendPath(SrcParentD, SD, SrcDir),
	extendPath(TgtParentD, SD, TgtSUBD),
	test_and_copy(FilesL, [], SrcDir, TgtSUBD, OS, NewUp),
	copy_each_subdir(SDL, SrcParentD, FilesL, TgtParentD, OS, NewUp).


/*--------------------------------------------------------------------------------
 *-------------------------------------------------------------------------------*/
handle_packing(XArchOS, ArchOS, TopLevel)
	:-
	asplit(ArchOS, 0'_, Arch, FOS),
	general_os(Arch, FOS, OS, GOS),
	refine_os(OS, GOS, ROS),
	handle_packing(ROS, XArchOS, ArchOS, TopLevel).

refine_os(dj2, _, dos) :-!.
refine_os(OS, GOS, GOS).

handle_packing(unix, XArchOS, ArchOS, TopLevel)
	:-!,
	sprintf(InitPackHeadCs, 'tar -cf ./%t.tar ', [XArchOS]),
	atom_codes(InitPackHead, InitPackHeadCs),
	cat_together_spaced([InitPackHead | TopLevel], InitPack),
	printf('Executing %t\n',[InitPack]),
	system(InitPack),
	sprintf(FinalPack, 'gzip %t.tar', [XArchOS]),
	printf('Executing %t\n',[FinalPack]),
	system(FinalPack),
	system('mv *.gz ..'),
	system('rm -f *.obp').

handle_packing(dos, XArchOS, ArchOS, TopLevel)
	:-!,
	open('zip2net.bat', write, OStream1, []),
	write(OStream1, 'pkzip i386_dj2 -rp -x*.zip'), nl(OStream1),
	close(OStream1),
	open('zip2disk.bat', write, OStream2, []),
	write(OStream2, 'pkzip A:i386_dj2 -rp -x*.zip'), nl(OStream2),
	close(OStream2),
	printf('To finishing the distribution for i386_dj2,\n',[]),
	printf('On DOS - in ~builds\%t , execute the following:\n',[XArchOS]), 
	printf('1.  Run zip2net.bat\n',[]),
	printf('2.  mv i386_dj2.zip ..\n', []),
	printf('3.  For disks, execute: zip2disk.bat \n',[]),
	system('rm -f *.obp').
	

handle_packing(ROS, XArchOS, ArchOS, TopLevel)
	:-!,
	printf('To finishing the distribution for %t ,\n',[ArchOS]),
	printf('  Use the native install for this platform.\n',[]).


endmod.
