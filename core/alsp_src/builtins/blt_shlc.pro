/*================================================================*
 |		blt_shlc.pro
 |	Copyright (c) 1998 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	Commond stuff for blt_shl/blt_dvsh/blt_cslt
 |
 |	Original Creation Date: 7/98
 *================================================================*/

module builtins.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Common command-line processing routines
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_command_line_info(DefaultShellCall,CommandLine,ResidualCommandLine,Mod,CLInfo)
	:-
	%% Get the raw command line and process it.
	abolish(command_line,1),
	pbi_get_command_line(RawCommandLine),

	%% get the command line, but ignore the image name
	(RawCommandLine = [Image | CommandLine] ; CommandLine = []),
	!,
	ss_parse_command_line(CommandLine, ResidualCommandLine, Mod, CLInfo).

/*-----------------------------------------------------------------*
 | ss_parse_command_line/4
 | ss_parse_command_line(CommandLine, ResidualCommandLine, Mod, CLInfo),
 | ss_parse_command_line(+, -, +, +/-),
 *-----------------------------------------------------------------*/

	%% Empty/end of command line:
ss_parse_command_line([], [], _, CLInfo)
	:-!.

	%% -p: Start application part of command line:
ss_parse_command_line(['-p' | T], T, _, CLInfo)
	:-!.

	%% -P: Start application part of command line, pushing on image name:
ss_parse_command_line(['-P' | T], [ImageName | T], _, CLInfo)
	:-!,
	arg(4,CLInfo,ImageName).

/*
	%% -ind: "Indirect" file: open, read 1 line, process the line, & continue:
ss_parse_command_line(['-ind', File | T], L, Mod, CLInfo)
	:-!,
	open(File,read,IS,[]),
	get_line(IS, Line),
	close(IS),
	atomread(Line, IndCmdLine),
	append(IndCmdLine, T, RestCL),
	ss_parse_command_line(RestCL, L, Mod, CLInfo).
*/

	%% -g <Goal>: Start up goal:
ss_parse_command_line(['-g', GoalAtom | T], L, Mod, CLInfo)
	:-!,
	atom_codes(GoalAtom, GoalCodes),
	term_codes(Goal, GoalCodes),	%% FIXME: Catch syntax errors
	mangle(1,CLInfo,Goal),
	ss_parse_command_line(T,L,Mod,CLInfo).

	%% -b: "Batch" mode: exit after running -g startup goal (don't run default shell):
ss_parse_command_line(['-b' | T], L, Mod,CLInfo)
	:-!,
	mangle(7, CLInfo, true),
	ss_parse_command_line(T, L, Mod,CLInfo).

	%% -v: Turn on verbose mode:
ss_parse_command_line(['-v' | T], L, Mod,CLInfo)
	:-!,
	mangle(2, CLInfo, false),
	assert(global_verbosity(noisy)),
	ss_parse_command_line(T, L, Mod,CLInfo).

	%% -q: Turn off verbose mode: (set = "true": be quiet)
ss_parse_command_line(['-q' | T], L, Mod,CLInfo)
	:-!,
	mangle(2, CLInfo, true),
	assert(global_verbosity(quiet)),
	ss_parse_command_line(T, L, Mod,CLInfo).

	%% -s: Atom - File to add to search list;
	%% in reverse order; later reverse it:
ss_parse_command_line(['-s', File | T], L, Mod,CLInfo)
	:-!,
	arg(6, CLInfo, PrevSL),
	mangle(6, CLInfo, [File | PrevSL]),
	ss_parse_command_line(T, L, Mod,CLInfo).

	%% -a: Atom - Assert Atom in module user.
	%% -A: Atom - Assert Atom in module user.
ss_parse_command_line(['-A', Expr | T], L, Mod,CLInfo)
	:-!,
	ss_parse_command_line(['-a', Expr | T], L, Mod,CLInfo).

ss_parse_command_line(['-a', Expr | T], L, Mod,CLInfo)
	:-!,
	arg(8, CLInfo, PrevCmdLineAsserts),
	mangle(8, CLInfo, [Expr | PrevCmdLineAsserts]),
	ss_parse_command_line(T, L, Mod,CLInfo).

	%% For historical compatability:
	%% -obp: Keep obp files in directory where image is running:
ss_parse_command_line(['-obp' | T], L, Mod,CLInfo)
	:-!,
%	generated_in_cur,
	assert(global_obp_location(gic)),
	ss_parse_command_line(T, L, Mod,CLInfo).

	%% "Generated In Current directory:"	
	%% -gic: Keep generated files in directory where image is running:
ss_parse_command_line(['-gic' | T], L, Mod,CLInfo)
	:-!,
%	generated_in_cur,
	assert(global_obp_location(gic)),
	ss_parse_command_line(T, L, Mod,CLInfo).

	%% "Generated In Source directory:"	
	%% -gis: Keep generated files in directory where sources reside:
ss_parse_command_line(['-gis' | T], L, Mod,CLInfo)
	:-!,
%	generated_with_src,
	assert(global_obp_location(gis)),
	ss_parse_command_line(T, L, Mod,CLInfo).

	%% "Generated In Architecture sub-directory of Sources directory:"	
	%% -gias: Keep generated files in arch subdirectory where sources reside:
ss_parse_command_line(['-gias' | T], L, Mod,CLInfo)
	:-!,
%	generated_in_arch(src),
	assert(global_obp_location(gias)),
	ss_parse_command_line(T, L, Mod,CLInfo).

	%% "Generated In Architecture sub-directory of Current directory:"	
	%% -giac: Keep generated files in arch subdirectory of current dir:
ss_parse_command_line(['-giac' | T], L, Mod,CLInfo)
	:-!,
%	generated_in_arch(cur),
	assert(global_obp_location(gias)),
	ss_parse_command_line(T, L, Mod,CLInfo).

	%% "Generated In exlicipt Location:"
	%% -gil: Keep generated files in explict Path dir:
ss_parse_command_line(['-gil', Path | T], L, Mod,CLInfo)
	:-!,
%	generated_in_locn(Path),
	assert(global_obp_location(gil(Path))),
	ss_parse_command_line(T, L, Mod,CLInfo).

	%% -no_obp: Don't generate obp files:
ss_parse_command_line(['-no_obp', Path | T], L, Mod,CLInfo)
	:-!,
%	generated_in_locn(Path),
	assert(global_obp_location(no_obp)),
	ss_parse_command_line(T, L, Mod,CLInfo).


	%% -nwd: Set debugger to "nowins"
ss_parse_command_line(['-nwd' | T], L, Mod,CLInfo)
	:-!,
	debugger:nospy,
	(debugger:set_debug_io(nowins),!;true),
	assert(cl_debug_io(nowins)),
	ss_parse_command_line(T, L, Mod,CLInfo).

	/* Skip -heap and -stack arguments because they 
       must be handled at the C level. */
ss_parse_command_line(['-heap', _ | T], L, Mod,CLInfo)
	:-!,
	ss_parse_command_line(T, L, Mod,CLInfo).

ss_parse_command_line(['-stack', _ | T], L, Mod,CLInfo)
	:-!,
	ss_parse_command_line(T, L, Mod,CLInfo).

	%% Specify non-default shell:
	%% -shell Shell : Use shell (alsdev only known):
ss_parse_command_line(['-shell', ShellName | T], L, Mod,CLInfo)
	:-!,
	mangle(7, CLInfo, ShellName),
	ss_parse_command_line(T, L, Mod,CLInfo).

	%% -no_dot_alspro: suppress loading .alspro (or alspro.pro):
ss_parse_command_line(['-no_dot_alspro' | T], L, Mod,CLInfo)
	:-!,
	mangle(9, CLInfo, false),
	ss_parse_command_line(T, RestL, Mod,CLInfo).

	%% Otherwise: should be file for special processing:
ss_parse_command_line([File | T], L, Mod,CLInfo)
	:-
	Mod:special_ss_parse_command_line(File, T, NT, CLInfo),
	ss_parse_command_line(NT, L, Mod, CLInfo).


setup_search_dirs(CLInfo)
	:-
	arg(6, CLInfo, RevCmdLineSearch),
	dreverse(RevCmdLineSearch, CmdLineSearch),
	ss_init_searchdir(CmdLineSearch).

output_system_banner(CLInfo)
	:-
	arg(2,CLInfo,ConsultNoise),
	OutputStream = user_output,
	als_system(SysList),
	(ConsultNoise = true -> 
		true ; 
		print_banner(OutputStream,SysList)
	).

load_cl_files(CLInfo)
	:-
%	arg(2, CLInfo, Verbosity),
%	set_consult_messages(Verbosity),
	arg(3, CLInfo, Files),
	ss_load_files(Files).

process_cl_asserts(CLInfo)
	:-
	arg(8, CLInfo, CLAsserts),
	ss_cl_asserts(CLAsserts, OutputStream).

ss_cl_asserts([], OutputStream).
ss_cl_asserts([Expr | CLAsserts], OutputStream)
	:-
	catch( ss_cl_assert(Expr),
			_,
			ss_cl_assert_error(Expr, OutputStream)
		),
	ss_cl_asserts(CLAsserts, OutputStream).


ss_cl_assert(Expr)
	:-
	atomread(Expr, CLAssrt, [syntax_errors(quiet)]),
	ss_cl_assert0(CLAssrt).

ss_cl_assert_error(Expr, OutputStream)
	:-
	als_advise(OutputStream, 'Error reading command-line assert: %s', [Expr]),
	nl(OutputStream),
	flush_output(OutputStream).

ss_cl_assert0((Mod:AX))
	:-
	(modules(Mod,_) -> true ; create_new_module(Mod)),
	!,
	ss_cl_assert1(AX, Mod),
	'$icode'(-9,0,0,0,0).

ss_cl_assert0(AX)
	:-
	ss_cl_assert1(AX, user).

ss_cl_assert1((AX, BX), Mod)
	:-!,
	ss_cl_assert1(AX, Mod),
	ss_cl_assert1(BX, Mod).

ss_cl_assert1(AX, Mod)
	:-
	Mod:assertz(AX).

	%%%%%%%
	%%%%%%%

setup_init_goal(CLInfo, ShellCall)
	:-
	arg(7, CLInfo, CLShellCall),
	arg(1, CLInfo, CmdLineGoal),
	(CmdLineGoal = true ->	
		ShellCall = CLShellCall
		;
		ShellCall = 
			catch(CmdLineGoal, _, CLShellCall)
	).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% General library setup (called when shell
	%% starts up; blt_lib.pro is (to be) no 
	%% longer loaded as part of builtins:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

library_setup
	:-
	(ConsultNoise = true -> true ;
		als_advise(OutputStream, 'Setting up library indicies...may take a moment...',[])),
	setup_libraries,
	(ConsultNoise = true -> true ; 
		als_advise(OutputStream, 'Done.\n',[]),
		nl(OutputStream),
		flush_output(OutputStream)
	).

export setup_libraries/0.
setup_libraries
	:-
	sys_searchdir(ALSDIRPath),
	pathPlusFile(ALSDIRPath, '*', ALSDIRPattern),
	directory(ALSDIRPattern,1,SubdirList0),
	list_delete(SubdirList0, '.', SubdirList1),
	list_delete(SubdirList1, '..', SubdirList2),
	list_delete(SubdirList2, builtins, SubdirList),
	setup_local_libraries(SubdirList, ALSDIRPath),

	(getenv('ALS_LIBS', EnvLibsString) ->
		atom_codes(EnvLibsString, ELSCs),
		asplit0_all(ELSCs, 0',, EnvLibsStrings),
		all_to_atoms(EnvLibsStrings, EnvLibsList),
		setup_remote_libraries(EnvLibsList)
		;
		true
	).

setup_local_libraries([], _).
setup_local_libraries([Lib | LibsList], DirPath)
	:-
	pathPlusFile(DirPath, Lib, LibPath),
	setup_lib(LibPath),
	setup_local_libraries(LibsList, DirPath).

setup_remote_libraries([]).
setup_remote_libraries([LibPath | LibsList])
	:-
	setup_lib(LibPath),
	setup_remote_libraries(LibsList).

lib_extension(alb).

:- dynamic(als_lib_lcn/1).

export setup_lib/1.
setup_lib(LibPath)
	:-
	exists_file(LibPath),
	!,
	pathPlusFile(PathHead,LibDirName,LibPath),
	disp_setup_lib(LibDirName,LibPath,PathHead).

disp_setup_lib(library,LibPath,PathHead)
	:-
	als_lib_lcn(_),
	!.

disp_setup_lib(LibDirName,LibPath,PathHead)
	:-
	lib_extension(LibExt),
	filePlusExt('*',LibExt,Pattern),
	files(LibPath, Pattern, LibFileHeaders),
	install_lib_files(LibFileHeaders, LibPath),
	(filename_equal(LibDirName, library) ->
		assert(als_lib_lcn(PathHead)) 
		; 
		true
	).

setup_lib(LibPath)
	:-
	write(setup_lib_file_bad_path=LibPath),nl, flush_output,!,
	prolog_system_warning(lib_pth, [LibPath] ).

install_lib_files([], LibPath).
install_lib_files([LibFileHd | LibFileHeaders], LibPath)
	:-
	install_lib_f(LibFileHd, LibPath),
	install_lib_files(LibFileHeaders, LibPath).

install_lib_f(LibFileHd, LibDirPath)
	:-
	pathPlusFile(LibDirPath, LibFileHd, HeaderFile),
	open(HeaderFile, read, IS, []),
	read(IS, LHTerm0),
	close(IS),
	(LHTerm0 = (:- LHTerm) ->
		call(builtins:LHTerm)
		;
		call(builtins:LHTerm0)
	).

export als_advise/1.
als_advise(FormatString) 
	:-
	als_advise(warning_output, FormatString, []).

export als_advise/2.
als_advise(FormatString, Args) 
	:-
	als_advise(warning_output, FormatString, Args).

export als_advise/3.
als_advise(Stream, FormatString, Args) 
	:-
	printf(Stream, FormatString, Args),
	flush_output(Stream).

endmod.
