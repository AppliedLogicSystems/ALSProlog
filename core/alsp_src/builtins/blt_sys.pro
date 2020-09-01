/*================================================================*
 |		blt_sys.pro
 |	Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |
 |	Builtin predicates for dealing with the global Prolog 
 |	system environment, as well as miscellaneous stuff
 |	for dealing with the os environment
 |
 |	Authors: Kevin A. Buettner, Ken Bowen, Chris White,
 |	         Keith Hughes, Ilyas Cicekli
 |	Original Creation Date: 3/20/86
 *================================================================*/
module debugger.

:-	make_gv('Call'),		%% Debugger Call variable
	make_gv('Depth',1),		%% Debugger Depth variable
	make_gv('Retry', 0),	%% Retry variable
	make_gv('DebugInterrupt',debug_off).
					%% DebugInterrupt is explicitly set
					%% by goals which change the debugging
					%% state.
export setCall/1.
export getCall/1.

export setDepth/1.
export getDepth/1.

export setRetry/1.
export getRetry/1.

export getDebugInterrupt/1.
export setDebugInterrupt/1.

endmod.

module builtins.


/*------------------------------------------------------------------*
 * The following global variable access predicates are for use with the
 * shell and with the debugger.
 *------------------------------------------------------------------*/

%:-	make_gv('_shell_level'), set_shell_level(0).	%% shell level
:-	make_gv('_shell_level', 0).			

export halt/0.
halt :-
	pbi_halt.

/*------------------------------------------------------------------*
 |	corrected_sys_searchdir/1
 |	corrected_sys_searchdir(Dir)
 |	corrected_sys_searchdir(-)
 |
 |	If builtins:sys_searchdir(X) is a path the form
 |		X = <Path>/alsdir    (or <Path>\alsdir , etc.)
 |	then Dir = <Path>, else Dir = X  .
 *------------------------------------------------------------------*/
export corrected_sys_searchdir/1.
corrected_sys_searchdir(SSDIR)
	:-
	builtins:sys_searchdir(SSDIR0),
	path_elements(SSDIR0, SSDIRList0),
	last(SSDIRList0, LL),
	fin_corrected_sys_searchdir(LL, SSDIRList0, SSDIR).

fin_corrected_sys_searchdir(alsdir, SSDIRList0, SSDIR)
	:-!,
	dappend(SSDIRList, [alsdir], SSDIRList0),
	path_elements(SSDIR, SSDIRList).

fin_corrected_sys_searchdir(_, SSDIRList, SSDIR)
	:-
	path_elements(SSDIR, SSDIRList).

/*-------------------------------------------------------------------*
 | 	curmod/1          (returns the name of the current module)
 |	curmod(Mod)
 |	curmod(?)
 |
 |	- Mod is the current module context
 |
 |	The following icode command creates a module closure, which 
 |	unifies the module obtained from the module closure with the argument.
 *-------------------------------------------------------------------*/

:-	compiletime,
	module_closure(curmod,1,curmod).

curmod(M,M).

/*------------------------------------------------------------------*
 |	modules/2
 |	modules(Module, List)
 |	modules(+, -)
 |
 |	- List is the "use list" of Module
 *------------------------------------------------------------------*/
export modules/2.

modules(Module,UseList) 
	:-
	'$next_module'(-1,N,FirstModule,FirstUseList),  % first module
	next_modules(N,FirstModule,FirstUseList,Module,UseList).

next_modules(_,Module,UseList,Module,UseList).
next_modules(N,_,_,Module,UseList) 
	:-
	'$next_module'(N,NN,NextModule,NextUseList),
	next_modules(NN,NextModule,NextUseList,Module,UseList).

export sys_modules/1.
sys_modules([
	builtins,syscfg,rel_arith,xconsult,sio,
	pgm_info,debugger,tcltk,windows,tk_alslib,alsdev,utilities,
	alsshell,avl,cref,macroxp,shellmak,ttyshlmk
	]).

export non_sys_modules/1.
non_sys_modules(Mods)
	:-
	findall(M, (modules(M,_), (debugger:not(noshow_module(M)))), Mods).

export module_preds/2.
export module_preds/3.

module_preds(M,L)
	:-
	findall(PA, 
		(all_procedures(M,P,A,R), R\=0,atom_codes(P,[PC0|_]),
			PC0<129, catenate([P,'/',A],PA)
		), 
		L0),
	(L0 = [] ->
		catenate('No predicates defined in ',M,Msg),
		L = [Msg] 
		; 
		sort(L0, L1),
		L = L1
	).

module_preds(Mod,S,L)
	:-
	findall(PA, (debugger:spying_on(CallForm,Mod),
					functor(CallForm,P,A),
					catenate([P,'/',A],PA)  ),
					S0),
	sort(S0, S),

	findall(PA, 
		(all_procedures(Mod,P,A,R), R\=0,atom_codes(P,[PC0|_]),
			PC0<129, catenate([P,'/',A],PA)
		), 
		L0),
	(L0 = [] ->
		catenate('No predicates defined in ',Mod,Msg),
		L = [Msg] 
		; 
		sort(L0, L1),
		remove(S, L1, L)
	).

remove([], L, L).
remove([Item | S], L1, L)
	:-
	delete_sorted(L1, Item, L2),
	remove(S, L2, L).

delete_sorted([], Item, []).
delete_sorted([Item | L], Item, L)
	:-!.
delete_sorted([H | L], Item, L)
	:-
	H @> Item,
	!.

delete_sorted([H | L1], Item, [H | L2])
	:-
	delete_sorted(L1, Item, L2).


sys_mods_status(SMs)
        :-
        sys_modules(InitSysMs),
        sort(InitSysMs, SysMs),
        annotate_showing(SysMs, SMs).

annotate_showing([], []).
annotate_showing([M | SysMs], [[M, S] | SMs])
        :-
        ((debugger:noshow_module(M)) ->
                S = 0
                ;
                S = 1
        ),
        annotate_showing(SysMs, SMs).






/*------------------------------------------------------------------*
 | procedures/4
 | all_procedures/3
 | all_procedures/4
 | all_ntbl_entries/4
 | current_predicate/1
 *------------------------------------------------------------------*/

export procedures/4.
export all_procedures/3.
export all_procedures/4.
export all_ntbl_entries/4.

/*------------------------------------------------------------------*
 | Procedures written in Prolog.
 *------------------------------------------------------------------*/
procedures(M,P,A,DBRef) 
	:-
	procedures(M,P,A,DBRef,0).

/*------------------------------------------------------------------*
 | Procedures written in both Prolog and C.
 *------------------------------------------------------------------*/
all_procedures(M,P,A) 
	:-
	procedures(M,P,A,_,1).

all_procedures(M,P,A,DBRef) 
	:-
	procedures(M,P,A,DBRef,1).

/*------------------------------------------------------------------*
 | All name table entries
 *------------------------------------------------------------------*/
all_ntbl_entries(M,P,A,DBRef) :-
	procedures(M,P,A,DBRef,2).

/*------------------------------------------------------------------*
 | Basic access to procedure info:
 *------------------------------------------------------------------*/
    	% Directly access the name table entry
procedures(M,P,A,DBRef,F) 
	:-
	nonvar(M), nonvar(P), nonvar(A), 
	!,
	'$procinfo'(_,M,P,A,DBRef,_).

    	% Search all name table entries for possible matches
    	% starting from the first name table entry
procedures(M,P,A,DBRef,F) 
	:-
	'$nextproc'(-1,F,FirstProc),
	procedures(FirstProc,F,M,P,A,DBRef).

procedures(Proc,_,M,P,A,DBRef) 
	:-
	'$procinfo'(Proc,M,P,A,DBRef,_).

	% next procedure
procedures(CurProc,F,M,P,A,DBRef) 
	:-
	'$nextproc'(CurProc,F,NextProc),
	procedures(NextProc,F,M,P,A,DBRef).

/*------------------------------------------------------------------*
 | current_predicate/1
 | current_predicate(PredicateIndicator)
 | current_predicate(+/-)
 |
 |	Module closure of current_predicate/2:
 *------------------------------------------------------------------*/
:- module_closure(current_predicate,1).

current_predicate(M, P/A)
	:-
	procedures(M,P,A,_,1).


/*------------------------------------------------------------------*
 | clauses/4
 *------------------------------------------------------------------*/

export clauses/4.

clauses(M,P,A,DBRef) 
	:-
	procedures(M,P,A,First,0),
	clauses(First,DBRef).


/*------------------------------------------------------------------*
 * make_det_gv/1
 *
 * Name should be a string (list of small numbers) with the name of the
 * global variable.
 *
 * make_det_gv will build two access routines setName and getName where Name
 * is the list denoting the name of the global variable.  The difference
 * between make_det_gv/1 and make_gv/1 lies in the set___ routine 
 * generated.  Whereas the set_____ routine generated by make_gv/1
 * simply sets the appropriate global variable to the argument of
 * the set____ routine, the set____ routine generated by make_det_gv/1
 * sets the appropriate global variable to the result of applying
 * (copy_term) to the argument of the set____ routine.
 * This "makes the value of the global variable determinate" so that
 * future backtracking, arriving at top level, etc. will not 
 * uninstantiate portions of the term whose computation was 
 * non-determinate.
 *
 *------------------------------------------------------------------*/

:-	compiletime,
	module_closure(make_det_gv,1,make_det_gv1),
	module_closure(make_det_gv,2,make_det_gv2).

make_det_gv1(Mod,Name) :- 
	make_det_gv2(Mod,Name,0).

make_det_gv2(Mod,Name,InitVal) 
	:- 
	atom(Name), 
	global_gv_info:gvi(Name,VN,InitVal0),
	!,
	(InitVal0 = InitVal ->
		true
		;
		global_gv_info:retract(gvi(Name,VN,Mod,InitVal0)),
		global_gv_info:assert_at_load_time(gvi(Name,VN,Mod,InitVal))
	).

make_det_gv2(Mod,Name,InitVal) :- 
	atom(Name), 
	!, 
	name(Name,NameList), 
	make_det_gv2(Mod, NameList, 0).

make_det_gv2(Mod,NameList,InitVal) 
	:-
	name(Name,NameList),
	global_gv_info:gvi(Name,VN,Mod,InitVal0),
	!,
	(InitVal0 = InitVal ->
		true
		;
		global_gv_info:retract(gvi(Name,VN,Mod,InitVal0)),
		global_gv_info:assert_at_load_time(gvi(Name,VN,Mod,InitVal))
	).

make_det_gv2(Mod,Name,InitVal) :-
	name(SetFunc,[0's, 0'e, 0't | Name]),
	name(GetFunc,[0'g, 0'e, 0't | Name]),
	functor(SetHead,SetFunc,1),
	functor(GetHead,GetFunc,1),
	arg(1,SetHead,SetVar),
	arg(1,GetHead,GetVar),
	gv_alloc(VN),
	copy_term(InitVal,CopyInitVal),
	gv_set(VN, CopyInitVal),
	Mod:assert_at_load_time((GetHead :- gv_get(VN,GetVar))),
	Mod:assert_at_load_time(
			(SetHead :- 
				copy_term(SetVar,CopySetVar),
				gv_set(VN,CopySetVar))	),
	name(AtomicName, Name),
	copy_term(InitVal,CopyInitVal2),
	global_gv_info:assert_at_load_time( gvi(AtomicName,VN,Mod,CopyInitVal2) ).


/*------------------------------------------------------------------*
 | exec_file_command appends a file string onto a command and calls
 | system on the result.
 *------------------------------------------------------------------*/

exec_file_command(Command,FileStruct) 
	:-
	catenate([Command, ' ', FileStruct], NewCommandName),
	system(NewCommandName).

/*--------TO LIBRARY ??---------------------------------------------*
 |	exec_to/3
 |	exec_to(Module, TargetStream, Code)
 *------------------------------------------------------------------*/
export exec_to/3.
exec_to(Module, TargetStream, Code) 
	:-
	telling(CurrentStream),
	tell(TargetStream),
	Module:call(Code),
	tell(CurrentStream).

/*------------------------------------------------------------------*
 | Editor Interface
 |   vi, edit, editorchange
 |
 *------------------------------------------------------------------*/

export vi/0.
export vi/1.
export edit/0.
export edit/1.
export editorchange/1.

:- 	op(1125, fx, vi),
	op(1125, fx, edit).

'$editor'(vi).       %default

editorchange(New_Editor) :-
 	abolish('$editor',1),
 	name(New_Editor,EdStr),
 	assert('$editor'(EdStr)).

vi(File) :- 
	do_edit(vi,File).

(vi) :-  
	edit2(vi).

(edit) :-
	'$editor'(EdStr),
	edit2(EdStr).

edit(File) :-
	'$editor'(EdStr),
	do_edit(EdStr, File).

edit2(EditorNameString) :-
	'$editing'(File),!,
	do_edit(EditorNameString, File).
edit2(EditorNameString) :-
	write('File = '),
	read(File),
	do_edit(EditorNameString, File).

do_edit(EditorNameString, File) :-
	record_edit(File),
	exec_file_command(EditorNameString, File),
	reconsult(File).

record_edit(File) :-
	abolish('$editing',1),
	assert('$editing'(File)).

/*------------------------------------------------------------------*
 * ls, cd, and dir
 *------------------------------------------------------------------*/

export ls/0.
export ls/1.
export cd/0.
export cd/1.
export dir/0.
export dir/1.
export os/0.

:-	op(1125, fx, ls),
	op(1125, fx, cd),
	op(1125, fx, dir).

(ls) :- 
	system(ls).

ls(Dir) :- 
	tilda_expand(Dir, NDir),
	exec_file_command(ls,NDir).

dir :-
	system(dir).
dir(Dir) :-
	tilda_expand(Dir, NDir),
	exec_file_command(dir,NDir).

os :-
	sys_env(OS, _, _),
	os(OS).
os :-
	write('No os shell available.'),nl.

os(mswin32) :-
	(getenv('SHELL', Shell) ; getenv('COMSPEC', Shell)),
	!,
	system(Shell).
os(mswin32) :-
	system('command').

os(unix) :-
	getenv('SHELL', Shell),
	!,
	system(Shell).
os(unix) :-
	system('sh').

cd :-
	getenv('HOME', Home),
	change_cwd(Home).

cd(Dir) :-
	tilda_expand(Dir, NDir), 
	change_cwd(NDir).

/*------------------------------------------------------------------*
 * printing system usage information
 *------------------------------------------------------------------*/

export statistics/0.
export display_stats/1.

statistics :-
	sio:get_current_output_stream(Stream),
	display_stats(Stream).

display_stats(Stream) :-
	statistics(Stat),		% C - builtin
	dmember(stack(Sleft,Sused,Stotal),Stat),
	KStotal is Stotal / 1024,
	dmember(heap(Hleft,Hused,Tused,GValloced,Halloced),Stat),
	KHused is Hused / 1024,
	KHalloced is Halloced / 1024,
	dmember(code_area(Cused,Ctotal),Stat),
	KCtotal is Ctotal / 1024,
	dmember(wm_regs(SP,E,SPB,HB,H,TR,B),Stat),
	printf(Stream,'------------ statistics (in bytes) -----------------\n\n',[]),
	printf(Stream,'Heap used    \t: %d [%dK]\n',[Hused,KHused]),
	printf(Stream,'Trail used   \t: %d\n',[Tused]),
	printf(Stream,'Heap & Trail \t: %d / %d [%dK] (remaining/total alloc)\n',[Hleft,Halloced,KHalloced]),
	printf(Stream,'Stack        \t: %d / %d / %d [%dK] (used/remaining/total)\n',[Sused,Sleft,Stotal,KStotal]),
	printf(Stream,'Code         \t: %d / %d [%dK] (used/total)\n',[Cused,Ctotal,KCtotal],[]),
	printf(Stream,'Global Vars  \t: %d\n',[GValloced]),
	printf(Stream,'WAM regs     \t: SP=%#x E=%#x SPB=%#x\n',[SP,E,SPB]),
	printf(Stream,'                 \t: HB=%#x H=%#x TR=%#x B=%#x\n',[HB,H,TR,B]),
	flush_output(Stream).

export heap_status/1.
heap_status(Hleft) :-
	statistics(Stat),		% C - builtin
	dmember(heap(Hleft,Hused,Tused,GValloced,Halloced),Stat).

/*---------------------------------------------------------------------
 | Library management and loading facilities
 |
 | Information pertaining to a library is stored in a hash table.  We build
 | this hash table with the call to make_hash_table.
 *---------------------------------------------------------------------*/

:- make_hash_table('_libinfo').

	/*---------------------------------------------------------*
	 |	Below, we can almost just use cslt_blts_ld/3 from
	 |	builtins.pro.  However, that doesn't handle the
	 |	file clause group, and so the clauses loaded from
	 |	a library file would be assigned whatever clause group
	 |	was current.  So we have to put a little wrapper around
	 |	which assigns the clause group for the library file.
	 |	Unlike all other (user-defined) files, the library
	 |	leaves the .pro extension on the path in the record
	 |	for fcg.  There is nothing significant in this; just
	 |	expedient.
	 *---------------------------------------------------------*/

cslt_lib_ld(FileName, FilePathPro,FilePathObp)
	:-
	current_prolog_flag(debug, CurDBGFlag), 
	do_set_prolog_flag(debug, off),
	get_fcg(FilePathPro,CG),
	(clause(file_clause_groups(true),_) ->
		massively_abolish_clausegroup(CG) ; true ),
	push_clausegroup(CG),
	cslt_blts_ld(FileName, FilePathPro,FilePathObp),
	pop_clausegroup(_),
	do_set_prolog_flag(debug, CurDBGFlag).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Load an individual library file as
	%% driven by a given Call (in a Module):
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*---------------------------------------------------------------------*
 |	lib_load/2
 |	lib_load(Module,Call) 
 |	lib_load(+,+) 
 *---------------------------------------------------------------------*/
lib_load(Module,Call) 
	:-
	functor(Call,P,A),
		% try to get info on Module:P/A from the libary hash table (_libinfo):
	get_libinfo(Module:P/A,FileName),
%functor(Call,CF,CA),write( lib_load(Module,CF/CA, FileName) ),nl,flush_output,
		% Delete all _libinfo entries with value FileName:
		% pdel_libinfo(_,FileName), fail keeps backtracking until all
		% hits with value FileName have been removed; that is, all
		% entries for predicates from FileName have been removed from _libinfo:
	(pdel_libinfo(_,FileName), fail ; true),
	lib_load(FileName, Module, P,A, Module,Call).

lib_load(FileName, Module, P,A, Module,Call)
	:-
	is_absolute_path(FileName),
	!,
	(load(FileName,1,_,obp,_,_)
		; existence_error(lib_procedure,lib(Module:P/A,FileName),(Module:Call)) ),
	!,
	record_lib_load(FileName),
	Module:call(Call).

lib_load(FileName, Module, P,A, Module,Call)
	:-
	als_lib_lcn(ALSLibPathHead),
	split_path(ALSLibPathHead, PathHeadElts),
	dappend(PathHeadElts, [FileName], FFNElts),
	join_path(FFNElts, FullFileName),
    	'$atom_concat'(FullFileName,'.pro',FilePathPro),
    	'$atom_concat'(FullFileName,'.obp',FilePathObp),
    	(cslt_lib_ld(FileName, FilePathPro,FilePathObp)
    		; 
    		existence_error(lib_procedure,lib(Module:P/A,FileName),(Module:Call)) 
    	),
    	!,
    	record_lib_load(FileName),
    	Module:call(Call).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Force one or more library files to be 
	%% loaded (for use when packaging):
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
export force_libload_all/0.
export force_libload_all/1.
export force_libload_all/2.
export force_libload_all_lib/2.

force_libload_all
	:-
	force_libload_all(opts=[]).

force_libload_all(opts=Opts)
	:-
	sys_searchdir(ALSDIR),
	setof(File,
			(Pred^builtins:pget_libinfo(Pred,File),
				File\='builtins/debugger'),
			Files),
	force_libload_all(Files,ALSDIR,Opts).


force_libload_all(Files) 
	:-
	sys_searchdir(ALSDIR),
	force_libload_all(Files,ALSDIR,opts=[]).


force_libload_all(Files, opts=Opts) 
	:-
	sys_searchdir(ALSDIR),
	force_libload_all(Files,ALSDIR, Opts).


force_libload_all_lib(Files,Library) 
	:-
	force_libload_all_lib(Files,Library, opts=[]).


force_libload_all_lib(Files,Library, opts=Opts) 
	:-
	sys_searchdir(ALSDIR),
	split_path(ALSDIR, ALSDIRElts),
	dappend(ALSDIRElts, [Library], LPElts),
	join_path(LPElts, LibPath),
	force_libload_all(Files,LibPath, Opts).

force_libload_all([],_,_).
force_libload_all([File|Files],DirDC,Opts) 
	:-
	force_libload_file(File,DirDC,Opts),
	force_libload_all(Files,DirDC,Opts).

force_libload_file(File,DirDC,Opts)
	:-
	split_path(DirDC, DirDCElts),
	dappend(DirDCElts, [File], FNElts),
	join_path(FNElts, FileName),

	(member(show_msg, Opts) -> als_advise('Loading %s\n',[FileName])
		;
		true
	),
    	'$atom_concat'(FileName,'.pro',FilePathPro),
	'$atom_concat'(FileName,'.obp',FilePathObp),
%	(load(FileName,1,_,obp,_,_) ->
	(cslt_lib_ld(File, FilePathPro,FilePathObp) ->
		(pdel_libinfo(_,File), fail ; true),
		(member(show_msg, Opts) -> als_advise('...loaded\n',[])
			;
			true
		)
		;
		als_advise('\n!!WARNING File %s NOT LOADED!\n',[FileName])
	).


%:-dynamic(lib_path_rec/2).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Set up an individual predicate library 
	%% entry (name table and libinfo hash table):
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


export libactivate/4.
libactivate(M,[LH|LT],PredList,ModClosures) 
	:-
	libhide(M,[LH|LT],PredList),
	mc_all(ModClosures, M, [LH | LT]),
	!.
	%% warning_code(lib_act, 'Error activating library: %t %t %t-%t\n').
libactivate(M,LH_LT,PredList,ModClosures) 
	:-
	prolog_system_warning(lib_act, [M,LH_LT,PredList,ModClosures] ).

	
mc_all([], M, [LH | LT]).
mc_all([ModClose | ModClosures], M, [LH | LT])
	:-
	do_mc(ModClose, M, XP,XA),
	libhide(M,[LH|LT],[XP/XA]), 
	mc_all(ModClosures, M, [LH | LT]).

do_mc( module_closure(UserPredicate,Arity), M, UserPredicate, Arity1)
	:-!,
	'$create_mod_close'(M, UserPredicate,Arity, UserPredicate),
	Arity1 is Arity + 1.

do_mc( module_closure(UserPredicate,Arity, Procedure), M, Procedure, Arity1)
	:-
	'$create_mod_close'(M, UserPredicate,Arity, Procedure),
	Arity1 is Arity + 1.

:-dynamic(lib_mod_list/1).
libhide(M,List,PredList) 
	:-
	join_path(List, LibFileName),
	libhide0(PredList,M,LibFileName).

libhide0([],M,LibFileName) 
	:- !.
libhide0([P/A | Rest], M, LibFileName) 
	:-
	'$libbreak'(M,P,A,66),		% 66 is libload interrupt
	set_libinfo(M:P/A,LibFileName),
	newmodule(M),
	exportpred(P,A),
	endmodule,
	note_lib_mod(M),
	libhide0(Rest,M,LibFileName).

	%% record modules having library entries in: lib_mod_list/1
	%% used by cref to determine library imports
note_lib_mod(MF)
	:-
	lib_mod_list(LibModList),
	!,
	(dmember(MF, LibModList) -> 
		true
		;
		retract(lib_mod_list(_)),
		assert(lib_mod_list([MF | LibModList]))
	).

note_lib_mod(M)
	:-
	assert(lib_mod_list([M])).


/*---------------------------------------------------------------------
 * END: Library loading facilities
 *---------------------------------------------------------------------*/

:-op(800,fx,trace).
:-op(800,fx,spy).
:-op(800,fx,nospy).

/*------------------------------------------------------------------*
 * For use by the windowed debugger:
 *------------------------------------------------------------------*/

sys_exclude(builtins).
sys_exclude(sio).
sys_exclude(xconsult).

%sys_exclude(objects).

endmod.

module debugger.
%%
%% Establish the module closure for trace
%%

:- 
	compiletime,
	module_closure(trace,1,trace).

:- 
	compiletime,
	module_closure(debug_trace,1,trace).


endmod.  % debugger

module builtins.
/*------------------------------------------------------------------* 
 * dbg_notrace is used to stop tracing.  It is detected by the debugger
 * explicitly.
 *------------------------------------------------------------------*/

export dbg_notrace/0.
dbg_notrace.

export notrace/0.

notrace :-
	dbg_notrace,
	setDebugInterrupt(spying),
	setPrologInterrupt(spying).

	/*--------------------------------------------------------
	 | C-level debug points; for each of these, there must
	 | be a construct such as 
	 |		if (debug_system[GCFREEZEINFO]) {
	 |			printf(........) ; fflush(stdout);
	 |		}
	 | somewhere in the C code of the system, and there must
	 | be an entry in the "debug_feats" enum typedef in
	 | debugsys.h, such as:
	 |    GCFREEZEINFO,   /* gcfreezeinfo (2)     */
	 *-------------------------------------------------------*/
debug_sys_features(gcbeep,		0).
debug_sys_features(gcinfo,		1).
debug_sys_features(gcfreezeinfo,2).
debug_sys_features(cstrprim, 	3).
debug_sys_features(cstrchng, 	4).
debug_sys_features(cstrupdt, 	5).
debug_sys_features(cstruptm, 	6).
debug_sys_features(cstrupad, 	7).
debug_sys_features(cstrupxt, 	8).
debug_sys_features(gcintr, 		9).
debug_sys_features(cstrbpuf,	10).
debug_sys_features(frezbv,		11).
debug_sys_features(cut_rsb,		12).
debug_sys_features(cut_cpctr,	13).
debug_sys_features(frzdelay,	14).
debug_sys_features(intvbind,	15).

	/*--------------------------------------------------------
	 | Prolog-level debug points;  format:
	 |
	 |	   debug_sys_tag(Tag,  Module).
	 |
	 | Module is the module in which the give debug point
	 | occurs.  For each of these, there should be code
	 | such as:
	 |
	 | ,....,
	 | (debug_system_on(cstr_ig) ->
 	 |	printf_opt('Goal= %t\n',[Goal], [lettervars(false) ,line_length(100)])
	 |	; true),
	 | ,....,
	 |
	 *-------------------------------------------------------*/
debug_sys_tag(cstr_ig,  rel_arith).
debug_sys_tag(cstrislv, rel_arith).
debug_sys_tag(cstr_isv, rel_arith).

export toggle_system_debug/1.

toggle_system_debug(cstr)
	:-!,
	toggle_system_debug(cstrprim),
	toggle_system_debug(cstrchng),
	toggle_system_debug(cstrupdt),
	toggle_system_debug(cstruptm),
	toggle_system_debug(cstrupad),
	toggle_system_debug(cstrupxt),
	toggle_system_debug(cstrbpuf),

	toggle_system_debug(cstr_ig),
	toggle_system_debug(cstrislv),
	toggle_system_debug(cstr_isv).

	%% C-level stuff:
toggle_system_debug(WhichFeat)
	:-
	debug_sys_features(WhichFeat, FeatNum),
	!,
	toggle_sys_debug(FeatNum).

	%% Prolog-level stuff:
toggle_system_debug(WhichFeat)
	:-
	debug_sys_tag(WhichFeat, Module),
	toggle_debug_sys_tag(WhichFeat, Module).

/* Add error message */


toggle_debug_sys_tag(WhichFeat, Module)
	:-
	Module:clause(debug_system_on(WhichFeat),true,Ref),
	!,
	Module:erase(Ref).

toggle_debug_sys_tag(WhichFeat, Module)
	:-
	Module:assert(debug_system_on(WhichFeat)).





endmod.		%% blt_sys.pro: System-type builtins.
