/*================================================================*
 |		blt_sys.pro
 |	Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	Builtin predicates for dealing with the global Prolog 
 |	system environment, as well as miscellaneous stuff
 |	for dealing with the os environment
 |
 |	Authors: Kevin A. Buettner, Ken Bowen, Chris White,
 |	         Keith Hughes, Ilyas Cicekli
 |	Original Creation Date: 3/20/86
 *================================================================*/

module builtins.
%use objects.
 
/*
 * curmod/1          (returns the name of the current module)
 *
 * The following icode command creates a module closure.  In this
 * implementation, the module name is attached as the first argument
 * instead of the last.  (The PC implementation puts the module name
 * in as the last argument).
 */

:-	compiletime,
	module_closure(curmod,1,curmod).

curmod(M,M).

/*
 * modules/2
 */

export modules/2.

modules(Module,UseList) :-
	'$next_module'(-1,N,FirstModule,FirstUseList),  % first module
	next_modules(N,FirstModule,FirstUseList,Module,UseList).

next_modules(_,Module,UseList,Module,UseList).
next_modules(N,_,_,Module,UseList) :-
	'$next_module'(N,NN,NextModule,NextUseList),
	next_modules(NN,NextModule,NextUseList,Module,UseList).

/*
 * procedures/4
 * all_procedures/3
 * all_procedures/4
 * all_ntbl_entries/4
 */

export procedures/4.
export all_procedures/3.
export all_procedures/4.
export all_ntbl_entries/4.

/*
 * Procedures written in Prolog.
 */
procedures(M,P,A,DBRef) :-
	procedures(M,P,A,DBRef,0).

/*
 * Procedures written in both Prolog and C.
 */
all_procedures(M,P,A) :-
	procedures(M,P,A,_,1).

all_procedures(M,P,A,DBRef) :-
	procedures(M,P,A,DBRef,1).

/*
 * All name table entries
 */
all_ntbl_entries(M,P,A,DBRef) :-
	procedures(M,P,A,DBRef,2).

/*
 * Basic access to procedure info:
 */
procedures(M,P,A,DBRef,F) :-
    	% Directly access to the name table entry
	nonvar(M), nonvar(P), nonvar(A), !,
	'$procinfo'(_,M,P,A,DBRef,_).
procedures(M,P,A,DBRef,F) :-
    	% Search all name table entries for possible matches
    	% starting from the first name table entry
	'$nextproc'(-1,F,FirstProc),
	procedures(FirstProc,F,M,P,A,DBRef).

procedures(Proc,_,M,P,A,DBRef) :-
	'$procinfo'(Proc,M,P,A,DBRef,_).
procedures(CurProc,F,M,P,A,DBRef) :-
	% next procedure
	'$nextproc'(CurProc,F,NextProc),
	procedures(NextProc,F,M,P,A,DBRef).

/*
 * clauses/4
 */

export clauses/4.

clauses(M,P,A,DBRef) :-
	procedures(M,P,A,First,0),
	clauses(First,DBRef).

/*
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
 */

:-	compiletime,
	module_closure(make_det_gv,1).

make_det_gv(Mod,Name) :- 
	atom(Name), 
	!, 
	name(Name,NameList), 
	make_det_gv(Mod, NameList).
make_det_gv(Mod,Name) :-
	name(SetFunc,[0's, 0'e, 0't | Name]),
	name(GetFunc,[0'g, 0'e, 0't | Name]),
	functor(SetHead,SetFunc,1),
	functor(GetHead,GetFunc,1),
	arg(1,SetHead,SetVar),
	arg(1,GetHead,GetVar),
	gv_alloc(VN),
	Mod:assert_at_load_time((GetHead :- gv_get(VN,GetVar))),
	Mod:assert_at_load_time(
			(SetHead :- 
				copy_term(SetVar,CopySetVar),
				gv_set(VN,CopySetVar))	).

/*
 * exec_file_command appends a command onto a file string and calls
 * system on the result.
 */

exec_file_command(Command,FileStruct) :-
	make_file_name(FileStruct,File,_),
	atom_concat(Command,' ',CommandPlusSpace),
	atom_concat(CommandPlusSpace,File,NewCommandName),
	system(NewCommandName).

/*
 * Editor Interface
 *   vi, edit, editorchange
 *
 */

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

/*
 * ls, cd, and dir
 */

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
	exec_file_command(ls,Dir).

:-	als_system(ALS_Info),
	dmember(os=OS,ALS_Info),
	(   OS=dos, !,
		assert_at_load_time( ((dir) :- system(dir)) ),
		assert_at_load_time( (dir(Dir) :- exec_file_command(dir,Dir)) )
	;
	    true).

:-	als_system(ALS_Info),
	dmember(os=OS,ALS_Info),
	dmember(os_variation=OS_Variant,ALS_Info),
	(OS=dos, !,
		assert_at_load_time( ((cd) :- system('cd')) ),
		assert_at_load_time( ((os) :- system('command')) )
	 %% end DOS
	 ;
	 (OS=unix, !,
		assert_at_load_time( ((cd) :- getenv('HOME',Env), '$chdir'(Env))  ), 
		(OS_Variant = sun, !,
			assert_at_load_time( ((os) :- system('csh')) )
			;
			assert_at_load_time( ((os) :- system('sh')) )
		)
	 )
	 %% end UNIX
	 ;
	 assert_at_load_time( ((os) :- write('No os shell available.'),nl) )
	).

cd(Dir) :- 
	'$chdir'(Dir).

/*
 * printing system usage information
 */

export statistics/0.
export display_stats/1.

statistics :-
	sio:get_current_output_stream(Stream),
	display_stats(Stream).

display_stats(Stream) :-
	statistics(Stat),		% C - builtin
	dmember(stack(Sleft,Sused,Stotal),Stat),
	dmember(heap(Hleft,Hused,Tused,GValloced,Halloced),Stat),
	dmember(code_area(Cused,Ctotal),Stat),
	dmember(wm_regs(SP,E,SPB,HB,H,TR,B),Stat),
	printf(Stream,'------------ statistics (in bytes) -----------------\n\n',[]),
	printf(Stream,'Heap used    : %d\n',[Hused],[]),
	printf(Stream,'Trail used   : %d\n',[Tused],[]),
	printf(Stream,'Heap & Trail : %d / %d (remaining/total)\n',[Hleft,Halloced],[]),
	printf(Stream,'Stack        : %d / %d / %d (used/remaining/total)\n',[Sused,Sleft,Stotal],[]),
	printf(Stream,'Code         : %d / %d (used/total)\n',[Cused,Ctotal],[]),
	printf(Stream,'Global Vars  : %d\n',[GValloced],[]),
	printf(Stream,'WAM regs     : SP=%#x E=%#x SPB=%#x\n',[SP,E,SPB],[]),
	printf(Stream,'             : HB=%#x H=%#x TR=%#x B=%#x\n',[HB,H,TR,B],[]),
	flush_output(Stream).

export heap_status/1.
heap_status(Hleft) :-
	statistics(Stat),		% C - builtin
	dmember(heap(Hleft,Hused,Tused,GValloced,Halloced),Stat).

/*---------------------------------------------------------------------
 * Library loading facilities
 *
 * Information pertaining to a library is stored in a hash table.  We build
 * this hash table with the call to make_hash_table.
 *---------------------------------------------------------------------*/

:- make_hash_table('_libinfo').

lib_load(Module,Call) 
	:-
	functor(Call,P,A),
	get_libinfo(Module:P/A,FileName),
	(pdel_libinfo(_,FileName), fail ; true),
	sys_searchdir(ALSDIR),
	'$atom_concat'(ALSDIR,FileName,FullFileName),
%	Module:abolish(P,A),
	load(FullFileName,1,_,obp,_),
	!,
	Module:call(Call).

export force_libload_all/0.
export force_libload_all/1.
export force_libload_all/2.

force_libload_all 
	:-
	sys_searchdir(ALSDIR),
	setof(File,Pred^builtins:pget_libinfo(Pred,File),Files),
	force_libload_all(Files,ALSDIR).

force_libload_all(Files) 
	:-
	sys_searchdir(ALSDIR),
	force_libload_all(Files,ALSDIR).

force_libload_all([],_).
force_libload_all([File|Files],DirDC) 
	:-
	force_libload_file(File,DirDC),
	force_libload_all(Files,DirDC).

force_libload_file(File,DirDC)
	:-
%	lib_path_rec(LibHeader,LibList),
%	dmember(File,LibList),
%	subPath(DirDCList,DirDC),
%	append(DirDCList,[LibHeader,File],XDirDCList),
%	subPath(XDirDCList,FileName),
	extendPath(DirDC,File,FileName),
	als_advise('Loading %s\n',[FileName]),
	(load(FileName,1,_,obp,_) ->
		(pdel_libinfo(_,File), fail ; true),
		als_advise('...loaded\n',[])
		;
		als_advise('\n!!WARNING File %s NOT LOADED!\n',[FileName])
	).

:-dynamic(lib_path_rec/2).

/*
lib_recording(LH,LT)
	:-
pbi_write(lib_rec_try_lib_path_rec(LH,LT)),pbi_nl,pbi_ttyflush,
	lib_path_rec(LH,LT),
pbi_write(lib_rec_OK_lib_path_rec(LH,LT)),pbi_nl,pbi_ttyflush,
	!.

lib_recording(LH,LT)
	:-
pbi_write(lib_rec_try_lib_path_rec_CLAUSE2(LH,LT)),pbi_nl,pbi_ttyflush,
 	assert_at_load_time(lib_path_rec(LH,LT)),
pbi_write(lib_rec_try_lib_path_rec_CLAUSE2_DONE(LH,LT)),pbi_nl,pbi_ttyflush.
*/

export libactivate/4.
libactivate(M,[LH|LT],PredList,ModClosures) 
	:-
	libhide(M,[LH|LT],PredList),
	mc_all(ModClosures).
	
mc_all([]).
mc_all([ModClose | ModClosures])
	:-
	call(ModClose),
	mc_all(ModClosures).

libhide(M,[LH|LT],PredList) 
	:-
%	lib_recording(LH,LT),
	directory_separator(DS),
	mklibpath(LT,DS,LH,LibFileName),
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
	libhide0(Rest,M,LibFileName).

mklibpath([],DS,Name,Name) 
	:- !.
mklibpath([H|T],DS,Prefix,OutName) 
	:-
	'$atom_concat'(Prefix,DS,PrefixDS),
	'$atom_concat'(PrefixDS,H,NewPrefix),
	mklibpath(T,DS,NewPrefix,OutName).

:-op(800,fx,trace).
:-op(800,fx,spy).
:-op(800,fx,nospy).

/*
 * For use by the windowed debugger:
 */

sys_exclude(builtins).
sys_exclude(sio).
sys_exclude(objects).
sys_exclude(xconsult).

/*
 *
 * The following global variable access predicates are for use with the
 * shell and with the debugger.
 */

:-	make_gv('_shell_level'), set_shell_level(0).	%% shell level

module debugger.

:-	make_gv('Call'),		%% Debugger Call variable
	make_gv('Depth'),		%% Debugger Depth variable
	make_gv('Retry'), setRetry(0),	%% Retry variable
	make_gv('DebugInterrupt'), setDebugInterrupt(debug_off).
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

%%
%% Establish the module closure for trace
%%

:- 
	compiletime,
	module_closure(trace,1,trace).


endmod.  % debugger

/* 
 * dbg_notrace is used to stop tracing.  It is detected by the debugger
 * explicitly.
 */

export dbg_notrace/0.
dbg_notrace.

export notrace/0.

notrace :-
	dbg_notrace,
	setDebugInterrupt(spying),
	setPrologInterrupt(spying).
/*
 * xform_command_or_query(InGoal,OutGoal)
 *
 *	Transforms InGoal to OutGoal, replacing certain input patterns
 *	with different output patterns.  In particular, list forms are
 *	transformed into consults.  This procedure may also be used to
 *	perform other transformations.
 *
 * This procedure is exported as it is used by xconsult.
 */

export xform_command_or_query/2.

xform_command_or_query(VGoal,VGoal) :-
	var(VGoal),
	!.
xform_command_or_query([File|Files],Consults) :-
	!,
	xform_file_list(File,Files,Consults).
xform_command_or_query((G1,G2),(TG1,TG2)) :-
	!,
	xform_command_or_query(G1,TG1),
	xform_command_or_query(G2,TG2).
xform_command_or_query((G1;G2),(TG1;TG2)) :-
	!,
	xform_command_or_query(G1,TG1),
	xform_command_or_query(G2,TG2).
xform_command_or_query((G1->G2),(TG1->TG2)) :-
	!,
	xform_command_or_query(G1,TG1),
	xform_command_or_query(G2,TG2).
xform_command_or_query((G1|G2),(TG1|TG2)) :-
	!,
	xform_command_or_query(G1,TG1),
	xform_command_or_query(G2,TG2).
xform_command_or_query(call(G),call(TG)) :-
	!,
	xform_command_or_query(G,TG).
xform_command_or_query(M:G,M:TG) :-
	!,
	xform_command_or_query(G,TG).
xform_command_or_query(G,G).

xform_file_list(File1,[File2|Files],(consult(File1),Consults)) :-
	!,
	xform_file_list(File2,Files,Consults).
xform_file_list(File,_,consult(File)).



endmod.		%% blt_sys.pro: System-type builtins.
