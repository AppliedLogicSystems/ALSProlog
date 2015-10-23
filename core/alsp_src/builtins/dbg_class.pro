/*=============================================================*
 |		dbg_class.pro
 |	Copyright (c) 1986-1998 Applied Logic Systems, Inc.
 |
 |	ALS IDE part of debugger: classes & methods...
 |
 |	Author: Ken Bowen
 *=============================================================*/


module alsdev.
use tcltk.
use tk_alslib.

%use objects.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%       DEBUGGER ObjectPro CLASS DEFINITIONS    %%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        %% The one and only top level debugger class:
/*
:-defineClass(
	[   name=debugger_mgr,
		subClassOf=genericObjects,
		addl_slots=
			[ 
				debug_main_win, 			%% path to the ...
				debug_visible,  			%% true/false: debug_main_win visible
				src_trace_mgrs_by_file,		%% list of active mgrs, by file path
				fcg_index_size,				%% size of array for src_trace_mgrs_by_fcg
				src_trace_mgrs_by_fcg,		%% array (term) of active mgrs, by fcg
				mrfcg,						%% most recent file clause group touched
				stack_display_size,			%% size of stack to display
				stack_display_stream,		%% stream to write stack to
				stack_display_list			%% listbot to write stack to
			],
		defaults= [ 
			debug_main_win			= '.debugwin',
			debug_visible			=  false,
			src_trace_mgrs_by_file	= [],
			src_trace_mgrs_by_fcg	= [],
			mrfcg = 0,
			stack_display_size		= 20,
			stack_display_stream	= debugger_output,
			stack_display_list		= '.debugwin.stacklist'
			]
	]).
*/

debugger_mgrAction([Functor | Args], State)
	:-
	Msg =.. [Functor | Args],
	send_self(State, Msg).

debugger_mgrAction(ensure_db_showing, State)
	:-
	tcl_call(shl_tcli, [ensure_db_showing], _).

debugger_mgrAction(toggle_visibility, State)
	:-
	accessObjStruct(debug_visible, State, OldVisible),
	flip_true_fail(OldVisible, NewVisible),
	setObjStruct(debug_visible, State, NewVisible).

flip_true_fail(true, fail).
flip_true_fail(fail, true).

debugger_mgrAction(insert_by_fcg(ClauseGroup, SrcTrMgr), State)
	:-
	integer(ClauseGroup),
	!,
	accessObjStruct(fcg_index_size, State, FCG_Index_Size),
	accessObjStruct(src_trace_mgrs_by_fcg, State, CurIndex),
	(ClauseGroup =< FCG_Index_Size ->
		mangle(ClauseGroup, CurIndex, SrcTrMgr)
		;
		debugger_mgrAction(expand_st_ix(CurIndex,NewIndex), State),
		mangle(ClauseGroup, NewIndex, SrcTrMgr)
	).

	%% psl's and other things can produce ClauseGroup = '' ::
debugger_mgrAction(insert_by_fcg(ClauseGroup, SrcTrMgr), State).

debugger_mgrAction(expand_st_ix(CurIndex,NewIndex), State)
	:-
	functor(CurIndex, Fctr, CurSize),
	NewSize is CurSize + 50,
	functor(NewIndex, Fctr, NewSize),
	copy_entries(1, CurSize, CurIndex, NewIndex),
	CurSize1 is CurSize + 1,
	set_all_args(CurSize1, NewSize, NewIndex, nil).

copy_entries(Cur, Final, Source, Target)
	:-
	Cur > Final,
	!.
copy_entries(Cur, Final, Source, Target)
	:-
	arg(Cur, Source, Val),
	arg(Cur, Target, Val).

	%% debugger_mgrAction( get_mrfcg(CG, SrcMgr), State)
	%% debugger_mgrAction( get_mrfcg(+, -), State)
debugger_mgrAction( get_mrfcg(CG, SrcMgr), State)
	:-
	accessObjStruct(src_trace_mgrs_by_fcg, State, CurIndex),
	arg(CG, CurIndex, SrcMgr).

debugger_mgrAction( mgr_by_cg(CG, SrcMgr), State)
	:-
	accessObjStruct(src_trace_mgrs_by_fcg, State, CurIndex),
	arg(CG, CurIndex, SrcMgr).

debugger_mgrAction( clear_for_leap, State)
	:-
	accessObjStruct(mrfcg, State, CG),
	accessObjStruct(src_trace_mgrs_by_fcg, State, CurIndex),
	arg(CG, CurIndex, SrcMgr),
	send(SrcMgr, clear_decorations).

debugger_mgrAction( show_stack, State)
	:-
	accessObjStruct(stack_display_size, State, SDSZ),
	accessObjStruct(stack_display_stream, State, Stream),
	deb_stack_trace(1,1,SDSZ,Stream). 

deb_stack_trace(N,_,N,_) :- !.
deb_stack_trace(N,M,SZ,S) 
	:-
	frame_info(N,FI),
	!,
	deb_disp_stack_trace(FI,N,M,SZ,S).

deb_stack_trace(_,_,SZ,S).

deb_disp_stack_trace((builtins:GG),_,_,SZ,S) 
	:-
	functor(GG,do_shell_query,_),
	!.

deb_disp_stack_trace((debugger:dogoal(_,_,_,Mod,Goal)), N,M,SZ,S)
	:-!,
	printf(S,'(%d) %t:%t\n',[M,Mod,Goal],[quoted(true),maxdepth(8)]),
	NN is N+1,
	MM is M+1,
	deb_stack_trace(NN,MM,SZ,S).

deb_disp_stack_trace(FI, N,M,SZ,S)
	:-
	NN is N+1,
	deb_stack_trace(NN,M,SZ,S).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%% StackList %%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
debugger_mgrAction( show_stack_list, State)
	:-
	accessObjStruct(stack_display_size, State, SDSZ),
	accessObjStruct(stack_display_list, State, ListPath),
	tcl_call(shl_tcli, [ListPath,delete,0,end], _),
	deb_stack_trace_list(1,1,SDSZ,ListPath). 

deb_stack_trace_list(N,_,N,_) :- !.
deb_stack_trace_list(N,M,SZ,Win) 
	:-
	frame_info(N,FI),
	!,
	deb_disp_stack_trace_list(FI,N,M,SZ,Win).  

deb_stack_trace_list(_,_,SZ,Win).

deb_disp_stack_trace_list((builtins:GG),_,_,SZ,Win) 
	:-
	functor(GG,do_shell_query,_),
	!.

deb_disp_stack_trace_list((debugger:dogoal(_,_,_,Mod,Goal)), N,M,SZ,Win)
	:-!,
	sprintf(atom(Item),'%t:%t',[Mod,Goal],[maxdepth(4,'*','...'),quoted(true)]),
	tcl_call(shl_tcli, [Win, insert, 0, Item], _),
	NN is N+1,
	MM is M+1,
	deb_stack_trace_list(NN,MM,SZ,Win).

deb_disp_stack_trace_list(FI, N,M,SZ,Win)
	:-
	NN is N+1,
	deb_stack_trace_list(NN,M,SZ,Win).



exit_debugger
	:-
	debugger:nospy.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%    DEBUGGER Source Trace CLASS DEFINITIONS    %%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*-------------------------------------------------------
		Parent slot structure:
				%%%% source_handler:
			source_type,		%% file/anon win/....
			source_file, 		%% OS path to the ...
			base_file,			%% underlying file name
			ext,				%% underlying extension
			obp_file,			%% OS path to obp file if exists, or nil
			fcg, 				%% File clause group # for this (consulted) file
			consult_mode,		%% normal/debug
			last_consult		%% Time of last consult,

				%%%% shl_source_handler:
		    tcl_doc_path,       %% nil / Tcl id of edit window
			errors_display      %% nil / non-empty errs list

		%% ---------------------------------------------------------------
        %% The manager for individual source trace windows:
		%%		-- This is actually executed in alsdev.pro
		%%		   at run-time in the predicate setup_init_ide_classes/0:
		%% ---------------------------------------------------------------
:- defineClass(
	[   name=source_trace_mgr,
		subClassOf=shl_source_handler,
		addl_slots=
			[
				debugger_mgr,		%% home to daddy...
				last_visual_load,	%% Time of last load of file text widget
				num_lines,			%% num lines in the file
				linesizes,			%% list of num chars in each line
				invlineindex,		%% list of char offsets to start of each line
				head_tag,			%% i(S,E) = last colored "matching head (aph)tag" lcn
				call_tag			%% i(S,E) = last colored "matching_call (apg)tag" lcn
			],
		defaults= [ 
			visible				= false,
			last_visual_load 	= 0,
			num_lines			= 0,
			head_tag			= 0,
			call_tag			= 0
			]
	]).

	 *------------------------------------------------------*/

source_trace_mgrAction([Functor | Args], State)
	:-
	Msg =.. [Functor | Args],
	send_self(State, Msg).

source_trace_mgrAction(note_loaded(CG, Path), State)
	:-
	send_parent(State, note_loaded(CG, Path)),
	accessObjStruct(debugger_mgr, State, DBGMGR),
	send( DBGMGR, insert_by_fcg(CG, State) ).


source_trace_mgrAction( clear_errors_display, State)
	:-
	setObjStruct(errors_display, State, []).

source_trace_mgrAction( update_errors_wins(Ball), State)
	:-
	accessObjStruct(errors_display, State, ErrsList),
	cslt_reload(ErrsList, Ball, State).

	%% No previous errors, and no errors this time:
cslt_reload(nil, _, State)
	:- !.

cslt_reload([], _, State)
	:- !.

	%% There were previous errors:
cslt_reload(ErrsList, Ball, State)
	:-
	(var(Ball) ->
		send(State, close_and_reopen)
		;
		true
	).
		





source_trace_mgrAction(raise, State)
	:-
	accessObjStruct(tcl_doc_path, State, TclWin),
	(TclWin \= nil -> tcl_call(shl_tcli, [raise,TclWin], _) ; true).



source_trace_mgrAction(clear_decorations, State)
	:-
	accessObjStruct(tcl_doc_path, State, TclWin),
	(TclWin \= nil ->
		accessObjStruct(head_tag, State, PrevHT),
		(PrevHT = i(PSL,_,_,_) ->
			Line = PSL
			;
			Line = 1
		),
		tcl_call(shl_tcli, [clear_src_win_decorations, TclWin, Line], _)
		;
		true
	).

	%% Handle [user] and non-file windows separately (add clause):
source_trace_mgrAction(start_src_trace(BaseFileName, SrcFilePath, FCG), State)
	:-
	setObjStruct(fcg, State, FCG),
	!,
	fin_mgr_start_source_trace(SrcFilePath, State).

	%% Assume that the file has previously been consulted, with source code 
	%% debugging on, but that the source window for it is maybe not open; 
	%% the call below is used when we transition from
	%% clauses in some other file to this one during tracing:  we get a
	%%				'$dbg_aph'(ClsGrp,Start,End)
	%% hidden goal which gives us the clause group, and we get the
	%% source manager from the debugger_mgr:

source_trace_mgrAction(start_src_trace, State)
	:-
	accessObjStruct(source_file, State, SrcFilePath),
	!,
	fin_mgr_start_source_trace(SrcFilePath, State).

fin_mgr_start_source_trace(SrcFilePath, State)
	:-
	send_self(State, complete_open_edit_win(SrcFilePath,TclWin)),
	tcl_call(shl_tcli, [add_left_col, TclWin, 1], _),
	tcl_call(shl_tcli, [line_index_file,SrcFilePath],LoadRes0),
	LoadRes0 = [NumLines, LineIndex0],
	adjust_char_line_count(LineIndex0, LineIndex),
	LoadRes = [NumLines, LineIndex],
	setObjStruct(num_lines, State, NumLines),
	setObjStruct(linesizes, State, LineIndex),
	inverted_index(LineIndex, InvertedLineIndex),
	setObjStruct(invlineindex, State, InvertedLineIndex),
	setObjStruct(head_tag, State, 0),
	!,
	setObjStruct(call_tag, State, 0).

inverted_index(LineIndex, InvertedLineIndex)
	:-
	inverted_index(LineIndex, 0, InvertedList),
	InvertedLineIndex =.. [ili | InvertedList].

inverted_index([], _, []).
inverted_index([NLCs | LineIndex], CurCharCount, [CurCharCount | List])
	:-
	NxtCharCount is CurCharCount + NLCs + 1,
	inverted_index(LineIndex, NxtCharCount, List).

	%% This is a kludge:
adjust_char_line_count(LoadRes0, LoadRes)
	:-
	sys_env(OS, _, _),
	adjust_char_line_count(OS, LoadRes0, LoadRes).

adjust_char_line_count(mswin32, LoadRes0, LoadRes)
	:-!,
	add1_all(LoadRes0, LoadRes).
adjust_char_line_count(_, LoadRes, LoadRes).

add1_all([], []).
add1_all([N | LoadRes0], [N1 | LoadRes])
	:-
	N1 is N + 1,
	add1_all(LoadRes0, LoadRes).

endmod.
