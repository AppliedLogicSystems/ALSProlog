/*=====================================================================
 |			blt_misc.pro
 |		Copyright (c) 1996 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	Misc Builtin predicates setup stuff which must run assuming
 |	some other part of the builtins is already loaded.
 |	
 |	Authors: Ken Bowen
 |	Date: 6/96
 *====================================================================*/

module sio.

:-	current_prolog_flag(windows_system, WinSys),
	(WinSys = motif ->
		assert_at_load_time(
			( winsGetTextInsertionPosition(WinID, WinInsertPos) :-
    				x_XmTextGetInsertionPosition(WinID,WinInsertPos) )
						   ),
		assert_at_load_time(
			( write_buffer_to_win(output,BufUIA,NumCs,WinID,EndPos) :-
				 x_XmTextGetLastPosition(WinID,StartPos),
				 EndPos is StartPos + NumCs,
				 x_XmTextReplace(WinID,StartPos,EndPos,BufUIA),
				 x_XmTextSetInsertionPosition(WinID,EndPos),
				 getDisplay(Display),
				 x_XFlush(Display)

						   ) ),
	   assert_at_load_time(
	     ( write_buffer_to_win(append,BufUIA,NumCs,WinID,EndPos) :-!,
				x_XmTextGetLastPosition(WinID,StartPos),
				EndPos is StartPos + NumCs,
				x_XmTextReplace(WinID,StartPos,EndPos,BufUIA),
				x_XmTextSetInsertionPosition(WinID,EndPos),
				getDisplay(Display),
				x_XFlush(Display)
	     )
	   ),

		asserta_at_load_time(
			( wait_data(window, Stream, Call) :-!,
				stream_pgoals(Stream,PromptGoal),
				call(PromptGoal),
				sio:stream_addl3(Stream, CurAddl3),
				sio:set_stream_addl3(Stream, [Call | CurAddl3]),
				prolog_xt_mainLoop(data_ready(Stream)),
				sio:set_stream_addl3(Stream, CurAddl3),
				sio:read_buffer(window,Stream), !,
					%% restart call:
				call(Call)		)	
						   ) 
	;	%% Motif %%
	WinSys = openlook ->
		assert_at_load_time(
			( winsGetTextInsertionPosition(WinID, WinInsertPos) :-
					c_alloc(long,Dummy), c_alloc(long,PosBuf),
					x_OlTextEditGetCursorPosition(WinID,Dummy,
						Dummy,PosBuf,1),
					c_examine(PosBuf,long,WinInsertPos) )
		),
		assert_at_load_time(
			( write_buffer_to_win(BufUIA,NumCs,WinID,EndPos) :-
				x_OlTextEditInsert(WinID,BufUIA,NumCs,1),
				x_OlTextEditTextBuffer(WinID,TextBuf),
				x_LastTextBufferPosition(TextBuf,EndPos) )
		),
		asserta_at_load_time(
			( wait_data(window, Stream, Call) :-!,
				stream_pgoals(Stream,PromptGoal),
				call(PromptGoal),
				stream_identifier(Stream,StreamId),
				prolog_xt_stream_mainLoop(StreamId),
				read_buffer(window,Stream), !,
					%% restart call:
				call(Call)		)
		)
	;	%% OpenLook %%
	true
	).

endmod.
