/*---------------------------------------------------------------------
 |				generic_comms.pro
 |			Copyright (c) 1993 Applied Logic Systems, Inc.
 |		
 |		Generic stuff for use with ipc_test.pro and socket_test.pro
 *--------------------------------------------------------------------*/

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%	SEND
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generic_send_test(OutStream)
	:-
	write('>>'),flush_output,
	read(Cmd),
	disp_generic_send_test(Cmd,OutStream).

disp_generic_send_test(quit,OutStream) 
	:-!,
	write_clause(OutStream,quit).
disp_generic_send_test(Cmd,OutStream) 
	:-
	write_clause(OutStream,Cmd,[]),
	flush_output(OutStream),
	generic_send_test(OutStream).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%	RECEIVE
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generic_receive_test(InStream)
	:-
	read_term(InStream, Msg,[]),
	disp_generic_receive_test(Msg, InStream).

disp_generic_receive_test(quit, InStream)
	:-!,
	write('>> Got: '=quit),nl.

disp_generic_receive_test(end_of_file, InStream)
	:-!,
	generic_receive_test(InStream).

disp_generic_receive_test(Msg, InStream)
	:-
	write('>> Got: '=Msg),nl,flush_output,
	generic_receive_test(InStream).



