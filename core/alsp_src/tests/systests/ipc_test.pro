/*---------------------------------------------------------------------
 |				ipc_test.pro
 |			Copyright (c) 1993 Applied Logic Systems, Inc.
 |		
 *--------------------------------------------------------------------*/

:-[-generic_comms].

ipc_key(key('/tmp/ipc_test',ipc_test)).
%ipc_key(99).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%	SEND
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ipc_send :-
	ipc_key(Key),
	open(sysV_queue(Key), write, [], OutStream),
	!,
	generic_send_test(OutStream).

ipc_send :-
	ipc_key(Key),
	write(open(sysV_queue(Key), write, [], OutStream)-failed),nl.

ipcc_send :-
	ipc_key(Key),
	open(sysV_queue(Key), write, [], OutStream),
	!,
	generic_char_send_test(OutStream).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%	RECEIVE
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ipc_receive :-
	ipc_key(Key),
	open(sysV_queue(Key), read, [], InStream),
	!,
	generic_receive_test(InStream).

ipc_receive :-
	ipc_key(Key),
	write(open(sysV_queue(Key), read, [], InStream)-failed),nl.


generic_char_send_test(OutStream)
	:-
	write('>>'),flush_output,
	get_some_chars(CharAtom),
	write(OutStream, CharAtom), 
write(sent=CharAtom),nl,flush_output,
	flush_output(OutStream),
	generic_char_send_test(OutStream).
	
get_some_chars(CharAtom)
	:-
	get_some_chars0(CharList),
	name(CharAtom, CharList).

get_some_chars0(CharList)
	:-
	get_char(C),
	disp_get_some_chars(C,CharList).

disp_get_some_chars(0'\n,[]) :-!.
disp_get_some_chars(C,[C | CharList])
	:-
	get_some_chars0(CharList).
