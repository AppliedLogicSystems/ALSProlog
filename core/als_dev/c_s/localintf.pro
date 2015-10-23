/*======================================================================*
 | 			localintf.pro 
 |		Copyright (c) 1995-96 Applied Logic Systems, Inc.
 |
 |		Hooks for using the API without a "real socket"		
 |
 | Author: Ken Bowen
 | Started: November, 1995
 |
 | Based on server portion of mathserv1.pro example by Kevin Buettner
 *======================================================================*/

module socket_comms.
use usradmn.

open_local(Descrip, S, SInfo)
	:-
	dmember(read= (RDesc + ROpts), Descrip),
	dmember(write=(WDesc + WOpts), Descrip),
	open(RDesc, read, SR, ROpts),
	open(WDesc, write,SW, WOpts),
	ConnType = login,				%% change to non-login??
	S = c(SR,SW,InitState,ConnType),
	initial_state(ConnType,0,SR,SW,_,InitState,_,SInfo).

export receive_from_client/1.
receive_from_client(Message)
	:-
	write( receive_from_client(Message) ), nl,
%	(atom(Message) -> write(mssage_is_atom) ; write(other)),nl.

	get_intf_state(c(SR,SW,State,ConnType,SInfo)),
	disp_service_request(Message,SR,SW,State,ConnType,_,_,SInfo).


endmod.
