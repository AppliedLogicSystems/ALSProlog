/*===========================================================================
 |			sktclint.pro
 |		Copyright (c) 1995-96 Applied Logic Systems, Inc.
 |      Distribution rights per Copying ALS
 |
 |	Interface-independent parts of Prolog-based client side		
 |	to remote socket-based server.
 |	-- Uses inet (connection-full) sockets
 |
 |	Author: Ken Bowen
 |	Started: November 1995
 |
 |	Based on the client portion of Kevin Buettner's mathserv1.pro example.
 *==========================================================================*/

%% test stuff:

csh :- connect_to_server(hilbert).
csc :- connect_to_server(calder).

cs(PortNum) :- connect_to_server(hilbert,PortNum).

module socket_comms.

%% test stuff:

hosts_and_ports(calder, 5783).
hosts_and_ports(hilbert, 5783).

	/*--------------------------------------------------------------*
	 |	Defaults for local (error) communication:
	 |	can include 'dialog(Name)', which causes a popup dialog
	 |	box under a GUI:
	 *--------------------------------------------------------------*/

local_read_stream(user_input).
local_write_stream(user_output).

	/*--------------------------------------------------------------*
	 |	Info for connections is stored in:
	 |
	 |		hosts_and_ports(Host,Port).
	 |		host_alias_for(HostAlias, Host).
	 |		default_port(DefaultPort)
	 |
	 |	Routines for manipulating this occur below
	 |
	 |	Host: IP address or symbolic name
	 |	Port: Integer
	 |
	 |	Entries in host_alias_for(HostAlias, Host):
	 |	- must already exist an entry hosts_and_ports(Host,Port):
	 |
	 |	If appropriate, 
	 |			default_port(DefaultPort)
	 |	is set uniformly by a given program using this code.
	 *--------------------------------------------------------------*/

export connect_to_server/1.

connect_to_server(Host)
	:-
	hosts_and_ports(Host, Port),
	!,
	connect_to_server(Host, Port).

connect_to_server(HostAlias)
	:-
	host_alias_for(HostAlias, Host),
	hosts_and_ports(Host, Port),
	!,
	connect_to_server(Host, Port).

connect_to_server(Host)
	:-
	default_port(Port),
	!,
	connect_to_server(Host, Port).

connect_to_server(Host)
	:-
	client_error(no_port_info, [Host]).

	/*--------------------------------------------------------------*
	 |	Use aliases to make the socket streams globally accessible:
	 |		client_read_sock for  the input read socket 
	 |		client_write_sock for the output write socket 
	 |
	 |	Sockets are opened in default socket-type stream, because
	 |	one can cause a hang with mis-matched line endings
	 |	(e.g. mac <-> unix); 
	 *--------------------------------------------------------------*/

export connect_to_server/2.

connect_to_server(Host,Port)
	:-
	catch(open(socket(inet_stream,Host,Port),read,_,[alias(client_read_sock)]),
			_,
			client_error(connection,[Host,Port])),
	(is_server_socket(client_read_sock) ->
		close(client_read_sock),
		client_error(server_socket,[Host,Port])
		;
		open(socket(clone,client_read_sock),write,_,[alias(client_write_sock)])
	),
		%% drop this if no authentication:
	client_authentication.


export stop_client/0.

stop_client
	:-
	close(client_read_sock),
	close(client_write_sock).
			 
	%% emergency use(?): (server might ignore this)
export stop_server/1.
stop_server(Password)
	:-
	printf(client_write_sock,'stop_yourself(%t).\n',[Password]),
	flush_output(client_write_sock).

	/*--------------------------------------------------------------*
	 |	send_request_wait_answer/2
	 |	send_request_wait_answer(Exp, Answer)
	 |	send_request_wait_answer(+, -)
	 |
	 |	- calls send_request_wait_answer/3 with default Prolog Terminator
	 |
	 |	send_request_wait_answer/3
	 |	send_request_wait_answer(Exp, Terminator, Answer)
	 |	send_request_wait_answer(Exp, Terminator, Answer)
	 |
	 |	- sends Exp on outbound socket, and waits for answer
	 |
	 |	Input Args:
	 |	- Exp: 			an arbitrary Prolog term, including atoms
	 |	- Terminator:	a (quoted) string, possibly empty
	 |	Output Args:
	 |	- Answer:		Normally an uninstatiated variable
	 |
	 |	Given an arbitrary Prolog Term Exp and a possibly quoted
	 |	atom terminator, forms the atom composed of the chars of
	 |	Exp followed immediately by the chars of Terminator,
	 |	writes the resulting atom on client_write_sock, and then
	 |	blocks until it can read Answer off of client_read_sock.
	 *--------------------------------------------------------------*/

	%% testing:
export sr/1.
sr(Exp)
	:-
	send_request_wait_answer(Exp,Answer),
	printf('Answer is %t\n',[Answer]).

export send_request_wait_answer/2.
export send_request_wait_answer/3.

send_request_wait_answer(Exp, Answer)
	:-
	send_request_wait_answer(Exp, '.\n', Answer).

send_request_wait_answer(Exp, Terminator, Answer)
	:-
	printf(client_write_sock, '%t%t', [Exp,Terminator]),
	flush_output(client_write_sock),
	read(client_read_sock, Answer),
	skip(client_read_sock, 0'\n).
						  
export send_request_wait_answers/3.
send_request_wait_answers(Exp, Terminator, AnswerList)
	:-
	printf(client_write_sock, '%t%t', [Exp,Terminator]),
	flush_output(client_write_sock),
	read_until(client_read_sock, AnswerList),
	skip(client_read_sock, 0'\n).
						  
read_until(InStream, AnswerList)
	:-
	read(InStream, Item),
	disp_read_until(Item, InStream, AnswerList).

disp_read_until(Ending, InStream, []) 
	:-
	multiple_answer_ending(Item),
	!.
disp_read_until(Item, InStream, [Item | AnswerList], Ending)
	:-
	read_until(InStream, AnswerList, Ending).

multiple_answer_ending('$ending$'(_)).


	/*--------------------------------------------------------------*
	 |	Error/Warning notification:
	 *--------------------------------------------------------------*/

client_error(MessageID, MsgArgs)
	:-
	client_error_msg(MessageID, MsgFormat),
	sprintf(atom(OutMessage), MsgFormat, MsgArgs),

	local_write_stream(LocalOut),
	xmit_client(LocalOut, OutMessage),
	!,
	fail.

client_warning(MessageID, MsgArgs)
	:-
	client_warnings_on,
	!,
	client_warning_msg(MessageID, MsgFormat),
	sprintf(atom(OutMessage), MsgFormat, MsgArgs),

	local_write_stream(LocalOut),
	xmit_client(LocalOut, OutMessage).

client_warning(_, _).

	%% default at startup:
client_warnings_on.

export toggle_client_warnings/0.

toggle_client_warnings
	:-
	client_warnings_on,
	!,
	abolish( client_warnings_on/0 ).

toggle_client_warnings
	:-
	assert( client_warnings_on ).


		%%%%%%%%%%%%%%%%%%%%
		%% ERROR MESSAGES
		%%%%%%%%%%%%%%%%%%%%

	%% client_error(connection,[Host,Port]),
client_error_msg(connection, 'Can''t connect to host %t at port %t!\n').

	%% client_error(server_socket,[Host,Port])
client_error_msg(server_socket,
		'Error: obtained server socket connecting to host %t at port %t!\n').

	%% client_error(no_port_info, [Host]).
client_error_msg(no_port_info, 'Error: No port information for host %t!\n').

		%%%%%%%%%%%%%%%%%%%%
		%% WARNING MESSAGES
		%%%%%%%%%%%%%%%%%%%%

	%% client_warning(change_port, [Host,OldPort,Port]),
client_warning_msg(change_port, 'Warning: Changing host %t port %t to %t.\n').

	%% client_warning(delete_host, [Host,OldPort]),
client_warning_msg(delete_host, 'Warning: Deleting host %t (port %t).\n').

	%% client_warning(unknown_host, [Host]),
client_warning_msg(unknown_host, 'Warning: No info on host %t.\n').

	%% client_warning(login_failed, []).
client_warning_msg(login_failed, 'Login failed').

		%%%%%%%%%%%%%%%%%%%%
		%%%%%%%%%%%%%%%%%%%%

xmit_client(dialog(InvWin), OutMessage)
	:-!,
	send(message_dialog_object, message(InvWin, Text)).

xmit_client(message(InvWin), OutMessage)
	:-!,
	send(message_dialog_object, message(InvWin, Text)).

	%% default:
xmit_client(LocalOut, OutMessage)
	:-
	printf(LocalOut, OutMessage, []).

	/*--------------------------------------------------------------*
	 |	Host/port information manipulation:
	 |		hosts_and_ports(Host,Port).
	 |		host_alias_for(HostAlias, Host).
	 |		default_port(DefaultPort)
	 *--------------------------------------------------------------*/
	
default_port(4777).

export add_host_port/2.
add_host_port(Host, Port)
	:-
	hosts_and_ports(Host, OldPort),
	!,
	client_warning(change_port, [Host,OldPort,Port]),
	retract_all( hosts_and_ports(Host, _) ),
	assert( hosts_and_ports(Host, Port) ).

export delete_host/1.
delete_host(Host)
	:-
	hosts_and_ports(Host, OldPort),
	!,
	client_warning(delete_host, [Host,OldPort]),
	retract_all( hosts_and_ports(Host, _) ),
	retract_all( host_alias_for(_, Host) ).

delete_host(HostAlias)
	:-
	host_alias_for(HostAlias, Host),
	hosts_and_ports(Host, OldPort),
	!,
	client_warning(delete_host, [Host,OldPort]),
	retract_all( hosts_and_ports(Host, _) ),
	retract_all( host_alias_for(_, Host) ).

delete_host(HostAlias)
	:-
	client_warning(unknown_host, [Host]).

	/*--------------------------------------------------------------*
	 |	Handling the local notification streams:
	 *--------------------------------------------------------------*/

export change_local_read_stream/1.
change_local_read_stream(NewStream)
	:-
	abolish(local_read_stream/1),
	assert( local_read_stream(NewStream) ).

export change_local_write_stream/1.
change_local_write_stream(NewStream)
	:-
	abolish(local_write_stream/1),
	assert( local_write_stream(NewStream) ).

	/*--------------------------------------------------------------*
	 |	Client authetication at initial connection:
	 *--------------------------------------------------------------*/

client_authentication
	:-
	printf(client_write_sock,'login_request.\n',[]),
	flush_output(client_write_sock),
	read(client_read_sock, LoginPrompt),
	skip(client_read_sock, 0'\n),
	cl_auth(LoginPrompt).

cl_auth(login)
	:-
	local_write_stream(LocalOut),
	local_read_stream(LocalIn),
	get_name_passwd(UserName, Password, LocalIn, LocalOut),
		%% need to link routine to get machine name/ip, & pass that too:
	printf(client_write_sock,'login_info(%t,%t).\n',[UserName,Password]),
	flush_output(client_write_sock),
	read(client_read_sock, LoginTryResponse),
	skip(client_read_sock, 0'\n),
	auth_result(LoginTryResponse).
/******** Default -- application can supply own (export it) ****
get_name_passwd(UserName, Password, LocalIn, LocalOut)
	:-
	printf(LocalOut, 'User Name = ',[]),
	get_line(LocalIn, UNLine),
	atomread(UNLine, UserName),
	printf(LocalOut, 'Password = ',[]),
	get_line(LocalIn, PWLine),
	atomread(PWLine, Password).
 *****************************************************************/

auth_result(failure-_)
	:-
	client_warning(login_failed, []),
	is_stream(client_read_sock, RS),
	(stream_open_status(RS, open) ->
		close(RS) ; true),
	is_stream(client_write_sock, WS),
	(stream_open_status(WS, open) ->
		close(WS) ; true).

auth_result(success-MOTD)
	:-
	client_notify(logged_in, []),
	(MOTD = [] ->
		true 
		;
		client_message_view(MOTD, [])
	).

client_notify_on.

client_notify(MessageID, MsgArgs)
	:-
	client_notify_on,
	!,
	client_notify_msg(MessageID, MsgFormat),
	sprintf(atom(OutMessage), MsgFormat, MsgArgs),

	local_write_stream(LocalOut),
	xmit_client(LocalOut, OutMessage).

	%% client_notify(logged_in, []).
client_notify_msg(logged_in, 'logged in.\n\n').

client_message_view(Message, MsgArgs)
	:-
	client_notify_on,
	!,
	local_write_stream(LocalOut),
	multi_message_out(Message, LocalOut, MsgArgs).

		%% soup this up to a pop-up text window in the GUIs:
multi_message_out([], _, _).
multi_message_out([Line | Message], LocalOut, MsgArgs)
	:-
	sprintf(atom(OutMessage), Line, MsgArgs),
	xmit_client(LocalOut, OutMessage),
	multi_message_out(Message, LocalOut, MsgArgs).

endmod. % socket_comms


