/*=====================================================================*
 |		sio.pro		
 |	Copyright (c) 1991-1996 Applied Logic Systems, Inc.
 |
 |	stream I/O builtins
 |
 | Author: Kevin A. Buettner
 | Original Creation: 10/11/91
 | Additions for different kinds of streams: Ken Bowen
 | Additions for sockets: Kevin Jiang
 | Revisions to pull it all together & clean things up: Kevin Buettner
 | Fixups for windows: Ken Bowen
 *=====================================================================*/

module sio.
use windows.
use tcltk.
use curl.

:- auto_use(sio).

/*
 * Define some global variables:
 *
 *	The call to make_hash_table("_alias") will build the following
 *	access predicates: reset_alias/0, set_alias/2, get_alias/2,
 *	del_alias/2, pget_alias/2, pdel_alias/2.
 *
 *	current_input_stream and current_output_stream are used to implement
 *	the concept of default streams.
 *
 *	next_stream_identifier is used to give each stream a unique integer
 *	so that two streams with the same filename will not unify together.
 *
 */

:-	make_hash_table("_alias"),
	make_hash_table("_stream_table"),
	make_gv("_current_input_stream"),
	make_gv("_current_output_stream"),
	make_gv("_user_prompt",''),
	make_gv("_next_stream_identifier").


/*
 * Define the default end-of-line types.
 */

default_read_eoln_type(_, universal).

default_write_eoln_type(socket, crlf) :- !.
default_write_eoln_type(tk_win, lf) :- !.

:-	als_system(L),
	dmember(os=OS, L),
	(OS=mswin32 -> assertz(default_write_eoln_type(_, crlf))
		;
		(OS=macos -> assertz(default_write_eoln_type(_, cr))
			;
			(OS=unix ->
				dmember(os_variation=OSM, L),
				( (OSM=djgpp1 ; OSM=djgpp2) ->
					assertz(default_write_eoln_type(_, crlf))
					;
					assertz(default_write_eoln_type(_, lf))
				)
			)
			;
			assertz(default_write_eoln_type(_, crlf))
	)
).

/*
 * next_stream_identifier/2 is used to retrieve the next stream identifier
 * unless one has been provided in the open options in which case that
 * identifier is returned.
 */

next_stream_identifier(Options,Id) :-
	dmember('$stream_identifier'(Id), Options),
	nonvar(Id),
	!.

next_stream_identifier(Options,Id) :-
	get_next_stream_identifier(Id),
%	NextId is (Id+1) /\ 0xffffff,
	NextId is (Id+1),
%(integer(Id) -> JKK=int ; JKK = real),
%(integer(NextId) -> KK=int ; KK = real),
%pbi_write(next_stream_identifier(JKK,Id,NextId,KK)),pbi_nl, pbi_ttyflush,
	set_next_stream_identifier(NextId).


/*
 * The following predicates access various components of a stream descriptor.
 *
 * Slot definitions:
 *
 *  stream_buffer   -- the buffer (UIA) associated with the stream. Our
 *             C routines manage this component for the most
 *             part.
 *  stream_open_status -- either open or closed.
 *  stream_type -- the type of stream.  This will be an atom such as file, etc.
 *  stream_name -- the name of the stream; for files this will simply
 *             be the file name or device name.
 *  stream_mode -- A list cell whose head is either input or noinput
 *             and whose tail is either output or nooutput. A
 *             stream with the stream_mode field set to
 *             [noinput|nooutput] is not terribly useful.
 *  stream_repositionability  
 *		-- Either true or false indicating the whether or not
 *		the stream is capable of being repositioned.
 *  stream_identifier -- an integer uniquely identifying the stream. With
 *             this identifier it is possible to open the same
 *             file or device twice and yet not have the two
 *             streams unify because the streams will be assigned
 *             different stream identifiers.
 *  stream_addl1    -- an extra field for use by certain streams.
 *  stream_addl2    -- an extra field for use by certain streams.
 *  stream_addl3    -- an extra field for use by certain streams.
 *  stream_pgoals   -- the goals to run (for input streams) when a new
 *		buffer is being read and a prompt is desired.
 *  stream_syntax_errors -- number of syntax errors seen on a stream open for
 *		input.
 *  stream_wt_opts	-- structure containing default values for use
 *		when writing out structured terms (write_term, writeq,
 *		write, etc).  This will be a three argument structure
 *		called with functor 'wt_opts'.  The first argument will give
 *		the line length, the second gives the maximum depth to write
 *		to, the third gives the manner in which depths are computed
 *		(an atom).  Note that this value will only be instantiated
 *		to this structure for streams opened for write access.
 *    stream_wt_line_length	-- access first element of the structure
 *		obtained by stream_wt_opts
 *    stream_wt_maxdepth	-- access the second element of the structure
 *		obtained by stream_wt_opts
 *    stream_wt_depth_computation	-- access third element of the structure
 *		obtained by stream_st_opts
 *  stream_token_list	-- remaining tokens to be processed in a read.
 *  stream_snr_action	-- value is one of wait, error, or snr_code.
 *			   Indicates what to do when an I/O operation would
 *			   block.
 *  stream_stype	-- value = text or binary; indicates the type
 *			option specified in the open options.
 *  stream_eof_action	-- action to take when the end of the stream is
 *			reached
 *
 *  We have included two extra slots for future expansion.  This comment
 *  should be revised when this changes.
 *
 *  Note that certain of these access predicates have been exported.  Use
 *  of these predicates is discouraged by application programs for the
 *  following reasons:
 *  1)  They are non-portable and may change over time.
 *  2)  There is no error checking performed.
 *  3)  The stream argument must be an actual stream descriptor and not
 *      an alias.
 *  4)  The stream I/O package manipulates these fields and depends on
 *      them; if an application program messes them up, we're in trouble.
 */

export stream_buffer/2.
export stream_open_status/2.
export stream_type/2.
export stream_name/2.
export stream_mode/2.
export stream_repositionability/2.
export stream_identifier/2.
export stream_extra/2.
export stream_addl1/2.
export stream_addl2/2.
export stream_addl3/2.
export stream_pgoals/2.
export stream_syntax_errors/2.
export stream_wt_opts/2.
export stream_wt_line_length/2.
export stream_wt_maxdepth/2.
export stream_wt_depth_computation/2.
export stream_token_list/2.
export stream_snr_action/2.
export stream_stype/2.
export stream_eof_action/2.
export stream_blocking/2.

stream_buffer(Stream,SD) :- 			arg(1,Stream,SD).
stream_open_status(Stream,OpenStatus) :- 	arg(2,Stream,OpenStatus).
stream_type(Stream,Type) :-			arg(3,Stream,Type).
stream_name(Stream,Name) :-			arg(4,Stream,Name).
stream_mode(Stream,Mode) :-			arg(5,Stream,Mode).
stream_repositionability(Stream,RP) :-		arg(6,Stream,RP).
stream_identifier(Stream,Id) :-			arg(7,Stream,Id).
stream_extra(Stream,Extra) :-			arg(8,Stream,Extra).
stream_addl1(Stream,Addl1) :-			arg(9,Stream,Addl1).
stream_addl2(Stream,Addl2) :-			arg(10,Stream,Addl2).
stream_addl3(Stream,Addl3) :-			arg(11,Stream,Addl3).
stream_pgoals(Stream,G)    :-			arg(12,Stream,G).
stream_syntax_errors(Stream,E) :-		arg(13,Stream,E).
stream_wt_opts(Stream,A) :-			arg(14,Stream,A).
stream_wt_line_length(S,LL) :-			arg(14,S,A), arg(1,A,LL).
stream_wt_maxdepth(S,MD) :-			arg(14,S,A), arg(2,A,MD).
stream_wt_depth_computation(S,DC) :-		arg(14,S,A), arg(3,A,DC).
stream_token_list(S,TL) :-			arg(15,S,TL).
stream_snr_action(S,Im) :-			arg(16,S,Im).
stream_stype(S,Type) :-				arg(17,S,Type).
stream_eof_action(S,EA) :-			arg(18,S,EA).
stream_blocking(S,Bl) :-			arg(19,S,Bl).

export set_stream_buffer/2.
export set_stream_open_status/2.
export set_stream_type/2.
export set_stream_name/2.
export set_stream_mode/2.
export set_stream_repositionability/2.
export set_stream_identifier/2.
export set_stream_extra/2.
export set_stream_addl1/2.
export set_stream_addl2/2.
export set_stream_addl3/2.
export set_stream_pgoals/2.
export set_stream_syntax_errors/2.
export set_stream_wt_opts/2.
export set_stream_wt_line_length/2.
export set_stream_wt_maxdepth/2.
export set_stream_wt_depth_computation/2.
export set_stream_token_list/2.
export set_stream_snr_action/2.
export set_stream_stype/2.
export set_stream_eof_action/2.
export set_stream_blocking/2.

set_stream_buffer(Stream,SD) :-			mangle(1,Stream,SD).
set_stream_open_status(Stream,OpenStatus) :-	mangle(2,Stream,OpenStatus).
set_stream_type(Stream,Type) :-			mangle(3,Stream,Type).
set_stream_name(Stream,Name) :-			mangle(4,Stream,Name).
set_stream_mode(Stream,Mode) :-			mangle(5,Stream,Mode).
set_stream_repositionability(Stream,RP) :-	mangle(6,Stream,RP).
set_stream_identifier(Stream,Id) :-		mangle(7,Stream,Id).
set_stream_extra(Stream,Extra) :-		mangle(8,Stream,Extra).
set_stream_addl1(Stream,Addl1) :-		mangle(9,Stream,Addl1).
set_stream_addl2(Stream,Addl2) :-		mangle(10,Stream,Addl2).
set_stream_addl3(Stream,Addl3) :-		mangle(11,Stream,Addl3).
set_stream_pgoals(Stream,G) :-			mangle(12,Stream,G).
set_stream_syntax_errors(Stream,E) :-		mangle(13,Stream,E).
set_stream_wt_opts(Stream,A) :-			mangle(14,Stream,A).
set_stream_wt_line_length(S,LL) :-		arg(14,S,A), mangle(1,A,LL).
set_stream_wt_maxdepth(S,MD) :-			arg(14,S,A), mangle(2,A,MD).
set_stream_wt_depth_computation(S,DC) :-	arg(14,S,A), mangle(3,A,DC).
set_stream_token_list(S,TL) :-			mangle(15,S,TL).
set_stream_snr_action(S,Im) :-			mangle(16,S,Im).
set_stream_stype(S,Type) :-			mangle(17,S,Type).
set_stream_eof_action(S,EA) :-			mangle(18,S,EA).
set_stream_blocking(S,Bl) :-			mangle(19,S,Bl).

/*
 * is_stream(Stream_or_alias, Stream)
 *
 * Succeeds when Stream_or_alias is a stream or alias and will bind Stream
 * to the corresponding Stream.
 *
 * FIXME:  Nuke all callers which call is_stream/2 in favor of either
 *	stream_or_alias_ok/2, input_stream_or_alias_ok/2, or
 *	output_stream_or_alias_ok/2.  Then nuke is_stream/2.
 */

export is_stream/2.

is_stream(Stream_or_alias, Stream) :-
	var(Stream_or_alias),
	!,
	fail.
is_stream(Stream, Stream) :-
	is_stream(Stream),
	!.
is_stream(user, UserInput) :-
	get_alias(user_input, UserInput).
is_stream(user, UserOutput) :-
	!,
	get_alias(user_output, UserOutput).
is_stream(Alias,Stream) :-
	get_alias(Alias,Stream).

/*
 * is_stream/1		-- the only place where stream_descriptor/20 is
 * 			   explicitly referenced.
 */

is_stream(Stream) :-
	nonvar(Stream),
	functor(Stream,stream_descriptor,20).

/*
 * is_alias/1
 */

is_alias(Object) :- atom(Object).

/*
is_alias(Alias) :-
	nonvar(Alias),
	get_alias(Alias,_),		%% hash table search
	!.
*/

/*
 * is_output_alias/2
 */

is_output_alias(Alias,Stream) :-
	nonvar(Alias),
	get_alias(Alias,Stream),
	stream_mode(Stream,[_|output]),
	!.
is_output_alias(Alias,Stream) :-
	nonvar(Alias),
	!,
	Alias = user,
	get_alias(user_output,Stream).

is_input_alias(Alias,Stream) :-
	nonvar(Alias),
	get_alias(Alias,Stream),
	stream_mode(Stream,[input|_]),
	!.
is_input_alias(Alias,Stream) :-
	nonvar(Alias),
	!,
	Alias = user,
	get_alias(user_input,Stream).

/*
 * is_not_stream_or_alias/1
 */

is_not_stream_or_alias(Var) :-
	var(Var),
	!.
is_not_stream_or_alias(user) :-
	!,
	fail.
is_not_stream_or_alias(NSA) :-
	is_stream(NSA),
	!,
	fail.
is_not_stream_or_alias(NSA) :-
	is_alias(NSA),
	!,
	fail.
is_not_stream_or_alias(NSA).


/*
 * stream_or_alias_ok(Stream_or_alias,Stream)
 *
 */

export stream_or_alias_ok/2.

stream_or_alias_ok(Var,_) :-
	var(Var),
	!,
	instantiation_error(2).
stream_or_alias_ok(Stream, Stream) :-
	is_stream(Stream),
	stream_open_status(Stream,open),
	!.
stream_or_alias_ok(Alias, Stream) :-
	get_alias(Alias,Stream),
	!,
	stream_open_status(Stream,open).
stream_or_alias_ok(user, Stream) :-
	!,
	get_alias(user_output, Stream).
stream_or_alias_ok(Culprit, _) :-
	stream_or_alias_error(Culprit, _).

stream_ok(Var) :-
	var(Var),
	!,
	instantiation_error(2).
stream_ok(Stream) :-
	is_stream(Stream),
	stream_open_status(Stream,open),
	!.
stream_ok(Culprit) :-
	stream_error(Culprit).

/*
 * input_stream_alias_ok(S_or_a,Stream)
 *
 * This predicate also checks to make sure that the stream is open.
 */

input_stream_or_alias_ok(Var, _) :-
	var(Var),
	instantiation_error(2).
input_stream_or_alias_ok(Stream, Stream) :-
	is_stream(Stream),
	stream_mode(Stream,[input|_]),
	stream_open_status(Stream,open),
	!.
input_stream_or_alias_ok(Alias, Stream) :-
	get_alias(Alias, Stream),
	stream_mode(Stream,[input|_]),
	stream_open_status(Stream,open),
	!.
input_stream_or_alias_ok(user, Stream) :-
	!,
	get_alias(user_input, Stream).
input_stream_or_alias_ok(Culprit, _) :-
	stream_or_alias_error(Culprit,input).

stream_or_alias_error(Culprit,Mode) :-
	var(Culprit),
	!,
	instantiation_error(2).
stream_or_alias_error(Culprit, _) :-
	is_not_stream_or_alias(Culprit),
	!,
	domain_error(stream_or_alias,Culprit,2).
stream_or_alias_error(Culprit, _) :-
	is_alias(Culprit),
	not(get_alias(Culprit, _)),
	!,
	existence_error(stream, Culprit, 2).
stream_or_alias_error(Culprit, Mode) :-
	is_stream(Culprit,S),
	stream_open_status(S,OpenStatus),
	OpenStatus \= open,
	!,
	existence_error(stream, Culprit, 2).
stream_or_alias_error(Culprit, input) :-
	is_stream(Culprit,S),
	stream_mode(S, SMode),
	SMode \= [input | _],
	!,
	permission_error(input, stream, Culprit, 2).
stream_or_alias_error(Culprit, output) :-
	is_stream(Culprit,S),
	stream_mode(S, SMode),
	SMode \= [_ | output],
	!,
	permission_error(output, stream, Culprit, 2).

stream_error(Culprit) :-
	var(Culprit),
	!,
	instantiation_error(2).
stream_error(Culprit) :-
	is_not_stream_or_alias(Culprit),
	!,
	domain_error(stream,Culprit,2).
stream_error(Culprit) :-
	is_stream(Culprit,S),
	stream_open_status(S,OpenStatus),
	OpenStatus \= open,
	!,
	existence_error(stream, Culprit, 2).
	

/*
 * output_stream_or_alias_ok(S_or_a,Stream)
 * 
 * This predicate also checks to make sure that the stream is open.
 */

output_stream_or_alias_ok(Var, _) :-
	var(Var),
	!,
	instantiation_error(2).
output_stream_or_alias_ok(Stream, Stream) :-
	is_stream(Stream),
	stream_mode(Stream,[_|output]),
	stream_open_status(Stream,open),
	!.
output_stream_or_alias_ok(Alias, Stream) :-
	get_alias(Alias, Stream),
	stream_mode(Stream,[_|output]),
	stream_open_status(Stream,open),
	!.
output_stream_or_alias_ok(user, Stream) :-
	!,
	get_alias(user_output, Stream).
output_stream_or_alias_ok(Culprit, _) :-
	stream_or_alias_error(Culprit,output).

/*
 * assign_alias(Alias, Stream_or_alias)
 *
 * Associates the atom Alias with the stream associated with Stream_or_alias.
 */

export assign_alias/2.

assign_alias(Alias, Stream_or_alias) :-
	nonvar_ok(Alias),
	stream_or_alias_ok(Stream_or_alias, Stream),
	set_alias(Alias, Stream).


/*
 * cancel_alias(Alias)
 *
 *	Deletes the association of the atom Alias with the stream (if any) with
 *	which it is associated.
 */

export cancel_alias/1.

cancel_alias(Alias) :-
	nonvar_ok(Alias),
	pdel_alias(Alias,_),
	fail.
cancel_alias(_).


/*
 * reset_user/2
 *
 *	"Cancels" the user alias and assigns new user aliases.  This
 *	procedure is for use by the development environment only. It
 *	should NOT be exported.
 */

reset_user(InStream,OutStream) 
	:-
	stream_identifier(InStream, OldInID),
	stream_identifier(OutStream, OldOutID),

%(integer(OldInID) -> pbi_write(int(OldInID)) ;  pbi_write(real(OldInID))),
%pbi_nl,pbi_ttyflush,
%(integer(OldOutID) -> pbi_write(int(OldOutID)) ;  pbi_write(real(OldOutID))),
%pbi_nl,pbi_ttyflush,

	del_stream_table(OldInID,_),
	del_stream_table(OldOutID,_),
	set_stream_identifier(InStream,-1),
	set_stream_identifier(OutStream,-2),
	set_stream_table(-1, InStream),
	set_stream_table(-2, OutStream),
		%% Reset connections between the sio stream and
		%% the window (if any):
	((clause(setStreamId(OldInID,_),gv_set(InGVNum,_))) ->
		retract((setStreamId(OldInID,_) :- gv_set(InGVNum,_))),
		retract((getStreamId(OldInID,_) :- gv_get(InGVNum,_))),
		assert((setStreamId(-1,Vin) :- gv_set(InGVNum,Vin))),
		assert((getStreamId(-1,Vin) :- gv_get(InGVNum,Vin)))
		;
		true
	),
	((clause(setStreamId(OldOutID,_),gv_set(OutGVNum,_))) ->
		retract((setStreamId(OldOutID,_) :- gv_set(OutGVNum,_))),
		retract((getStreamId(OldOutID,_) :- gv_get(OutGVNum,_))),
		assert((setStreamId(-1,Vout) :- gv_set(OutGVNum,Vout))),
		assert((getStreamId(-1,Vout) :- gv_get(OutGVNum,Vout)))
		;
		true
	),
	set_alias(user_input,InStream),
	set_alias(user_output,OutStream).

/*
 * current_alias(Alias,Stream)
 *
 *	Succeeds iff Alias is an alias which is associated with the stream
 *	Stream.
 */

export current_alias/2.
current_alias(Alias,Stream) :-
	nonvar(Alias),
	!,
	get_alias(Alias, Stream).
current_alias(Alias,Stream) :-
	pget_alias(Alias,Stream).

/*
 * current_input(Stream)
 *
 *      Succeeds iff the stream Stream is the current input stream.
 */

export current_input/1.

current_input(Stream) :-
		(var(Stream) ; is_stream(Stream)), !,
        get_current_input_stream(Stream).
current_input(Stream) :-
        domain_error(stream, Stream, 1).

/*
 * current_output(Stream)
 *
 *      Succeeds iff the stream Stream is the current output stream.
 */

export current_output/1.

current_output(Stream) :-
		(var(Stream) ; is_stream(Stream)), !,
        get_current_output_stream(Stream).
current_output(Stream) :-
        domain_error(stream, Stream, 1).

/*
 * set_input(Stream_or_alias)
 *
 * Sets the stream Stream_or_alias to be the current input stream.
 */

export set_input/1.

set_input(Stream_or_alias) :-
	input_stream_or_alias_ok(Stream_or_alias, Stream),
        set_current_input_stream(Stream).


 /*
  * set_output(Stream_or_alias)
  *
  * Sets the stream Stream_or_alias to be the current output stream.
  */

export set_output/1.

set_output(Stream_or_alias) :-
	output_stream_or_alias_ok(Stream_or_alias, Stream),
        set_current_output_stream(Stream).


/*
 * open(Source_sink,Mode,Stream)
 */

export open/3.
open(Source_sink,Mode,Stream) 
	:-
	open(Source_sink,Mode,Stream,[type(text)]).


/*
 * open(Source_sink,Mode,Stream,Options)
 */

export open/4.
open(Source_sink,Mode,Stream,Options) 
	:-
	check_source_sink_and_mode(Source_sink,Mode),
	check_open_options(Options),
	check_alias(Options, Source_sink, Alias),
	alias_extend_opts(Alias, Options, XOptions),

	check_repositionability(Source_sink,Mode,Options,Positionability),
	var_ok(Stream),
	open_stream(Source_sink,Mode,XOptions,Stream),
	(Alias = no(alias) -> true ; set_alias(Alias, Stream)),
	stream_identifier(Stream,Id),
	set_stream_table(Id,Stream),
	set_stream_repositionability(Stream,Positionability).

/*
 * check_source_sink_and_mode(Source_sink,Mode)
 *
 *	Examines the Mode argument for validity and triggers an error
 *	if not valid.
 */

check_source_sink_and_mode(Source_sink,Mode) :-
	var(Source_sink),
	!,
	instantiation_error(2).
check_source_sink_and_mode(null_stream(_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
%	dmember(Mode, [write]).
check_source_sink_and_mode(Source_sink,Mode) :-
	atom(Source_sink),
	!,
	check_mode(Mode,file_modes(Mode,_,_)).
check_source_sink_and_mode(sysV_queue(_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(ssbq(_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(nsocket(_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(socket(_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(socket(_,_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(socket(_,_,_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(string(_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(char_list(_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(code_list(_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(atom(_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(console(_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(console_error(_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(tk_win(_,_),Mode) :-
	!,
	(Mode = read_write; Mode = read ; Mode = write ).
%	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(window(_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(tcl_transfer(_,_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).

check_source_sink_and_mode(url(_),Mode) :-
	!,
	check_mode(Mode,read_write_modes(Mode,_,_)).
check_source_sink_and_mode(url(_,CurlOptions),Mode) :-
	!,
	check_curl_options(CurlOptions),
	check_mode(Mode,read_write_modes(Mode,_,_)).

check_source_sink_and_mode(Source_sink,Mode) :-
	domain_error(source_sink, Source_sink, 2).

check_mode(Mode,ConstraintGoal) :-
	var(Mode),
	!,
	instantiation_error(2).
check_mode(Mode,ConstraintGoal) :-
	atom(Mode),
	ConstraintGoal,
	!.
check_mode(Mode,_) :-
	atom(Mode),
	file_modes(Mode,_,_),
	!,
	%% This is not in the draft standard, but I consider it a permission
	%% error if Mode is a valid open mode, but not valid for the particular
	%% stream which we are trying to open.
	permission_error(open,stream,Mode,2).
check_mode(Mode,_) :-
	atom(Mode),
	!,
	domain_error(io_mode,Mode, 2).
check_mode(Mode,_) :-
	type_error(atom, Mode, 2).

/*
 * check_open_options(Source_sink,Mode,Options,Stream)
 *
 *	Examines the Options argument for validity and triggers an exception
 *	if not valid.
 */

check_open_options(Options) :-
	check_list_options(Options,
		[type(text),type(binary),
		 alias(_),
		 reposition(true), reposition(false),
		 eof_action(error), eof_action(eof_code), eof_action(reset),
		 buffering(byte),buffering(line),buffering(block),
		 snr_action(wait), snr_action(error),
		 snr_action(snr_code),
		 bufsize(_),
		 '$stream_identifier'(_),	%% ALS internal use only
         	 prompt_goal(_),
		 maxdepth(_), line_length(_), depth_computation(_),
		 blocking(_),
		 perms(_), perms(_,_,_),
		 connects(_), 				%% sockets
		 address(_),address(_,_),
		 write_eoln_type(cr), write_eoln_type(lf), write_eoln_type(crlf),
		 read_eoln_type(cr), read_eoln_type(lf), read_eoln_type(crlf),
		 read_eoln_type(universal)
		]).


check_list_options(V, ML) :-
	var(V),
	!,
	instantiation_error(2).
check_list_options([], ML) :-
	!.
check_list_options([H|T], ML) :-
	check_list_option(H,ML),
	!,
	check_list_options(T,ML).
check_list_options(Culprit,ML) :-
	type_error(list, Culprit,2).

check_list_option(V,ML) :-
	var(V),
	!,
	instantation_error(3).
check_list_option(M,ML) :-
	dmember(M,ML),
	!.
check_list_option(M,ML) :-
        domain_error(stream_option,M,3).
	
check_curl_options([]) :-!.
check_curl_options([Opt | CurlOptions])
	:-
	check_curl_opt(Opt),
	!,
	check_curl_options(CurlOptions).

check_curl_options(CurlOptions)
	:-
	type_error(list,CurlOptions,2).


check_curl_opt(Tag=_) 
	:-
	make_uc_sym(Tag, UC_Tag),
	member(UC_Tag, ['DATA', 'DATAFILE', 'EOL', 'EOLCODE', 'FIELDS', 'FIELDSFILE', 'RESULT', 'RESULTFILE', 'URL', 'POST']).

check_curl_opt(Tag=_) 
	:-
	make_uc_sym(Tag, UC_Tag),
	not lookup_opt_info(UC_Tag),
	!,
        domain_error(curl_option,Tag=_,4).

check_curl_opt(Opt) 
	:-
	type_error('equation (_=_)', Opt,4).
	

/*----------------------------------------------------------*
 | check_alias/3
 | check_alias(Options, Source_sink, Alias)
 | check_alias(+, +, -)
 |
 |	Checks for an alias 'Alias' and determines whether or 
 |	not it is a valid alias for a new stream open; 
 |	returns the alias found in the variable 'Alias'.
 |	Under some circumstances (eg, for a tk_win stream),
 |	will generate an alias if one is not found on Options. 
 |	Normally, if no alias is found, binds Alias to no(alias)
 *----------------------------------------------------------*/


check_alias(Options, Source_sink, Alias) 
	:-
	dmember(alias(Alias), Options),
	!,
	check_alias(Alias).

	%% No alias; use WinName as one for tk_windows:
check_alias(Options, tk_win(_,WinName), Alias) 
	:-!,
	(atom(WinName) -> Alias = WinName ; sprintf(atom(Alias), '%t', [WinName]) ),
	check_alias(Alias).

check_alias(Options, Source_sink, no(alias)).

check_alias(Var) :-
	var(Var),
	!,
	instantiation_error(2).
check_alias(Alias) :-
	get_alias(Alias,_),
	!,
	permission_error(open,source_sink,alias(Alias),2).
check_alias(Alias).
	
alias_extend_opts(no(alias), Options, Options)
	:-!.
alias_extend_opts(Alias, Options, Options)
	:-
	dmember(alias(Alias), Options),
	!.
alias_extend_opts(Alias, Options, [alias(Alias) | Options]).

/*
 * check_repositionability/4 checks to see if the positionability option
 * specified is valid for the given stream.  If there is no specified
 * option, it will attempt to discern it.
 */

check_repositionability(Source_sink,Mode,Options,Positionability) :-
	dmember(reposition(Positionability),Options),
	!,
	check_repositionability(Positionability,Source_sink,Mode).

check_repositionability(Source_sink,Mode,Options,true) 
	:-
	is_repositionable(Source_sink,Mode),
	!.

check_repositionability(Source_sink,Mode,Options,false).

check_repositionability(Var,_,_) :-
	var(Var),
	!,
	instantiation_error(2).
check_repositionability(false,_,_) :- !.
check_repositionability(true,Source_sink,Mode) :-
	is_repositionable(Source_sink,Mode),
	!.
check_repositionability(true,Source_sink,Mode) :-
	!,
	permission_error(open, source_sink, reposition(true),2).
check_repositionability(A,_,_) :-
	domain_error(stream_option,reposition(A),2).

is_repositionable(FileName,_) :-
	atom(FileName),
	!.

/*
 * open_stream(Source_sink,Mode,Options,Stream)
 *
 *	Performs the open of Source_sink.
 */

open_stream(null_stream(Name),Mode,Options,Stream) 
	:-!,
	open_null_stream(Name,Mode,Options,Stream).

open_stream(Source_sink,Mode,Options,Stream) 
	:-
	atom(Source_sink),
	!,
	open_file_stream(Source_sink,Mode,Options,Stream).

open_stream(sysV_queue(Key),Mode,Options,Stream) 
	:-!,
	open_sysVq_stream(Key,Mode,Options,Stream).

open_stream(ssbq(Q_Name),Mode,Options,Stream) 
	:- !,
	open_ssbq_stream(Q_Name,Mode,Options,Stream).

open_stream(nsocket(Socket),Mode,Options,Stream) 
	:- !,
	open_nsocket_stream(Socket, Mode, Options, Stream).
	
open_stream(Socket,Mode,Options,Stream) 
	:-
	functor(Socket,socket,_),
	!,
	open_socket_stream(Socket,Mode,Options,Stream).

open_stream(string(String),Mode,Options,Stream) 
	:- !,
	(nonvar(String),!, String = [_|_]; true),
	open_string_stream(String,Mode,Options,Stream).

open_stream(code_list(String),Mode,Options,Stream) 
	:- !,
	(nonvar(String),!, String = [_|_]; true),
	open_string_stream(String,Mode,Options,Stream).

open_stream(char_list(String),Mode,Options,Stream) 
	:- !,
	(nonvar(String),!, String = [_|_]; true),
	open_char_list_stream(String,Mode,Options,Stream).

open_stream(atom(Atom),Mode,Options,Stream) 
	:- !,
	(nonvar(Atom),!,atom(Atom); true),
	open_atom_stream(Atom,Mode,Options,Stream).

open_stream(window(WinName),Mode,Options,Stream) 
	:- !,
	open_window_stream(WinName,Mode,Options,Stream).

open_stream(tk_win(Interp,WinName),Mode,Options,Stream) 
	:- !,
	open_tk_window_stream(WinName,Interp,Mode,Options,Stream).

open_stream(console(Name), Mode, Options, Stream) 
	:- !,
	open_console_stream(Name, Mode, 0, Options, Stream).
	
open_stream(console_error(Name), Mode, Options, Stream) 
	:- !,
	open_console_stream(Name, Mode, 1, Options, Stream).

open_stream(tcl_transfer(Interp,CmdTemplate),Mode,Options,Stream) 
	:- !,
	open_tcl_transfer_stream(CmdTemplate,Interp,Mode,Options,Stream).

open_stream(url(URL),Mode,NonURLOptions,Stream) 
	:- !,
	open_stream(url(URL,[]),Mode,NonURLOptions,Stream).

open_stream(url(URL,URLOptions),Mode,NonURLOptions,Stream) 
	:- !,
	open_url_stream(URL,Mode,NonURLOptions,URLOptions,Stream).

%%
%% This is the place to put in clauses for dealing with other types of streams
%%

		/*-------------*
		 | NULL STREAM
		 *-------------*/

open_null_stream(Source_sink,Mode,Options,Stream)
	:-
	initialize_stream(null_stream, Source_sink, Options, Stream),
	file_modes(Mode,NMode,SMode),
	set_stream_mode(Stream,SMode),
	buffering(Options,NBuffering),
	eoln_modes(null_stream, Options, NEoln),
	sio_generic_open(0,Stream,NMode,NBuffering,NEoln).
open_null_stream(Source_sink,Mode,Options,Stream) :-
	permission_error(open,source_sink,Source_sink,2).



		/*---------*
		 |   FILES |
		 *---------*/

open_file_stream(Source_sink,Mode,Options,Stream) 
	:-
	initialize_stream(file, Source_sink, Options, Stream),
	file_modes(Mode,NMode,SMode),
	set_stream_mode(Stream,SMode),
	buffering(Options,NBuffering),
	eoln_modes(file, Options, NEoln),
	sio_file_open(Source_sink,Stream,NMode,NBuffering,NEoln),
	!.

open_file_stream(Source_sink,Mode,Options,Stream) 
	:-
	%% FIXME: Incorporate errno into error.
	%%permission_error(open,source_sink,Source_sink,2).
	existence_error(source_sink,Source_sink,2).

initialize_stream(StreamType,Source_sink,Options,Stream) 
	:-
	bufsize(StreamType,Options,BufSize),
	sio_mkstream(BufSize,Stream),
	set_stream_open_status(Stream,open),
	set_stream_type(Stream,StreamType),
	set_stream_name(Stream,Source_sink),
	set_stream_repositionability(Stream,false),
	next_stream_identifier(Options,Id),
	set_stream_identifier(Stream,Id),
	wt_init_options(Options,Stream),	%% initialize wt options
	prompt_goal(Options,PromptGoal),
	set_stream_pgoals(Stream,PromptGoal),
	set_stream_syntax_errors(Stream,0),
	set_stream_token_list(Stream,[]),
	get_option(Options,eof_action,eof_code,EOFAction),
	set_stream_eof_action(Stream,EOFAction),
	get_option(Options,type,text,SType),
	set_stream_stype(Stream,SType),
	default_snr(StreamType, DefSNR),
	get_option(Options,snr_action,DefSNR,SNRAction),
	set_stream_snr_action(Stream,SNRAction),

	default_blocking(StreamType, DefBlocking),
	get_option(Options,blocking,DefBlocking,Blocking),
	set_stream_blocking(Stream,Blocking).

default_snr(window, snr_code).
default_snr(tk_win, snr_code).
default_snr(StreamType, wait).

default_blocking(window, false) :-!.
default_blocking(tk_win, true) :-!.
default_blocking(Type, false) :- functor(Type, socket,_), !.
default_blocking(_, true).


%%
%% get_option(Options,What,Initial,Value)
%%
%% The standard states that in the case of conflicting options, the
%% rightmost option is used.  This procedure gets the rightmost option
%% from the options list.
%%

get_option([],_,V,V) :-
	!.
get_option([S | T],F,_,V2) :-
	functor(S,F,1),
	!,
	arg(1,S,V1),
	get_option(T,F,V1,V2).
get_option([_ | T],F,V1,V2) :-
	get_option(T,F,V1,V2).
		

		/*-------------------*
		 |   SYSTEM V IPC Qs |
		 *-------------------*/

open_sysVq_stream(Key,Mode,Options,Stream) :-
	get_ipc_key(Key, IPC_Key),
	initialize_stream(sysV_queue,sysVq_stream(Key,IPC_Key),Options, Stream),
	read_write_modes(Mode,NMode,SMode),
	set_stream_mode(Stream,SMode),
	buffering(Options,NBuffering),
	%% '$$msgget'(Key, PermsFlag, RetVal),
	make_perms_flag(Mode, Options, PermsFlag),
	get_message_type(Options, MessType),
	get_message_wait(Options, MessWait),
	sio_sysVq_open(IPC_Key,Stream,NMode,NBuffering,
					    PermsFlag,MessType,MessWait),
	!.
open_sysVq_stream(Key,Mode,Options,Stream) :-
	permission_error(open,source_sink,Source_sink,2).

get_ipc_key(Key, Key) :-
	integer(Key),
	!.
get_ipc_key(key(File,ID), IPC_Key) :-
	ftok(File,ID,IPC_Key).			%%%%% where is this defined ???? - raman

make_perms_flag(Mode, Options, PermsFlag) :-
	get_perms_flag(Mode, Options, PermsFlag0),
	check_create(Options, PermsFlag0, PermsFlag).

get_perms_flag(Mode, Options, PermsFlag) :-
	dmember(perms(PermsFlag),Options), !.
get_perms_flag(Mode, Options, PermsFlag) :-
	dmember(perms(Owner,Group,Others),Options),
	decode_ipc_perms(Owner, OwnI),
	decode_ipc_perms(Group, GrpI),
	decode_ipc_perms(Others, OthI),
	!,
	PermsFlag is OwnI*64 + GrpI*8 + OthI.
get_perms_flag(Mode, Options, 438).		% 0666

decode_ipc_perms(w,2).
decode_ipc_perms(r,4).
decode_ipc_perms(rw,6).

check_create(Options, PermsFlag0, PermsFlag) :-
	dmember(create, Options),
	!,
	PermsFlag is PermsFlag0 \/ 512.  % IPC_CREATE in <sys/ipc.h>
check_create(Options, PermsFlag, PermsFlag).

get_message_type(Options, MessType) :-
	dmember(msg_type(MessType), Options), 
	!.
get_message_type(Options, 1).

get_message_wait(Options, MessWait) :-
	dmember(msg_wait(yes),Options),
	!,
	MessWait is not(2024).		% ~IPC_NOWAIT in <sys/ipc.h>
get_message_wait(Options, 2024).



		/*------------*
		 |   SSB Qs   |
		 *------------*/

	%% Recommended: SIGUSR1 (30) or SIGUSR2 (31)
ssbq_signal_num(31). 

check_ssbq_initialize(_) :-
	ssbq_system_initialized,
	!.
check_ssbq_initialize(_) :-
	sio_ssbq_initialize,
	assert(ssbq_system_initialized).

open_ssbq_stream(Q_Name,Mode,Options,Stream) :-
	check_ssbq_initialize(Mode),
%	make_perms_flag(Mode, Options, PermsFlag),
	get_message_wait(Options, MessWait),
	initialize_stream(ssbq,Q_Name,Options,Stream),
	read_write_modes(Mode,NMode,SMode),
	set_stream_mode(Stream,SMode),
	target_node(Mode,Options,TargetNode),
	buffering(Options,NBuffering),
	sio_ssbq_open(Q_Name,Stream,NMode,NBuffering,TargetNode,MessWait),
	!.
open_ssbq_stream(Q_Name,Mode,Options,Stream) :-
	permission_error(open,source_sink,Source_sink,2).

target_node(read,_,'').
target_node(write,Options,TargetNode) :-
	dmember(target_node(TargetNode), Options).

		/*------------*
		 |   SOCKETS  |
		 *------------*/

/* Error checking */
		 
check_instantiation(Object) :- nonvar(Object).
check_instantiation(Object) :- instantiation_error(2).

check_var(V) :- var(V).
check_var(V) :- type_error(variable, V, 2).

check_atom(A) :- atom(A).
check_atom(A) :- type_error(atom, A, 2).

check_integer(I) :- integer(I).
check_integer(I) :- type_error(integer, I, 2).

atom_const_error(Object, Domain) :- atomic(Object), domain_error(Domain, Object, 2).
atom_const_error(Object, Domain) :- type_error(atomic, Object, 2).

check_family(Family, _) :- not check_instantiation(Family).
check_family(internet, 0).
%%check_family(unix, 1).
%%check_family(appletalk, 2).
check_family(Family, _) :- atom_const_error(Family, nsocket_family).

check_type(Type, _) :- not check_instantiation(Type).
check_type(stream, 0).
check_type(Type, _) :- atom_const_error(Type, nsocket_type).

check_protocol(Protocol, _) :- not check_instantiation(Protocol).
check_protocol(0, 0).
check_protocol(Protocol, _) :- atom_const_error(Type, nsocket_protocol).

nsocket_check_result(Result) :- nonvar(Result), Result = 0.
nsocket_check_result(Error) :- system_error(Error). % system error.

is_nsocket(S) :- nonvar(S), S = nsocket(_, _, _, _).

check_nsocket(nsocket(_, _, _, _)).
check_nsocket(Object) :- type_error(nsocket, Object, 2).

/* nsocket data access predicates.
   Note: assumes check_socket has been called. */
nsocket_family_code(nsocket(FamilyCode, _, _, _), FamilyCode).

nsocket_type_code(nsocket(_, TypeCode, _, _), TypeCode).

nsocket_descriptor(nsocket(_, _, _, SocketDescriptor), SocketDescriptor).

/* Public predicates. */

export nsocket/4.
nsocket(Family, Type, Protocol, Socket) :-
	check_family(Family, FamilyCode),
	check_type(Type, TypeCode),
	check_protocol(Protocol, ProtocolCode),
	check_var(Socket),
	sio_nsocket(FamilyCode, TypeCode, ProtocolCode, SocketDescriptor, Result),
	nsocket_check_result(Result),
	Socket = nsocket(FamilyCode, TypeCode, ProtocolCode, SocketDescriptor).

export nsocket_connect/3.
nsocket_connect(Socket, Address, Port) :-
	check_nsocket(Socket),
	check_atom(Address),
	check_integer(Port),
	nsocket_family_code(Socket, FamilyCode),
	nsocket_descriptor(Socket, SocketDescriptor),
	sio_nsocket_connect(FamilyCode, SocketDescriptor, Address, Port, Result),
	nsocket_check_result(Result).
	
export nsocket_bind/2.
nsocket_bind(Socket, Port) :-
	check_nsocket(Socket),
	check_integer(Port),
	nsocket_family_code(Socket, FamilyCode),
	nsocket_descriptor(Socket, SocketDescriptor),
	sio_nsocket_bind(FamilyCode, SocketDescriptor, Port, Result),
	nsocket_check_result(Result).

export nsocket_listen/2.
nsocket_listen(Socket, Backlog) :-
	check_nsocket(Socket),
	check_integer(Backlog),
	nsocket_family_code(Socket, FamilyCode),
	nsocket_descriptor(Socket, SocketDescriptor),
	sio_nsocket_listen(FamilyCode, SocketDescriptor, Backlog, Result),
	nsocket_check_result(Result).
	
export nsocket_accept/3.
nsocket_accept(Socket, Peer, NewSocket) :-
	check_nsocket(Socket),
	check_var(Peer),
	check_var(NewSocket),
	Socket = nsocket(FamilyCode, TypeCode, ProtocolCode, SocketDescriptor),
	sio_nsocket_accept(FamilyCode, SocketDescriptor, Peer, NewSocketDescriptor, Result),
	nsocket_check_result(Result),
	NewSocket = nsocket(FamilyCode, TypeCode, ProtocolCode, NewSocketDescriptor).

export nsocket_close/1.
nsocket_close(Socket) :-
	check_nsocket(Socket),
	nsocket_descriptor(Socket, SocketDescriptor),
	sio_nsocket_close(SocketDescriptor, Result),
	nsocket_check_result(Result).
	
extract_descriptor([], []).
extract_descriptor([Socket | SRest], [D | DRest]) :-
	is_nsocket(Socket),
	nsocket_descriptor(Socket, D),
	extract_descriptor(SRest, DRest).
extract_descriptor([Stream_or_alias | SRest], [D | DRest]) :-
	is_stream(Stream_or_alias, Stream),
	stream_name(Stream, Socket),
	is_nsocket(Socket),
	nsocket_descriptor(Socket, D),
	extract_descriptor(SRest, DRest).
extract_descriptor([Object | _], _) :-
	type_error(nsocket_stream, Object, 2).

calc_time(Sec:USec, Sec, USec).
calc_time(off, off, off).
calc_time(X, _, _) :- domain_error(select_time, X, 2).

export nsocket_select/7.
nsocket_select(ReadList, WriteList, ExceptionList, ReadMark, WriteMark, ExceptionMark, Time) :-
	extract_descriptor(ReadList, RSList),
	extract_descriptor(WriteList, WSList),
	extract_descriptor(ExceptionList, ESList),
	calc_time(Time, Sec, USec),
	sio_nsocket_select(RSList, WSList, ESList, ReadMark, WriteMark, ExceptionMark, Sec, USec, Result),
	nsocket_check_result(Result).

export nsocketpair/2.
nsocketpair(S0, S1) :-
	check_var(S0),
	check_var(S1),
	sio_nsocketpair(D0, D1, Result),
	nsocket_check_result(Result),
	% A little White Lie:
	% The family is actually unix, but unix isn't supported yet,
	% so make fake internet sockets.
	S0 = nsocket(internet, stream, 0, D0),
	S1 = nsocket(internet, stream, 0, D1).


open_nsocket_stream(Socket,Mode,Options,Stream) :-
	initialize_stream(nsocket,Socket,Options,Stream),
	read_write_modes(Mode,NMode,SMode),
	set_stream_mode(Stream,SMode),
	buffering(Options,NBuffering),
	eoln_modes(socket, Options, NEoln),
	nsocket_descriptor(Socket, SocketDescriptor),
	nsocket_type_code(Socket, TypeCode),
	sio_nsocket_open(SocketDescriptor,TypeCode,NMode,NBuffering,NEoln,Stream),
	!.
%% FIXME: Needs to be expanded to more accurately report errors.
open_nsocket_stream(Socket,Mode,Options,Stream) :-
	permission_error(open,source_sink,Socket,2).


export gethostbyname/2.
gethostbyname(Name, Address) :- gethostbyname(Name, _, _, [Address]).
export gethostbyaddr/2.
gethostbyaddr(Address, Name) :- gethostbyaddr(Address, Name, _, _).
	

open_socket_stream(Description,Mode,Options,Stream) :-
	socket_params(Description,HostOrPath,Port,Domain,Type,SDescr),
	SN = socket(0,0,Domain,Type),
	initialize_stream(socket,SN, Options,Stream),
	read_write_modes(Mode,NMode,SMode),
	set_stream_mode(Stream,SMode),
	buffering(Options,NBuffering),
	eoln_modes(socket, Options, NEoln),
	get_socket_connects(QLen,Options),
	socket_codes(Domain,Dom, Type, Typ),
	sio_socket_open(HostOrPath,Port,Dom,Typ,NMode,NBuffering,NEoln,QLen,
				SDescr,Stream),
	!,
	mangle(1,SN,HostOrPath),	%% Fill these in in case of vars
	mangle(2,SN,Port).
%% FIXME: Needs to be expanded to more accurately report errors.
open_socket_stream(Socket,Mode,Options,Stream) :-
	permission_error(open,source_sink,Socket,2).


socket_params(socket(unix,PathName), PathName, 0, unix, stream, 0) :-
	atom(PathName),
	PathName \= [],
	!.
socket_params(socket(clone,Stream_or_alias),
	      HostOrPath,Port,Domain,Type,Stream) :-
	!,
	is_stream(Stream_or_alias,Stream),
	stream_name(Stream,socket(HostOrPath,Port,Domain,Type)).
socket_params(socket(inet_stream, HostName), HostName, Port, inet, stream, 0) :-
	!,
	socket_port_default(Port).
socket_params(socket(inet_stream, HostName, Port),
	      HostName, Port, inet, stream, 0) :-
	!.
socket_params(socket(inet_dgram, HostName, Port),
	      HostName, Port, inet, dgram, 0) :-
	!.

export socket_stream_info/5.
socket_stream_info(Stream, HostOrPath,Port,Domain,Type)
	:-
	stream_name(Stream, socket(HostOrPath,Port,Domain,Type) ).


%%
%% We can use the same port number for both datagram and stream sockets
%% since stream sockets use TCP and datagram sockets use UDP.  At the lower
%% levels, the protocol is added as part of the address.  So even though
%% the port numbers are the same (between TCP and UDP), the actual addresses
%% are different.  This code was previously written to use different port
%% numbers.
%%
%% The convention (see /etc/services) seems to be to make the port number
%% the same (whenever possible) for the same service.  I don't know what
%% service ALS-Prolog is providing, but it seems to make sense to use
%% the same port number for both datagram and stream sockets so long as
%% they don't interfere with one another.
%%

socket_port_default(1599).

socket_codes(Domain,Dom, Type, Typ) :-
	socket_domain_codes(Domain,Dom),
	socket_type_codes(Type, Typ).

	%% These are the #define values from <sys/socket.h>

socket_domain_codes(unix, 1).
socket_domain_codes(inet, 2).
socket_domain_codes(ppc,  5).
socket_domain_codes(appletalk, 16).

socket_type_codes(stream, 1).
socket_type_codes(dgram, 2).

get_socket_connects(SocketConnects,Options) :-
	dmember(connects(SocketConnects),Options),
	SocketConnects > 0, !.
get_socket_connects(1,_).
	
/*
 * is_server_socket/1 will succeed if its single argument represents a
 * socket which is waiting for a connection.
 */

export is_server_socket/1.
is_server_socket(Stream_or_alias) :-
	stream_or_alias_ok(Stream_or_alias, Stream),
	sio_is_server_socket(Stream).

/*
 * accept_socket_connection/1 will explicitly accept a socket connection.
 */

export accept_socket_connection/1.
accept_socket_connection(Stream_or_alias) 
	:-
	accept_socket_connection(Stream_or_alias, _).

export accept_socket_connection/2.
accept_socket_connection(Stream_or_alias, ConnectionAddr)
	:-
	stream_or_alias_ok(Stream_or_alias, Stream),
	sio_accept_socket_connection(Stream, ConnectionAddr).

export poll/2.
poll(Stream_or_alias, TimeOut) :-
	stream_or_alias_ok(Stream_or_alias, Stream),
	integer_ok(TimeOut),
	sio_cpos(Stream, CPOS),
	sio_lpos(Stream, LPOS),
	poll(CPOS, LPOS, Stream, TimeOut).

poll(CPOS, LPOS, _, _) :-
	CPOS < LPOS,
	!.
poll(_,_,Stream,TimeOut) :-
	sio_poll(Stream, TimeOut).

/*--------------------------------------------------------------*
 |  simple_select/2
 |  simple_select(List, Timeout)
 |  simple_select(+, +)
 |
 |	List is a list of read streams, and Timeout is an integer 
 |	(indicating a number of milliseconds).  
 |
 |	Implements the functionality of unix select for these read
 |	streams without telling what streams are actually ready.
 *--------------------------------------------------------------*/

	%% FIXup needed -  Handle window streams correctly.

export simple_select/2.
simple_select([], _) :-!.

simple_select(StreamList, Timeout)
	:-
	integer(Timeout),
	no_immed_streams(StreamList),
	try_select(StreamList, Timeout).

no_immed_streams([]).
no_immed_streams([S | Streams])
	:-
	is_input_stream(S),
	stream_type(S, ST),
	not(always_ready(ST)),
	no_immed_streams(Streams).

always_ready(file).
always_ready(string).
always_ready(atom).
always_ready(console).

try_select(StreamList, Timeout)
	:-
	sio_simple_select(StreamList, Timeout, Return),
	!,
	disp_try_select(Return, StreamList, Timeout).

try_select(StreamList, Timeout)
	:-
	try_select(StreamList, Timeout).

	%% Return Arg = 0 (timeout) or > 0: # of events triggered:
disp_try_select(Return, StreamList, Timeout)
	:-
	Return >= 0,
	!.

	%% Return arg (Arg#3) = negative of error return code (-4);
	%% EINTR = 4 (signal delivered before selected events or timeout):
	%% Go back into select:
disp_try_select(Return, StreamList, Timeout)
	:-!,
	try_select(StreamList, Timeout).

/* Other errors: should raise appropriate exception:
disp_try_select(Return, StreamList, Timeout)
	:-
*/

		/*------------*
		 |   FORK    |
		 *------------*/

export fork/1.

fork(ID) :-
	sio_fork(ID).

		/*------------*
		 |   REXEC    |
		 *------------*/

export rexec/2.

rexec(Command, Options) :-
	rexec_defaults(RD),
	rexec_get_options(Options,RD),
	rexec_params(RD, Host,User,Password,RStream,WStream,EStream),
	rexec_open0(RStream, read, RS, RSAlias),
	rexec_open0(WStream, write, WS, WSAlias),
	rexec_open0(EStream, read, ES, ESAlias),
	sio_rexec(Host,Command,User,Password,RS,WS,ES),
	rexec_open1(RS,RSAlias),
	rexec_open1(WS,WSAlias),
	rexec_open1(ES,ESAlias).


rexec_defaults(rd(   0,   0,       0,      0,      0,      0)).
	/*     rd(Host,User,Password,RStream,WStream,EStream) */

rexec_params(rd(Host,User,Password,RStream,WStream,EStream),
	     Host, User, Password, RStream, WStream, EStream).

rexec_get_options([],RD) :-
	!.
rexec_get_options([Opt|Opts],RD) :-
	rexec_get_option(Opt,RD),
	!,
	rexec_get_options(Opts,RD).

rexec_get_option(host(Host),RD) :-
	mangle(1,RD,Host).
rexec_get_option(username(User),RD) :-
	mangle(2,RD,User).
rexec_get_option(password(Password),RD) :-
	mangle(3,RD,Password).
rexec_get_option(rstream(S,OpenOpts),RD) :- 
	mangle(4,RD,s(S,OpenOpts)).
rexec_get_option(wstream(S,OpenOpts),RD) :-
	mangle(5,RD,s(S,OpenOpts)).
rexec_get_option(estream(S,OpenOpts),RD) :-
	mangle(6,RD,s(S,OpenOpts)).


rexec_open0(0, _, 0, no(alias)) :-
	!.
rexec_open0(s(Stream,Opts), Mode, Stream, Alias) :-
	var_ok(Stream),
	check_open_options(Opts),
	check_alias(Opts, nil, Alias),
	initialize_stream(socket,socket(0,0,inet,stream),Opts,Stream),
	file_modes(Mode,NMode,SMode),
	set_stream_mode(Stream,SMode),
	buffering(Opts,NBuffering),
	eoln_modes(socket, Opts, NEoln),
	sio_generic_open(0,Stream,NMode,NBuffering,NEoln).



rexec_open1(0,_) :-
	!.
rexec_open1(Stream,Alias) :-
	(Alias = no(alias) -> true ; set_alias(Alias,Stream)),
	stream_identifier(Stream,Id),
	set_stream_table(Id,Stream),
	set_stream_repositionability(Stream,false).

		/*------------*
		 |   STRINGS  |
		 *------------*/

open_string_stream(Source_sink,read,Options,Stream) :-
	initialize_stream(string,string(Source_sink),Options,Stream),
	set_stream_extra(Stream,Source_sink),
	file_modes(read,NMode,SMode),
	set_stream_mode(Stream,SMode),
	buffering(Options,NBuffering),
	eoln_modes(string, Options, NEoln),
	sio_generic_open(0,Stream,NMode,NBuffering,NEoln),
	!.
open_string_stream(Source_sink,write,Options,Stream) :-
	initialize_stream(string,string(Source_sink),Options,Stream),
	set_stream_extra(Stream,[]),		%% head of list stream
	set_stream_addl1(Stream,[]),		%% last cons cell of list stream
	file_modes(write,NMode,SMode),
	set_stream_mode(Stream,SMode),
	buffering(Options,NBuffering),
	eoln_modes(string, Options, NEoln),
	sio_generic_open(0,Stream,NMode,NBuffering,NEoln),
	!.
open_string_stream(Source_sink,Mode,Options,Stream) :-
	permission_error(open,source_sink,Source_sink,2).

		/*------------*
		 | CHAR LIST  |
		 *------------*/

open_char_list_stream(Source_sink,read,Options,Stream) :-
	atom_chars(Atom,Source_sink),
	open_atom_stream(Atom,read,Options,Stream),
	!.
open_char_list_stream(Source_sink,write,Options,Stream) :-
	open_atom_stream(Atom,write,Options,Stream),
	set_stream_addl1(Stream,atom_chars(Atom,Source_sink)),
	!.
open_char_list_stream(Source_sink,Mode,Options,Stream) :-
	permission_error(open,source_sink,Source_sink,2).

		/*-----------------*
		 |   SYMBOLS/UIAS  |
		 *-----------------*/

open_atom_stream(Atom,read,Options,Stream) :-
	initialize_stream(atom,atom(Atom),Options,Stream),
	set_stream_extra(Stream,Atom),
	set_stream_addl1(Stream,0),
	atom_length(Atom,ALen),
	set_stream_addl2(Stream,ALen),
	file_modes(read,NMode,SMode),
	set_stream_mode(Stream,SMode),
	buffering(Options,NBuffering),
	eoln_modes(atom, Options, NEoln),
	sio_generic_open(0,Stream,NMode,NBuffering,NEoln),
	!.
open_atom_stream(Source_sink,write,Options,Stream) :-
	initialize_stream(atom,atom(Source_sink),Options,Stream),
	set_stream_extra(Stream,''),
	set_stream_addl1(Stream,true),
	file_modes(write,NMode,SMode),
	set_stream_mode(Stream,SMode),
	buffering(Options,NBuffering),
	eoln_modes(atom, Options, NEoln),
	sio_generic_open(0,Stream,NMode,NBuffering,NEoln),
	!.
open_atom_stream(Source_sink,Mode,Options,Stream) :-
	permission_error(open,source_sink,Source_sink,2).


/*
	CONSOLES
*/

open_console_stream(Source_sink, Mode, ErrorMode, Options, Stream)
	:-
	initialize_stream(console, Source_sink, Options, Stream),
	file_modes(Mode,NMode,SMode),
	set_stream_mode(Stream,SMode),
	buffering(Options,NBuffering),
	eoln_modes(console, Options, NEoln),
	sio_console_open(Source_sink,Stream,NMode,NBuffering,NEoln,ErrorMode),
	!.
open_console_stream(Source_sink,Mode,ErrorMode,Options,Stream) :-
	permission_error(open,source_sink,Source_sink,2).

/* ------------------------------- *
	URL Streams
 * ------------------------------- */

open_url_stream(Source_sink,read,NonURLOptions,URLOptions,Stream)
	:-
	uppercase_unwind(URLOptions, UCOptions),
	delete_from(UCOptions, 'RESULT', RemURLOptions, Vals),
	(Vals = [VV] -> WW = VV ; true),

	http(get, Source_sink, ['RESULT'=WW | RemURLOptions]),
	open(atom(WW), read, Stream, NonURLOptions).

open_url_stream(URL,write,NonURLOptions,URLOptions,Stream)
	:-
	open(string(StreamString), write, Stream, NonURLOptions),
	freeze(StreamString, 
	    (atom_codes(StringAtom, StreamString),
	     http(post, URL, [data=StringAtom | URLOptions])  )
	).

		/*-----------*
		 |  WINDOWS  |
		 *-----------*/

export data_ready/1.
data_ready(Stream)
	:-
	stream_extra(Stream,LineQ),
	LineQ \= [],
	!.

:-dynamic(setStreamId/2).
:-dynamic(getStreamId/2).

wait_data(tk_win, Stream, Call) 
	:-!,
	wait_data(tcltk, Stream, Call).

wait_data(tcltk, Stream, Call) 
	:-!,
	stream_addl1(Stream, Alias),
	stream_addl2(Stream, ti(Interp,WaitVarName)),
	stream_name(Stream, WinID),
%pbi_write('sio.pro-wait_data:Alias'=Alias),pbi_write(' Interp'=Interp),pbi_write(' WV'=WaitVarName),pbi_write(' WinID'=WinID),pbi_nl,
	tcl_call(Interp, [set_prompt_mark, WinID], _),
	tcl_call(Interp, 
		[bind,WinID,'<Return>', [ xmit_line_plain,WinID,Alias,WaitVarName]], _),
	tcl_call(Interp, [set,WaitVarName,0], _),
	tcl_call(Interp, [wait_for_line1, WaitVarName], WaitRes),
	finish_wait_data_tcltk(WaitRes, Stream, Call).

	%% Returned -1: Got a ^C:
finish_wait_data_tcltk(-1, Stream, Call)
	:-!,
	force_control_c_during_read(Stream).

	%% Returned -3: User typed ^D:
finish_wait_data_tcltk(-3, Stream, Call)
	:-!,
	stream_extra(Stream,XTRA),
	set_stream_extra(Stream,eof(XTRA)),
	sio_set_eof(Stream),
	sio_set_errcode(Stream,8).			% SIO_INTERR

finish_wait_data_tcltk(WaitRes, Stream, Call)
	:-
	call(Call).

force_control_c_during_read(StreamAlias)
	:-
	stream_or_alias_ok(StreamAlias, Stream),
	sio_set_errcode(Stream,16),			% SIO_INTERR
	forceCtlC.

wait_data(sysV_queue, Stream, Call) 
	:-!,
	loop_for_data(sysV_queue, Stream),
	!,
	call(Call).

wait_data(socket, Stream, Call) :-
	sio_poll(Stream, 100000),	% 1 sec  - restore when 2nd arg fixed
%	sio_poll(Stream, 10000),	% 0.1 sec
	!,
	call(Call).

wait_data(socket, Stream, Call) :-
	wait_data(socket, Stream, Call).

	%% This default clause is supplied for all window systems;
	%% each window system asserta's its particular delay clause
	%% for wait_data/3.

	%% Default: don't know how to wait, or wait failed, so convert to eof:
wait_data(Type, Stream, Call) :-
	sio_set_errcode(Stream,8),			%% SIOE_EOF
	functor(Call,_,LastArg),
	arg(LastArg,Call,-1).

loop_for_data(Type, Stream)
	:-
	read_buffer(Type,Stream), 
	!.
loop_for_data(Type, Stream)
	:-
	loop_for_data(Type, Stream).

open_window_stream(Window,Mode,Options,Stream) 
	:- 
	getWinID(Window,WinID),
pbi_write(open_window_stream_WinID=WinID),pbi_nl,
	getWinGV(WinID,WinPosGV),
	initialize_stream(window,Window,Options,Stream),
	file_modes(Mode,NMode,SMode),
	set_stream_mode(Stream,SMode),
	buffering(Options,NBuffering),
	winsGetTextInsertionPosition(WinID, WinInsertPos),
	%sio_window_open(WinID,Stream,WinInsertPos,NMode,NBuffering,WinPosGV),
	sio_window_open(WinID,Stream,NMode,NBuffering,WinInsertPos,WinPosGV),

	%%
	%% To Ken: The WINS_POS_GV field was not being used at all (it was
	%% being set, but never accessed.  It looks like you have other
	%% mechanisms for dealing with this.  It also appears that the
	%% WINS_INSERT_POS field is underutilized.  I found some comments
	%% that you had made regarding this field.  It seems that you were
	%% contemplating removing the call which updates it as you are also
	%% using a global variable for this value.  Since that is the case,
	%% we may safely eliminate this field from the stream.  If you need
	%% it again, I would suggest using the sio_aux field for this purpose.
	%% We already have a mechanism for getting this value. A C function
	%% would have to be written to also set this value.
	%%
	%% Note that I've commented out the lines above which are no longer
	%% needed.  You may well have to do something to replace them
	%% though.
	%%				-- Kev
	%%

	TextBuffer = [],
	set_stream_extra(Stream,TextBuffer),	%% the Prolog side window buffer
	set_stream_addl1(Stream,TextBuffer),	%% the window insertion position
	    %% Note: extra/addl1 together maintain the text buffer as an
	    %% extensible list; addl1 points at the last cons pair in the
	    %% list and new entries are made by mangling the tail pointer,
	    %% so as to avoid any problems with resetting a normal extensible
	    %% list tail variable on backtracking; this initial value for
	    %% TextBuffer is a single entry of an empty line, which doesn't
	    %% cause a new line to go out.
	!.
open_window_stream(Window,Mode,Options,Stream) :-
	permission_error(open,source_sink,Source_sink,2).

getWinID(Window,WinID) :-
	'$text_winIDFor$'(Window,WinID),!.
getWinID(Window,Window).


%/******
getWinGV(WinID,WinPosGV) :-
	'$text_winGV$'(WinID,WinPosGV),
	!.
getWinGV(WinID,WinPosGV) :-      %% allocate a gvar for WinID if not previously alloced
	gv_alloc(WinPosGV),
	assert('$text_winGV$'(WinID,WinPosGV)). 
%******/



export create_wait_var_name/2.
create_wait_var_name(WinID, WaitVarName)
	:-
	catenate('WaitForLine', WinID, WaitVarName).

open_tk_window_stream(WinName,Interp,Mode,Options,Stream)
	:-
	(atom(WinName) -> WinID = WinName ; sprintf(atom(WinID), '%t', [WinName]) ),
	initialize_stream(tk_win,WinID,Options,Stream),

	(dmember(alias(Alias), Options) -> true ; Alias = WinName),
		%% store the stream alias:
	set_stream_addl1(Stream, Alias),
		%% store the tcl/tk interpreter:
%	set_stream_addl2(Stream, Interp),
	
	create_wait_var_name(WinID, WaitVarName),
	set_stream_addl2(Stream, ti(Interp,WaitVarName)),
		%% store the window id:
	set_stream_addl3(Stream, ''),
	file_modes(Mode,NMode,SMode),
	set_stream_mode(Stream,SMode),
	buffering(Options,NBuffering),
	eoln_modes(tk_win, Options, NEoln),
 	sio_generic_open(0,Stream,NMode,NBuffering,NEoln),
	TextBuffer = '',
	set_stream_extra(Stream,TextBuffer),	%% the Prolog side window buffer
	    %% Note: extrar maintains the text buffer;
		%% New lines are pasted onto the end using
		%% '$atom_concat", and the result is 
		%% set_stream_extra into the stream slot
	!.

open_tk_window_stream(WinName,Interp,Mode,Options,Stream)
	:-
	permission_error(open,source_sink,Source_sink,2).

open_tcl_transfer_stream(CmdPattern,Interp,Mode,Options,Stream)
	:-
	atom(CmdPattern),
	atom(Interp),
	catenate([Interp,CmdPattern],ID),
	initialize_stream(tcl_transfer,ID,Options,Stream),
	set_stream_addl2(Stream, Interp),
	set_stream_addl3(Stream, CmdPattern),
	file_modes(Mode,NMode,SMode),
	set_stream_mode(Stream,SMode),
	buffering(Options,NBuffering),
	sio_tk_win_open(ID,Stream,NMode,NBuffering).

	%% open_tk_window_stream(WinName,Interp,Mode,Options,Stream)
clone_stream(Stream, StreamName, StreamType, Mode, StreamClone)
	:-
	initialize_stream(tk_win,StreamName,[],StreamClone),
	set_stream_addl1(Stream, ''),
	stream_addl2(Stream, Interp),
	set_stream_addl2(StreamClone, Interp),
	set_stream_addl3(Stream, ''),

	stream_mode(Stream,SMode),
	set_stream_mode(StreamClone,SMode),
	file_modes(Mode,NMode,_),
	buffering([],NBuffering),
	sio_tk_win_open(StreamName,StreamClone,NMode,NBuffering),
	TextBuffer = '',
	set_stream_extra(StreamClone,TextBuffer),	%% the Prolog side window buffer
	!.


/*
 * bufsize/3 determines the buffer size to use by examining the options
 * list and returning the default value if option not set
 * The first argument allows for different default sizes
 * according to the stream type.
 */

bufsize(_,Options,Size) :-
	member(bufsize(Size),Options),
	!.
bufsize(_,_,1024).		%% Default buffer size

/*
 * prompt_goal/2 attempts to find the prompt goal from the options list
 * and returns true if none is found.
 */

prompt_goal(Options,PromptGoal) :-
    member(prompt_goal(PromptGoal),Options),
    !.
prompt_goal(_,true).

/*
 * wt_init_options/2 looks for maxdepth(N), line_length(N), and
 * 	depth_computation(N) in the options list.  Once found these
 *	values are installed in the stream (passed in as the second
 *	argument)
 */

wt_init_options(Options,Stream) :-
	wt_opts_default(WT_OPTS),
	set_stream_wt_opts(Stream,WT_OPTS),
	wt_init_opts(Options,Stream).

wt_init_opts([],Stream) :- !.
wt_init_opts([H|T],Stream) :-
    wt_init_opt(H,Stream),
    wt_init_opts(T,Stream).

wt_init_opt(maxdepth(N),Stream) :-
    integer(N),
    N >= 0,
    !,
    set_stream_wt_maxdepth(Stream,N).
wt_init_opt(line_length(L),Stream) :-
    integer(N),
    N > 4,
    !,
    set_stream_wt_line_length(Stream,L).
wt_init_opt(depth_computation(D),Stream) :-
    (D = flat ; D= nonflat),
    !,
    set_stream_wt_depth_computation(Stream,D).
wt_init_opt(_,_).


/*
 * wt_opts_default returns the default term to set the stream_wt_opts field
 * to in a stream descriptor.
 */

%wt_opts_default(wt_opts(78,40000,flat)).
wt_opts_default(wt_opts(78,400,flat)).

export associated_output_alias/2.
export set_associated_output_alias/2.

associated_output_alias(InStreamAlias, OutStreamAlias)
	:-
	current_alias(InStreamAlias, InStream),
	stream_addl3(InStream, OutStreamAlias).

set_associated_output_alias(InStreamAlias, OutStreamAlias)
	:-
	current_alias(InStreamAlias, InStream),
	set_stream_addl3(InStream, OutStreamAlias).

/*
 * buffering/2 determines the type of buffering and returns the numeric
 * code to pass to the C primitive.
 */

buffering(Options, BufferingCode ) :-
	buffering_and_blocking(Options, BufferingCode, _).

buffering_and_blocking(Options, BufferingCode, BlockingAction) :-
	get_option(Options, buffering, block, Buffering),
	get_option(Options, snr_action, wait, BlockingAction),
	buffering_numbers(Buffering,BCode1),
	blocking_numbers(BlockingAction,BCode2),
	!,
	BufferingCode is BCode1 \/ BCode2.

buffering_numbers(block,0).		%% block buffering
buffering_numbers(line,1).		%% line buffering
buffering_numbers(byte,2).		%% byte buffering

blocking_numbers(wait, 0).		%% blocking I/O
blocking_numbers(error, 4).		%% non-blocking
blocking_numbers(snr_code, 4).		%% also non-blocking

/*
 * eoln_modes/2 determines the end-of-line type and returns the numeric
 * code to pass to the C primitive.
 */

eoln_modes(StreamType, Options, Code) :-
	default_read_eoln_type(StreamType, RDef),
	get_option(Options, read_eoln_type, RDef, RType),
	eoln_type_number(RType, RCode),
	default_write_eoln_type(StreamType, WDef),
	get_option(Options, write_eoln_type, WDef, WType),
	eoln_type_number(WType, WCode),
	Code is RCode \/ WCode << 2.

eoln_type_number(crlf, 0).
eoln_type_number(cr, 1).
eoln_type_number(lf, 2).
eoln_type_number(universal, 3).


/*
 * file_modes/3 enumerates the symbolic file mode names with the numbers
 * used internally for file modes.  The third argument gives the mode type for
 * storage in the stream descriptor.
 */

file_modes(read,0,[input|nooutput]).
file_modes(read_write,1,[input|output]).
file_modes(write,2,[noinput|output]).
file_modes(append,3,[noinput|output]).


/*
 * read_write_modes/3
 *
 * This is for streams which permit only reading and writing, but not both
 * reading and writing or the append mode.
 */

read_write_modes(read,0,[input|nooutput]).
read_write_modes(write,2,[noinput|output]).



/*
 * is_input_stream(Stream)
 *	Succeeds if the stream has mode type input.
 */

is_input_stream(Stream) :-
	stream_mode(Stream,[input|_]).

/*
 * is_output_stream(Stream)
 *	Succeeds if the stream has mode type output.
 */

is_output_stream(Stream) :-
	stream_mode(Stream,[_|output]).


/*
 * close(Stream_or_alias)
 */

export close/1.
export close/2.

close(Stream_or_alias) :-
	stream_or_alias_ok(Stream_or_alias,Stream),
	close_stream(Stream).

close(Stream_or_alias,Options) :-
	nonvar_ok(Stream_or_alias),		%% Minimal type checking
	check_list_options(Options,[force(true),force(false)]),
	(dmember(force(true),Options)
	    ->  catch(close(Stream_or_alias),_,true)
	    ;   close_stream(Stream_or_alias)
	),
	!.

close_stream(Stream) :-
	stream_identifier(Stream,StreamId),
	StreamId < 0,
	!.
close_stream(Stream) :-
	stream_open_status(Stream,open),
	set_stream_open_status(Stream,closed),
	remove_aliases(Stream),
	stream_type(Stream,StreamType),
	close_stream(StreamType,Stream),
	stream_identifier(Stream,StreamId),
	del_stream_table(StreamId,_),
	!.
close_stream(_).

close_stream(atom,Stream) :-
	is_output_stream(Stream),
	!,
	write_buffer(atom,Stream),
	stream_extra(Stream,Atom),
	stream_name(Stream,atom(Atom)),
	stream_addl1(Stream,CloseGoal),
	CloseGoal,
	!.
close_stream(atom,_) :-
	!.

close_stream(string,Stream) :-
	is_output_stream(Stream),
	!,
	write_buffer(string,Stream),
	stream_extra(Stream,List),
	stream_name(Stream,string(List)).
close_stream(string,_) :-
	!.

close_stream(tk_win,Stream) :-
	is_output_stream(Stream),
	!,
	write_buffer(tk_win,Stream).
close_stream(tk_win,_) :-
	!.

close_stream(tcl_transfer,Stream) :-
	is_output_stream(Stream),
	!,
	write_buffer(tcl_transfer,Stream).
close_stream(tcl_transfer,_) :-
	!.


%%
%% close code for other stream types should be placed here.
%%

close_stream(_,Stream) :-
	sio_close(Stream).

%%
%% close code for other stream types should be placed here.
%%




export close_down_streams/1.
close_down_streams(Type)
	:-
	pget_stream_table(Num,Stream),
	stream_type(Stream,Type),
	close(Stream),
	fail.

close_down_streams(_).
	
system_stream_aliases([
	user_input,
	user_output,
	warning_input,
	warning_output,
	error_stream,
	shl_tk_in_win,
	shl_tk_out_win,
	gui_debugger_input,
	gui_debugger_output,
	debugger_input,
	debugger_output
	]).

export close_down_nonsystem_streams/0.
close_down_nonsystem_streams
	:-
	pget_stream_table(Num,Stream),
	close_down_this_nonsystem_stream(Num,Stream),
	fail.
close_down_nonsystem_streams.

close_down_this_nonsystem_stream(Num,Stream)
	:-
	Num >= 0,
	stream_open_status(Stream, open),
	!,
	close(Stream),
	(pget_alias(Alias,Stream) ->
		pdel_alias(Alias,_)
		;
		true
	).
close_down_this_nonsystem_stream(Num,Stream).

/*
export close_down_nonsystem_streams/0.
close_down_nonsystem_streams
	:-
	system_stream_aliases(SystemAliases),
	pget_alias(Alias,Stream),
	(dmember(Alias, SystemAliases) ->
		true
		;
		(stream_open_status(Stream, open) ->
			close(Stream),
			pdel_alias(Alias,_)
			;
			true
		)
	),
	fail.

close_down_nonsystem_streams.
*/
	


%%
%% Remove aliases associated with a given stream.
%%
%% Also reassign the current default input and output streams to user
%% if the given stream is attached to either (or both).
%%

remove_aliases(Stream) :-
	pdel_alias(_,Stream),
	fail.
remove_aliases(Stream) :-
	get_current_input_stream(Stream),
	set_input(user),
	fail.
remove_aliases(Stream) :-
	get_current_output_stream(Stream),
	set_output(user),
	!.
remove_aliases(_).

/*
 * get_failure/3
 *
 *	Handles the failure conditions for input streams.
 */

get_failure(0, _, _) :-			%% SIOE_NORMAL
	!,
	fail.
get_failure(6, Stream, Call) :-		%% SIOE_ILLREAD
	!,
	curmod(Mod),
	permission_error(input,stream,Stream,Mod:Call).
get_failure(5, Stream, Call) :-		%% SIOE_READ
	!,
	get_failure_read(Stream, Call).
get_failure(2, Stream, Call) :-		%% SIOE_INARG
	curmod(Mod),
	functor(Call,_,LastArg),
	arg(LastArg,Call,Culprit),
	type_error(integer,Culprit,[Mod:Call]).
get_failure(15, Stream, Call) :-	%% SIOE_PARTNUM
	!,
	curmod(Mod),
	existence_error(number,Stream,Mod:Call).

/*
 * get_failure_read/2 is called when a new buffer needs to be read.
 * get_failure_read/3 is called when the call to read_buffer/2 fails.
 */

get_failure_read(Stream, Call) 
	:-
	stream_type(Stream,Type),
	read_buffer(Type,Stream),
	stream_eof_action(Stream, EOFAction),
	get_failure_read_maybe_reset_eof(EOFAction, Stream),
	!,
	call(Call).			%% restart call

get_failure_read(Stream, Call) 
	:-
	sio_errcode(Stream, Num),
	get_failure_read(Num, Stream, Call).

get_failure_read(8, Stream, Call) :-	%% SIOE_EOF
	!,
	stream_eof_action(Stream, EOFAction),
	functor(Call,Pred,ArgNum),
	get_failure_read_eof(EOFAction, Pred, ArgNum, Stream, Call).

get_failure_read(14, Stream, Call) :-	%% SIOE_NOTREADY
	!,
	stream_snr_action(Stream, SNRAction),
	functor(Call,Pred,ArgNum),
	get_failure_read_snr(SNRAction, Pred, ArgNum, Stream, Call).

get_failure_read(_, Stream, Call) :-
	curmod(Mod),
	sio_errno(Stream,ErrNo),
	system_error([Mod:Call,errno(ErrNo)]).

/*
 * get_failure_read_maybe_reset_eof(Action, Stream)
 *
 * get_failure_read_maybe_reset_eof/2 is used to reset the SIOF_EOF flag
 * for streams which specify eof_action(reset).  This is necessary to prevent
 * read/[1,2] and read_term/[2,3] from interpreting the SIOF_EOF and returning
 * an explicit end-of-file code or possibly a syntax error concerning an
 * unclosed comment.
 */

get_failure_read_maybe_reset_eof(reset, Stream) :-
	sio_reset_eof(Stream),
	!.
get_failure_read_maybe_reset_eof(_,_).


/*
 * get_failure_read_eof(EOFAction, Pred, Arity, Stream, Call)
 *
 * get_failure_read_eof/5 is called when get_failure_read/3 needs to
 * handle an end-of-file condition.
 */

get_failure_read_eof(reset, _, _, Stream, Call) 
	:-!,
	sio_reset_eof(Stream),
	call(Call).

get_failure_read_eof(eof_code, Pred, ArgNum, Stream, Call) 
	:-
	get_failure_read_eof_code(Pred, EOF_Code),
	!,
	arg(ArgNum, Call, EOF_Code).


get_failure_read_eof(error, Pred, _, Stream, Call) :-
	Pred \= skip_layout,
	Pred \= get_token_list,
	!,
	curmod(Mod),
	existence_error(past_end_of_stream, Stream, Mod:Call).

get_failure_read_eof(_, _, _, _, Call) :-
		%% This clause handles get_token_list/2 and skip_layout/1.
	call(Call).

get_failure_read_eof_code(get_char, end_of_file).
get_failure_read_eof_code(get_number, end_of_file).
get_failure_read_eof_code(get_code, -1).
get_failure_read_eof_code(peek_code, -1).
get_failure_read_eof_code(get_line0, -1).


/*
 * get_failure_read_snr(SNRAction, Pred, Arity, Stream, Call)
 *
 * get_failure_read_snr/5 is called when get_failure_read/3 needs to
 * handle a stream-not-ready condition.
 */

%% Debugging clause:    
/*
get_failure_read_snr(SNRAction, Pred, Arity, Stream, Call)
	:-
	pbi_write(gfrs(SNRAction, Pred, Arity, stream, call)),pbi_nl,pbi_ttyflush,
	fail.
*/

get_failure_read_snr(snr_code, Pred, ArgNum, Stream, Call) 
	:-
	stream_blocking(Stream, BLOCK),
	stream_blocking(Stream, true),
	stream_type(Stream, StrmType),
	dmember(StrmType, [socket,tk_win,window]),
	!,
	wait_data(StrmType, Stream, Call).

get_failure_read_snr(wait, Pred, ArgNum, Stream, Call) :-
	sio_poll(Stream, 10000000),	% 10 sec
	!,
	get_failure_read(Stream,Call).
get_failure_read_snr(wait, Pred, ArgNum, Stream, Call) :-
	!,
	get_failure_read(Stream,Call).
get_failure_read_snr(_, get_token_list, ArgNum, Stream, Call) :-
	!,
	arg(ArgNum, Call, stream_not_ready).
get_failure_read_snr(snr_code, skip_layout, _, Stream, Call) :-
	!,
	fail.
get_failure_read_snr(_, skip_layout, _, Stream, Call) :-
	!,
	curmod(Mod),
	existence_error(stream_not_ready,Stream,Mod:Call).
get_failure_read_snr(snr_code, Pred, ArgNum, Stream, Call) :-
	get_failure_read_snr_code(Pred,SNR_Code),
	!,
	arg(ArgNum,Call,SNR_Code).
get_failure_read_snr(error, Pred, ArgNum, Stream, Call) :-
	!,
	curmod(Mod),
	existence_error(stream_not_ready,Stream,Mod:Call).
	

get_failure_read_snr_code(get_char, stream_not_ready).
get_failure_read_snr_code(get_number, stream_not_ready).
get_failure_read_snr_code(_, -2).


/*
 * get_code(Char)
 *
 *	Unifies Char with the next character obtained from the default
 *	input stream.
 */

export get_code/1.

get_code(Char) :-
	get_current_input_stream(Stream),
	sio_get_byte(Stream,Char),
	!.
get_code(Char) :-
	get_current_input_stream(Stream),
	sio_errcode(Stream, FailCode),
	get_failure(FailCode,Stream,get_code(Char)).

/*
 * get_code(Stream_or_alias, Char)
 *
 *	Unifies Char with the next character obtained from the stream
 *	associated with Stream_or_alias.
 */

export get_code/2.

get_code(Stream, Code) :-
	sio_get_byte(Stream,Code),
	!.
get_code(Alias, Code) :-
	is_input_alias(Alias,Stream),
	sio_get_byte(Stream,Code),
	!.
get_code(Stream_or_alias, Code) :-
	input_stream_or_alias_ok(Stream_or_alias, Stream),
	sio_errcode(Stream, FailCode),
	get_failure(FailCode,Stream,get_code(Stream_or_alias,Code)).

export get_code/3.

get_code(Stream_or_alias, Code, Options) :-
	dmember(blocking(NewBlocking), Options),
	is_input_alias(Stream_or_alias,Stream),
	!,
	stream_blocking(Stream,OldBlocking),
	blocking_switch(NewBlocking, OldBlocking, Stream, true, ResetGoals),
	get_code(Stream, Code),
	call(ResetGoals).

get_code(Stream_or_alias, Code, Options) :-
	get_code(Stream_or_alias, Code).

/*
 * get_char(Char)
 *
 *	Unifies Char with the next character obtained from the default
 *	input stream.
 */

export get_char/1.

get_char(Char) :-
	get_current_input_stream(Stream),
	sio_get_byte(Stream,Code),
	!,
	char_code(Char,Code).
get_char(Char) :-
	get_current_input_stream(Stream),
	sio_errcode(Stream, FailCode),
	get_failure(FailCode,Stream,get_char(Char)).


/*
 * get_char(Stream_or_alias, Char)
 *
 *	Unifies Char with the next character obtained from the stream
 *	associated with Stream_or_alias.
 */

export get_char/2.

get_char(Stream, Char) :-
	sio_get_byte(Stream,Code),
	!,
	char_code(Char,Code).
get_char(Alias, Char) :-
	is_input_alias(Alias,Stream),
	sio_get_byte(Stream,Code),
	!,
	char_code(Char,Code).
get_char(Stream_or_alias, Char) :-
	input_stream_or_alias_ok(Stream_or_alias, Stream),
	sio_errcode(Stream, FailCode),
	get_failure(FailCode,Stream,get_char(Stream_or_alias,Char)).

/*
 * peek_code(Code)
 *
 *	Unifies Code with the next character code obtained from the default
 *	input stream.  The character (code) is not consumed.
 */

export peek_code/1.

peek_code(InCode) :-
	get_current_input_stream(Stream),
	sio_get_byte(Stream, Byte),
	sio_unget_byte(Stream),
	!,
	InCode = Byte.
peek_code(Code) :-
	get_current_input_stream(Stream),
	sio_errcode(Stream, FailCode),
	get_failure(FailCode,Stream,peek_code(Code)).


/*
 * peek_code(Stream_or_alias, Code)
 *
 *	Unifies Code with the next character code obtained from the stream
 *	associated with Stream_or_alias.  The character (code) is not consumed.
 */

export peek_code/2.

peek_code(Stream, InCode) :-
	sio_get_byte(Stream,Byte),
	sio_unget_byte(Stream),
	!,
	InCode = Byte.
peek_code(Alias, InCode) :-
	is_input_alias(Alias, Stream),
	sio_get_byte(Stream,Byte),
	sio_unget_byte(Stream),
	!,
	InCode = Byte.
peek_code(Stream_or_alias, Code) :-
	input_stream_or_alias_ok(Stream_or_alias, Stream),
	sio_errcode(Stream, FailCode),
	get_failure(FailCode,Stream,peek_code(Stream_or_alias,Code)).

/*
 * peek_char(Char)
 *
 *	Unifies Char with the next character obtained from the default
 *	input stream.  The character is not consumed.
 */

export peek_char/1.

peek_char(InChar) :-
	get_current_input_stream(Stream),
	sio_get_byte(Stream,Byte),
	sio_unget_byte(Stream),
	!,
	char_code(InChar,Byte).
peek_char(Char) :-
	get_current_input_stream(Stream),
	sio_errcode(Stream, FailCode),
	get_failure(FailCode,Stream,peek_char(Char)).

/*
 * peek_char(Stream_or_alias, Char)
 *
 *	Unifies Char with the next character obtained from the stream
 *	associated with Stream_or_alias.  The character is not consumed.
 */

export peek_char/2.

peek_char(Stream, InChar) :-
	sio_get_byte(Stream,Byte),
	sio_unget_byte(Stream),
	!,
	char_code(InChar,Byte).
peek_char(Alias, InChar) :-
	is_input_alias(Alias, Stream),
	sio_get_byte(Stream,Byte),
	sio_unget_byte(Stream),
	!,
	char_code(InChar,Byte).
peek_char(Stream_or_alias, Char) :-
	input_stream_or_alias_ok(Stream_or_alias, Stream),
	sio_errcode(Stream, FailCode),
	get_failure(FailCode,Stream,peek_char(Stream_or_alias,Char)).

/*
 * get_atomic_nonblank_char(Char)
 *
 *	Unifies Char with the atomic form of the next non-whitespace character 
 *  obtained from the default input stream, if that occurs before the next 
 *	end of line, and unifies Char with the atom
 *				end_of_line
 *	otherwise.
 */

export get_atomic_nonblank_char/1.

get_atomic_nonblank_char(Char) :-
	get_current_input_stream(Stream),
	get_atomic_nonblank_char(Stream,Char).

/*
 * get_atomic_nonblank_char(Stream_or_alias, Char)
 *
 *	Unifies Char with the atomic form of the next non-whitespace character 
 *	obtained from the input stream associated with Stream_or_alias, if that 
 *	occurs before the next end of line, and unifies Char with the atom
 *				end_of_line
 *	otherwise.
 */

export get_atomic_nonblank_char/2.

/*
get_atomic_nonblank_char(Stream,Char)
	:-
	get_nonblank_char(Stream,Char0),
	(Char0 = end_of_line ->
		Char0 = Char
		;
		name(Char, [Char0])
	).
*/

get_atomic_nonblank_char(Stream,Char)
	:-
	get_line(Stream, Line),
	atom_codes(Line, CharList),
	first_nonblank_char(CharList, Char0),
	atom_codes(Char, [Char0]).
get_atomic_nonblank_char(Stream, end_of_line).

first_nonblank_char([Char | Rest], Char) :-
	Char > 32.
first_nonblank_char([_ | Rest], Char) :-
	first_nonblank_char(Rest, Char).



/*------------------------------------------------------------*
 |  read_buffer/2
 |	read_buffer(Type,Stream)
 |	read_buffer(+,+)
 |
 |	There are special case clauses for the streams which
 |	are handled primarily from Prolog (e.g., atom or string streams);
 |	the rest default to sio_readbuffer
 *------------------------------------------------------------*/

read_buffer(atom,Stream) 
	:-!,
	sio_bufshift(Stream),
	read_atom(Stream).
	
read_buffer(string,Stream) 
	:-!,
	sio_bufshift(Stream),
	read_string(Stream).

read_buffer(null_stream, Stream) 
	:-!,
	sio_set_eof(Stream),
	fail.	

/*
read_buffer(console,Stream) :-
	!,
	get_user_prompt(Prompt),
	sio_editline(
	stream_pgoals(Stream,PromptGoal),
	call(PromptGoal),
	read_buffer(Stream).
	.
*/

/****
read_buffer(window,Stream) 
	:-
	stream_extra(Stream,Tail),
	Tail == [],
	!,
	stream_pgoals(Stream,PromptGoal),
	call(PromptGoal),
	sio_set_errcode(Stream,14),		%% 14 =  SIOE_NOTREADY
	fail.
	
read_buffer(window,Stream) 
	:- !,
		%% get the queue of raw lines:
	stream_extra(Stream,CurQueue),
	sio_buf_params(Stream, BufStart, BufSize),
	stream_buffer(Stream,SD),
	move_lines_to_buffer(CurQueue,BufSize,BufStart,SD,0,NumCs,NewQueue),
	set_stream_extra(Stream,NewQueue),
	sio_set_position(Stream, 0, NumCs),
	(NewQueue \= [] ->
		true;
		set_stream_addl1(Stream, [])
	).
****/

read_buffer(tk_win,Stream) 
	:- 
	stream_extra(Stream,eof(L)),
	!,
	sio_set_errcode(Stream,8),		%% 8 =  SIOE_EOF
%	sio_set_eof(Stream),		
	set_stream_extra(Stream,L),
	fail.

read_buffer(tk_win,Stream) 
	:- 
	stream_extra(Stream,''),
	!,
	stream_pgoals(Stream,PromptGoal),
	call(PromptGoal),
	sio_set_errcode(Stream,14),		%% 14 =  SIOE_NOTREADY
%	wait_data(tcltk, Stream, Call),
	fail.

read_buffer(tk_win,Stream) 
	:- 
	stream_extra(Stream,InCurQueue),
	(InCurQueue = eof(CurQueue) ->
		EOFFlag = true
		;
		EOFFlag = false,
		CurQueue = InCurQueue
	),
	sio_buf_params(Stream, BufStart, BufSize),
	sio_cpos(Stream,  CPos),
	sio_lpos(Stream,  LPos),
	BeginPoint is BufStart + CPos,
	stream_buffer(Stream,SD),
	atom_length(CurQueue, CQL),
	move_queue_seg_to_buffer(CurQueue,CQL,BufSize,BeginPoint,SD,NumCs,NewQueue),
	NLPos is LPos + NumCs,
	NCPos is CPos,
	sio_set_position(Stream, NCPos, NLPos),
	(EOFFlag = true ->
		OutNewQueue = eof(NewQueue)
		;
		OutNewQueue = NewQueue
	),
	!,
	set_stream_extra(Stream,OutNewQueue).

read_buffer(tk_win,Stream) :-
	sio_errcode(Stream,16),			%% 16 = SIOE_INTERRUPTED
	!,
	read_buffer(tk_win,Stream).

set_extra_eof(StreamOrAlias)
	:-
	stream_or_alias_ok(StreamOrAlias, Stream),
	stream_token_list(Stream,TL),
	stream_extra(Stream,CurQueue),
	set_stream_extra(Stream,eof(CurQueue)),
	append(TL, [end_of_file(StreamOrAlias,0,0)],NTL),
	set_stream_token_list(Stream,NTL),
	sio_set_eof(Stream).

%%
%% This is the place to add read_buffer definitions for other stream types
%%

read_buffer(_,Stream) 
	:-!,
	stream_pgoals(Stream,PromptGoal),
	call(PromptGoal),
	read_buffer(Stream).

%%
%% read_buffer/1 is called by read_buffer/2 to really read the buffer
%% (via sio_readbuffer).   Note that this is where we restart interrupted
%% system calls.
%%

read_buffer(Stream) :-
	sio_readbuffer(Stream),
	!.
read_buffer(Stream) :-
	sio_errcode(Stream,16),			%% 16 = SIOE_INTERRUPTED
	!,
	read_buffer(Stream).

export push_prompt/1.
push_prompt(Stream_or_alias)
	:-
	is_stream(Stream_or_alias,Stream),
	stream_pgoals(Stream,PromptGoal),
	call(PromptGoal).

/*----------------------------------------------------------*
 |	add_to_stream_buffer/2
 |	add_to_stream_buffer(StreamOrAlias, InputLine)
 |	add_to_stream_buffer(+, +)
 |	
 |	called by the code implementing read_buffer/2 for
 |	tk_win streams to add the incoming line (which
 |	is part of a term) to the queue being accumulated
 |	in the "extra" slot of the stream.
 *----------------------------------------------------------*/

export add_to_stream_buffer/2.
add_to_stream_buffer(StreamOrAlias, InputLine)
	:-
	is_stream(StreamOrAlias, Stream),
	stream_extra(Stream, CurBuffer),
	'$atom_concat'(CurBuffer, InputLine, NewBuffer),
	set_stream_extra(Stream, NewBuffer).

/*----------------------------------------------------------*
 |	read_atom/1 
 |	
 |	called by read_buffer to fill the buffer associated 
 |	with an atom stream.
 *----------------------------------------------------------*/

read_atom(Stream) :-
	stream_addl1(Stream,APos),
	stream_addl2(Stream,ALen),
	APos < ALen,
	!,
	sio_buf_params(Stream,BufStart,BufSize),
	sio_lpos(Stream,LPos),
	stream_buffer(Stream,Buf),
	ADiff is ALen - APos,
	BDiff is BufSize - LPos,
	( ADiff =< BDiff
	    ->	NumCs = ADiff
	    ;	NumCs = BDiff
	),
	stream_extra(Stream,Atom),
	PokeStart is BufStart + LPos,
	'$uia_poke'(Buf,PokeStart,NumCs,Atom,APos),
	NewAPos is APos+NumCs,
	set_stream_addl1(Stream,NewAPos),
	NewLPos is LPos+NumCs,
	sio_cpos(Stream,CPos),
	sio_set_position(Stream,CPos,NewLPos).
read_atom(Stream) :-
	sio_set_eof(Stream),
	fail.

/*
 * read_string/1 is called from read_buffer to deal
 * with filling the stream buffer for string streams.
 */

read_string(Stream) :-
	stream_extra(Stream,CurTail),
	CurTail \== [],
	!,
	sio_buf_params(Stream, BufStart, BufSize),
	stream_buffer(Stream,SD),
	sio_lpos(Stream,LPos),
	TStart is BufStart + LPos,
	TLimit is BufSize - LPos,
	transfer_string(CurTail,TLimit,TStart,SD,0,NumCs,NewTail),
	set_stream_extra(Stream,NewTail),
	NewLPos is LPos+NumCs,
	sio_cpos(Stream,CPos),
	sio_set_position(Stream, CPos, NewLPos).
read_string(Stream) :-
	sio_set_eof(Stream),
	fail.

/*
 * transfer_string(Tail,Limit,CurPos,SD,CurNum,NumCs,NewTail)
 *
 * Does the actual filling of the buffer in stream descriptor SD
 * from the source Prolog string Tail.  Limit is the maximum number
 * of characters which may be transfered before the buffer is filled.
 * CurPos is the current position in that buffer as an offset from
 * the start of the uia SD; CurNum is the number of chars transferred
 * so far;  NumCs will return the total number of chars transferred,
 * and NewTail will return the remainder of Tail which isn't transferred.
 *
 */

transfer_string([], _, _, _, NumCs, NumCs, []).
transfer_string([C|RestTail],Limit,CurPos,SD,CurNum,NumCs,NewTail) :-
	CurNum < Limit,
	!,
	'$uia_pokeb'(SD, CurPos, C),
	NextPos is CurPos + 1,
	NextNum is CurNum + 1,
	transfer_string(RestTail,Limit,NextPos,SD,NextNum,NumCs,NewTail).
transfer_string(CurTail, _, _, _, NumCs, NumCs, CurTail).


/*************************UNUSED***********************************
/*
 * read_window/1 is called from read_buffer to fill
 * a portion of the stream buffer for window streams.
 *
 * TESTME!
 */

read_window(Stream) :-
		%% get the queue of raw lines:
	stream_extra(Stream,CurQueue),
	CurQueue \== [],
	!,
	sio_buf_params(Stream, BufStart, BufSize),
	stream_buffer(Stream, SD),
	sio_lpos(Stream, LPos),
	TStart is BufStart + LPos,
	TLimit is BufSize  - LPos,
	move_lines_to_buffer(CurQueue,TLimit,TStart,SD,0,NumCs,NewQueue),
	set_stream_extra(Stream,NewQueue),
	NewLPos is LPos + NumCs,
	sio_cpos(Stream, CPos),
	sio_set_position(Stream, CPos, NewLPos),
	(NewQueue \= [] ->
		true;
		set_stream_addl1(Stream, [])
	).
read_window(Stream) :-
	stream_pgoals(Stream,PromptGoal),
	call(PromptGoal),
	sio_set_errcode(Stream,14),			%% 14 =  SIOE_WAITDATA
	fail.
*************************UNUSED**********************************/


/*-------------------------------------------------------------------------
 |	move_lines_to_buffer/7
 |	move_lines_to_buffer(CurQueue,BufSize,BufStart,SD,CurNum,NumCs,NewQueue)
 |	move_lines_to_buffer(+,+,+,+,+,-,-)
 |	
 |	Fills (as much as possible) of the buffer in stream descriptor SD
 |	from the source CurQueue;  
 |
 |	CurQueue -- the initial raw lines queue (see below for details);
 |	BufSize	 -- the size of SD's character buffer;
 |	BufStart -- the position in SD at which the buffer starts;
 |	SD		 -- the stream descriptor (structured uia) of the stream;
 |	CurNum	 -- the number of chars which have already been written in;
 |	NumCs	 -- the final number (total) of chars written into the buffer;
 |	NewQueue -- the resulting raw line queue;  this is a tail of CurQueue;
 |				in addition, the second argument of the first pair on
 |				NewQueue may have been mangled from its original value.
 |
 |	Both CurQueue and NewQueue are lists of pairs of the form (Ptr, Offset),
 |	where Ptr is a C pointer to a C string (in C space), and Offset is an
 |	integer representing an offset in the C string.  When a pair is initially
 |	put on the queue, this Offset is 0.  If this predicate only partially
 |	consumes the string pointed at by Ptr (it will be the last one handled),
 |	Offset is mangled to N, where N is the Offset of the first unconsumed
 |	char in the C string;  move_lines_to_buffer/7 will start at this point
 |	the next time read_buffer/2 is called.
 *------------------------------------------------------------------------*/


move_lines_to_buffer([],_,_,_,CurNum,CurNum,[]).
move_lines_to_buffer(CurQueue,BufSize,BufStart,SD,CurNum,NumCs,NewQueue) :-
	CurNum < BufSize,
	!,
	CurQueue = [(Ptr,Offset,Length) | RestCurQueue], 
	BufferOffset is BufStart+CurNum,
		%% temporary hack:
	TextUIA = Ptr,
	name(TextUIA, TextString),
	transfer_string(TextString,BufSize,BufStart,SD,0,NumCopied,StringTail),

	NumCs is CurNum + NumCopied,
	NewQueue = RestCurQueue.
move_lines_to_buffer(CurQueue,_,_,_,CurNum,CurNum,CurQueue).


	%% move_queue_seg_to_buffer(CurQueue,CQL,BufSize,BeginPoint,SD,NumCs,NewQueue),

move_queue_seg_to_buffer('',0,_,_,_,_,_,_)
	:-!,
	fail.

move_queue_seg_to_buffer(CurQueue,CQL,BufSize,Offset,SD,NumCs,NewQueue)
	:-
	copy_into_uia(0,CQL,CurQueue,Offset,BufSize,SD,NumCs),
	(NumCs < CQL ->
		Start is NumCs + 1,
		sub_atom(CurQueue, Start, CQL, _, NewQueue)
		;
		NewQueue = ''
	).

copy_into_uia(CurPos,NumToCopy,SrcUIA,BufPos,BufSize,Buf,NumCs)
	:-
	CurPos =< NumToCopy,
	BufPos < BufSize,
	!,
	'$uia_peekb'(SrcUIA, CurPos, BB),
	'$uia_pokeb'(Buf, BufPos, BB),
	NextPos is CurPos + 1,
	NextBufPos is BufPos + 1,
	copy_into_uia(NextPos,NumToCopy,SrcUIA,NextBufPos,BufSize,Buf,NumCs).

copy_into_uia(CurPos,_,_,_,_,_,NumCs)
	:-
	NumCs is CurPos - 1.
	
	

/*
 * put_failure/4
 *
 * Handles various failure conditions for the output streams.
 */

put_failure(3,Stream,Arg,Call) :-	%% SIOE_WRITE
	stream_type(Stream,Type),
	write_buffer(Type,Stream),
	!,
	sio_aux(Stream,Aux),
	put_failure_write(Aux,Call).

put_failure(5,Stream,Arg,Call) :-	%% SIOE_READ
	stream_type(Stream,Type),
	read_buffer(Type,Stream),
	!,
	call(Call).
put_failure(5,Stream,Arg,Call) :-	%% SIOE_READ and read_buffer failure
	sio_errcode(Stream,8),		%% SIOE_EOF
	!,
	call(Call).

put_failure(0,Stream,Arg,Call) :-	%% SIOE_INARG for put_string
	curmod(Mod),
	functor(Call,_,LastArg),
	arg(LastArg,Call,Culprit),
	!,
	type_error(list, Culprit, [Mod:Call]).
put_failure(0,_,_,Call) :-		%% SIOE_NORMAL (should not happen)
	!,
	fail.

put_failure(2,Stream,Arg,Call) :-	%% SIOE_INARG
	var(Arg),
	!,
	curmod(Mod),
	instantiation_error(Mod:Call).
put_failure(2,Stream,Arg,Call) :-	%% SIOE_INARG
	!,
	curmod(Mod),
	functor(Call,_,LastArg),
	arg(LastArg,Call,Culprit),
	put_failure_inarg(Call, Arg, Culprit, Mod, Call).

put_failure(_,Stream,Arg,Call) :-	%% catchall
	!,
	curmod(Mod),
	sio_errno(Stream,ErrNo),
	system_error([Mod:Call,errno(ErrNo)]).

put_failure_write(0,_) :- !.
put_failure_write(_,Call) :- call(Call).


put_failure_inarg(put_atom(_, Arg), Arg, Culprit, Mod, Call) :- !,
		type_error(atom, Culprit, [Mod:Call]).
put_failure_inarg(put_code(_, Arg), Arg, Culprit, Mod, Call) :- !,
		type_error(code, Culprit, [Mod:Call]).
put_failure_inarg(put_char(_, Arg), Arg, Culprit, Mod, Call) :- !,
		type_error(character, Culprit, [Mod:Call]).
put_failure_inarg(put_number(_, KindNum, Arg), Arg, Culprit, Mod, Call) :- !,
		type_error(KindNum, Culprit, [Mod:Call]).

/*
 * put_char(Char)
 *
 *	Outputs the character Char to the current default output stream.
 *
 * FIXME: If char_code generates an error, the error goal is char_code/2
 *	not put_char (as it should be).
 */

export put_char/1.

/*****
put_char(Char) :-
	get_current_output_stream(Stream),
	char_code(Char,Code),
	sio_put_byte(Stream,Code),
	!.
put_char(Char) :-
	get_current_output_stream(Stream),
	sio_errcode(Stream,FailCode),
	put_failure(FailCode,Stream,Char,put_char(Char)).
*****/
put_char(Char) :-
	get_current_output_stream(Stream),
	put_char(Stream, Char).

/*
 * put_char(Stream_or_alias,Char)
 *
 *	Outputs the character Char to the stream defined by Stream_or_alias.
 *
 */

export put_char/2.


put_char(Stream, Char) :-
	char_code(Char,Code),
	sio_put_byte(Stream,Code),
	!.
%	tk_setmark(Stream).
put_char(Alias, Char) :-
	is_output_alias(Alias,Stream),
	char_code(Char,Code),
	sio_put_byte(Stream,Code),
	!.
%	tk_setmark(Stream).
put_char(Stream_or_alias,Char) :-
	output_stream_or_alias_ok(Stream_or_alias,Stream),
	sio_errcode(Stream,FailCode),
	put_failure(FailCode,Stream,Char,put_char(Stream_or_alias,Char)).

/*
 * put_code(Char)
 *
 *	Outputs the character code Code to the current default output stream.
 */

export put_code/1.

/***********
put_code(Char) :-
	get_current_output_stream(Stream),
	sio_put_byte(Stream,Char),
	!.
put_code(Char) :-
	get_current_output_stream(Stream),
	sio_errcode(Stream,FailCode),
	put_failure(FailCode,Stream,Char,put_code(Char)).
***********/
put_code(Char) :-
	get_current_output_stream(Stream),
	put_code(Stream, Char).


/*
 * put_code(Stream_or_alias,Char)
 *
 *	Outputs the character code Code to the stream defined by
 *	Stream_or_alias.
 *
 */

export put_code/2.

put_code(Stream, Char) 
	:-
	sio_put_byte(Stream,Char),
	!.
%	tk_setmark(Stream).
put_code(Alias, Char) 
	:-
	is_output_alias(Alias, Stream),
	sio_put_byte(Stream,Char),
	!.
%	tk_setmark(Stream).
put_code(Stream_or_alias,Code) 
	:-
	output_stream_or_alias_ok(Stream_or_alias,Stream),
	sio_errcode(Stream,FailCode),
	put_failure(FailCode,Stream,Code,put_code(Stream_or_alias,Code)).


/*
 *	put_string/[1,2]
 *
 *  Recursively calls put_code on its string argument.
 */

export put_string/1.

put_string(String) :-
	get_current_output_stream(Stream),
	put_string(Stream, String).

export put_string/2.

put_string(Stream_or_alias, String) :-
	var(String),
	!,
	curmod(Mod),
	instantiation_error(Mod:Call).
put_string(Stream_or_alias, String) :-
	is_stream(Stream_or_alias,Stream),
	is_output_stream(Stream),
	put_string0(String, Stream),
	!.
put_string(Stream_or_alias, String) :-
	output_stream_or_alias_ok(Stream_or_alias,Stream),
	sio_errcode(Stream,FailCode),
	put_failure(FailCode,Stream,String,put_string(Stream_or_alias,String)).

put_string0([], _).
put_string0([C | String], Stream) :-
	put_code(Stream,C),
	put_string0(String, Stream).


/*
 * put_atom(Atom)
 *
 * Outputs the atom to the current output stream
 */

export put_atom/1.

put_atom(Atom) :-
	get_current_output_stream(Stream),
	put_atom(Stream,Atom).

/*
 * put_atom(Stream_or_alias,Atom)
 *
 *	Outputs the atom Atom to the stream defined by Stream_or_alias.
 */

export put_atom/2.

put_atom(Stream, Atom) :-
	sio_put_atom(Stream,Atom),
	!.
%	tk_setmark(Stream).
put_atom(Alias, Atom) :-
	is_output_alias(Alias, Stream),
	sio_put_atom(Stream,Atom),
	!.
%	tk_setmark(Stream).
put_atom(Stream_or_alias,Atom) :-
	output_stream_or_alias_ok(Stream_or_alias,Stream),
	sio_errcode(Stream,FailCode),
	put_failure(FailCode,Stream,Atom,put_atom(Stream_or_alias,Atom)).

/*
 * put_number(Stream_or_alias,OutputType,Number)
 *
 *	Outputs the number Number as OutputType to the stream defined by
 *	Stream_or_alias.
 *
 *	OutputType may take on the following values:
 *		byte
 *		ubyte
 *		short
 *		ushort
 *		long
 *		ulong
 *		float
 *		double
 */

num_output_type(byte) :- !.
num_output_type(ubyte) :- !.
num_output_type(char) :- !.
num_output_type(uchar) :- !.
num_output_type(short) :- !.
num_output_type(ushort) :- !.
num_output_type(int) :- !.
num_output_type(uint) :- !.
num_output_type(long) :- !.
num_output_type(ulong) :- !.
num_output_type(float) :- !.
num_output_type(double) :- !.

export put_number/3.

put_number(Stream,OutputType,Number) :-
	var(OutputType),
	!,
	instantiation_error(2).
put_number(Stream,OutputType,Number) :-
	num_output_type(OutputType),
	!,
	put_number0(Stream,OutputType,Number).
put_number(Stream,OutputType,Number) :-
	domain_error(num_output_type,OutputType,2).


put_number0(Stream,OutputType,Number) :-
	sio_put_number(Stream,OutputType,Number),
	!.
%	tk_setmark(Stream).
put_number0(Alias, OutputType,Number) :-
	is_output_alias(Alias, Stream),
	sio_put_number(Stream,OutputType,Number),
	!.
%	tk_setmark(Stream).
put_number0(Stream_or_alias,OutputType,Number) :-
	output_stream_or_alias_ok(Stream_or_alias,Stream),
	sio_errcode(Stream,FailCode),
	put_failure(FailCode,Stream,Number,
		put_number(Stream_or_alias,OutputType,Number)).


/*
 * write_buffer/2
 */

/*****
write_buffer(window,Stream) :-
	sio_buf_params(Stream, BufStart, BufSize),
	stream_buffer(Stream,SD),
	sio_lpos(Stream, NumCs),
	NumCs > 0,
	!,
	sio_fd(Stream, WinID),
	'$uia_peeks'(SD,BufStart,NumCs,BufUIA),
	stream_mode(Stream, [_|OutType]),
	write_buffer_to_win(OutType,BufUIA,NumCs,WinID,EndPos),
	sio_set_position(Stream, 0, 0),
	stream_addl3(Stream, ReadStreamAlias),
	endpos_2_readstream(ReadStreamAlias,EndPos).

write_buffer(window,Stream) :-!.
*****/

endpos_2_readstream(0,_) :-!.
endpos_2_readstream(ReadStreamAlias,EndPos)
	:-
	current_alias(ReadStreamAlias, ReadStream),
	set_window_insert_pos(ReadStream,EndPos).

write_buffer(tk_win,Stream) :-
	sio_buf_params(Stream, BufStart, BufSize),
	stream_buffer(Stream,SD),
	sio_lpos(Stream, NumCs),
	NumCs > 0,
	!,
	'$uia_peeks'(SD,BufStart,NumCs,BufUIA),
	stream_mode(Stream, [_|OutType]),
	stream_addl2(Stream, ti(Interp,WaitVarName)),
	stream_name(Stream, WinID),
	sio_set_position(Stream, 0, 0),

	tcl_call(Interp, [WinID,insert,end,BufUIA], _),
	tcl_call(Interp, [WinID,mark,set,lastPrompt,[end,-2,chars]], _),
	tcl_call(Interp, [WinID,mark,set,insert,end], _),
	tcl_call(Interp, [WinID,see,end], _),
	tcl_call(Interp, [winfo,parent,WinID], Parent),
	tcl_call(Interp, [raise,Parent], _),
	tcl_call(Interp, [focus,WinID], _),
	tcl_call(Interp, [update], _).
/*
	tcl_call(Interp, [high_tide_flotsam,WinID,BufUIA], _).
*/

write_buffer(tk_win,Stream) :-!.

write_buffer(tcl_transfer,Stream) 
	:-!,
	write_buffer_tcl_tr(Stream).

write_buffer_tcl_tr(Stream)
	:-
	sio_buf_params(Stream, BufStart, BufSize),
	stream_buffer(Stream,SD),
	sio_lpos(Stream, NumCs),
	NumCs > 0,
	!,
	'$uia_peeks'(SD,BufStart,NumCs,BufUIA),
	stream_addl2(Stream, Interp),
	stream_addl3(Stream, CmdPattern),
	sprintf(atom(Cmd), CmdPattern, [BufUIA]),
	sio_set_position(Stream, 0, 0),
	tcl_eval(Interp, Cmd, _).

write_buffer_tcl_tr(Stream).





write_buffer(atom,Stream) 
	:-
	sio_buf_params(Stream, BufStart, BufSize),
	stream_buffer(Stream,SD),
	sio_lpos(Stream, NumCs),
	NumCs > 0,
	!,
	'$uia_peeks'(SD,BufStart,NumCs,BufUIA),
	stream_extra(Stream,Initial),
	'$atom_concat'(Initial,BufUIA,NewAtom),
	set_stream_extra(Stream,NewAtom),
	sio_increment_bufpos(Stream),
	sio_set_position(Stream, 0, 0).

write_buffer(atom,Stream) 
	:-!.

write_buffer(string,Stream) 
	:-
	sio_lpos(Stream, NumCs),
	NumCs > 0,
	!,
	sio_buf_params(Stream, BufStart, _),
	stream_buffer(Stream,SD),
	uia_to_list(NumCs,SD,BufStart,List),
	stream_extra(Stream,WholeString),
	stream_addl1(Stream,LastCell),
	last_cell(List,NewLastCell),
	(WholeString = [] ->  
		set_stream_extra(Stream,List)
		;   
		mangle(2,LastCell,List)
	),
	set_stream_addl1(Stream,NewLastCell),
	sio_increment_bufpos(Stream),
	sio_set_position(Stream,0,0).

write_buffer(string,_) 
	:-!.

write_buffer(null_stream,Stream) 
	:-
	sio_set_position(Stream, 0, 0).


%%
%% This is the place to put write_buffer definitions for other types of
%% streams.
%%

write_buffer(_,Stream) :-
	!,
	write_buffer(Stream).

write_buffer(Stream) :-
	sio_writebuffer(Stream),
	!.
write_buffer(Stream) :-
	sio_errcode(Stream,16),			%% 16 = SIOE_INTERRUPTED
	!,
	write_buffer(Stream).

/*
tk_setmark(StreamOrAlias)
	:-
	stream_or_alias_ok(StreamOrAlias, Stream),
	stream_type(Stream, StrmType),
	StrmType=tk_win,
	stream_name(Stream, WinID),
%	stream_addl2(Stream, Interp),
	stream_addl2(Stream, ti(Interp,WaitVarName)),
	!,
/*
	catch(tcl_call(Interp, [WinID,mark,set,lastPrompt,[end,-2,chars]], _),_,true),
	catch(tcl_call(Interp, [WinID,mark,set,insert,end], _),_,true),
	catch(tcl_call(Interp, [focus,WinID], _),_,true).
*/
	tcl_call(Interp, [WinID,mark,set,lastPrompt,[end,-2,chars]], _),
	tcl_call(Interp, [WinID,mark,set,insert,end], _),
	tcl_call(Interp, [focus,WinID], _).

tk_setmark(Stream).
*/

%%
%% uia_to_list(NumChars,UIA,Offset,List)
%%
%%	Moves NumChars starting at Offset from UIA into the list List.
%%
%%

uia_to_list(0,_,_,[]) :-
	!.
uia_to_list(N,UIA,Offset,[C | T]) :-
	'$uia_peekb'(UIA,Offset,C),
	Offset1 is Offset+1,
	N1 is N-1,
	uia_to_list(N1,UIA,Offset1,T).


%%
%% last_cell(List,LastCell)
%%
%%	Unifies LastCell with the last cons cell in the list List.
%%

last_cell(X,X) :- 
	X=[_],
	!.
last_cell([_|T],Last) :-
	last_cell(T,Last).


/*
 * flush_input/1
 *
 *	Discards data currently in buffer for given input stream.
 */

export flush_input/1.
flush_input(Stream_or_alias) :-
	is_stream(Stream_or_alias,Stream),
	is_input_stream(Stream),
	!,
	sio_lpos(Stream, NumCs),
	sio_set_position(Stream, NumCs, NumCs),
	check_special_in_flush(Stream),
	sio_reset_eof(Stream).

check_special_in_flush(Stream)
	:-
	stream_type(Stream, tk_win),
	!,
	stream_name(Stream,WinName),
%	stream_addl2(Stream, Interp),
	stream_addl2(Stream, ti(Interp,WaitVarName)),
	tcl_call(Interp, [WinName,mark,set,lastPrompt,[end,-1,chars]], _).


check_special_in_flush(Stream).


/*
 * flush_output/0
 *
 *	Sends any output which is currently buffered by the processor for
 *	the default output stream to be sent to that stream.
 */

export flush_output/0.

flush_output :-
	get_current_output_stream(Stream),
	flush_output0(Stream).


/*
 * flush_ouptut(Stream_or_alias)
 *
 *	Sends any output which is currently buffered by the processor for
 *	the stream associated with Stream_or_alias to be sent to that stream.
 */

export flush_output/1.

flush_output(Stream_or_alias) :-
	output_stream_or_alias_ok(Stream_or_alias, Stream),
	!,
	flush_output0(Stream).

flush_output0(Stream) :-
	stream_type(Stream,Type),
	write_buffer(Type,Stream),
	!.
flush_output0(Stream) :-
	curmod(Mod),
	sio_errno(Stream,ErrNo),
	system_error([Mod:flush_output0(Stream),errno(ErrNo)]).

/*
 * at_end_of_stream
 *
 *	Succeeds if the default input stream is positioned at the end.
 */

export at_end_of_stream/0.

at_end_of_stream :-
	get_current_input_stream(Stream),
	at_end_of_stream(Stream).


/*
 * at_end_of_stream(Stream_or_alias)
 *
 *	Succeeds if the input stream associated with Stream_or_alias is
 *	positioned at the end.
 */

export at_end_of_stream/1.

at_end_of_stream(Stream_or_alias) :-
	input_stream_or_alias_ok(Stream_or_alias,Stream),
	peek_code(Stream,-1).


/*
 * at_end_of_line
 *
 *	Succeeds if the default input stream is positioned at the end
 *	of a line
 */

export at_end_of_line/0.

at_end_of_line :-
	get_current_input_stream(Stream),
	at_end_of_line(Stream).


/*
 * at_end_of_line(Stream_or_alias)
 *
 *	Succeeds if the input stream associated with Stream_or_alias is
 *	positioned at the end of a line.
 */

export at_end_of_line/1.

at_end_of_line(Stream_or_alias) :-
	input_stream_or_alias_ok(Stream_or_alias,Stream),
	sio_at_end_of_line(Stream).

/*
 * skip_line
 *
 *	Skips to next line of input for the default input stream.
 */

export skip_line/0.

skip_line :-
	get_current_input_stream(Stream),
	skip_line(Stream).

/*
 * skip_line(Stream_or_alias)
 *	Skips to the next line of input for the stream associated with
 *	Stream_or_alias.
 */

export skip_line/1.

skip_line(Stream) :-
	input_stream_or_alias_ok(Stream_or_alias,Stream),
	sio_skip_line(Stream).

/*
 * nl
 *
 *	Causes a newline to be output to the default output stream.
 */

export nl/0.
nl :-
	get_current_output_stream(Stream),
	nl(Stream).

/*
 * nl(Stream)
 *
 *	Causes a newline to be output to the stream associated with
 *	Stream.
 */

export nl/1.

nl(Stream_or_alias) :-
	output_stream_or_alias_ok(Stream_or_alias, Stream),
	nl0(Stream),
	flush_output(Stream).

nl0(Stream) 
	:-
	sio_nl(Stream),
	!.
%	tk_setmark(Stream).
nl0(Alias) 
	:-
	is_output_alias(Alias, Stream),
	sio_nl(Stream),
	!.
%	tk_setmark(Stream).
nl0(Stream_or_alias) 
	:-
	output_stream_or_alias_ok(Stream_or_alias,Stream),
	sio_errcode(Stream,FailCode),
	put_failure(FailCode,Stream,Code,nl0(Stream_or_alias)).

/*
 * set_stream_position(Stream_or_alias, Position)
 */

export set_stream_position/2.

set_stream_position(Stream_or_alias, Position) :-
	stream_or_alias_ok(Stream_or_alias, Stream),
	nonvar_ok(Position),
	stream_repositionability(Stream,true),
	positions_for_file_streams(Position,_,_),
	!,
	stream_type(Stream,Type),
	stream_position(Type,Stream,_,Position).
set_stream_position(Stream_or_alias, Position) :-
	stream_or_alias_ok(Stream_or_alias, Stream),
	stream_repositionability(Stream,false),
	!,
	permission_error(reposition, stream, Stream_or_alias,
		sio:set_stream_position(Stream_or_alias,Position)).
set_stream_position(Stream_or_alias, Position) :-
	domain_error(stream_position, Position,
		sio:set_stream_position(Stream_or_alias,Position)).

/*
 * stream_position/2 and stream_position/3 are the mechanisms from
 * an earlier version of the draft standard.
 */

/*-------------------------------------------------------------------*
 | stream_position(Stream_or_alias, Position)
 |
 |	Unifies Position with the current stream position of the stream
 |	denoted by Stream_or_alias.  Note that we call sio_getpos (so long
 |  as we have a valid stream or alias).  These means that we can get
 |  positions even for streams which aren't seekable.
 *-------------------------------------------------------------------*/

export stream_position/2.

stream_position(Stream_or_alias, Position) :-
    stream_or_alias_ok(Stream_or_alias,Stream),
%   var_or_integer_ok(Position),
    var_or_number_ok(Position),
    !,
    sio_getpos(Stream,Position).

/*-------------------------------------------------------------------*
 | stream_position(Stream_or_alias, Current_position, New_position)
 |
 |	Unifies Current_position with the current stream position of 
 |	the stream denoted by Stream_or_alias.  As a side effect, also 
 |	sets the stream position of said stream to the position 
 |	represented by New_position.   
 |	New_position may be one of the following values:
 |
 |	An absolute integer position into the stream.
 |	A whole number float position into the stream.
 |	The atom beginning_of_stream.
 |	The term beginning_of_stream(N), N >= 0.
 |	The atom end_of_stream.
 |	The term end_of_stream(N), N =< 0
 |	The atom current_position.
 |	The term current_position(N), N an integer or whole number float
 |
 |	These have the intuitive meanings.
 |
 |	Non-whole number floats F as positions will be truncated to floor(F).
 *-------------------------------------------------------------------*/

export stream_position/3.

stream_position(Stream_or_alias, Current_position, New_position) 
	:-
	stream_position(Stream_or_alias,Current_position),
	set_stream_position(Stream_or_alias, New_position).

positions_for_file_streams(beginning_of_stream,0,0).
positions_for_file_streams(end_of_stream,0,2).
positions_for_file_streams(current_position,0,1).
positions_for_file_streams(beginning_of_stream(Pos),Pos,0) :-
%	integer(Pos),
	number(Pos),
	Pos >= 0.
positions_for_file_streams(end_of_stream(Pos),Pos,2) :-
%	integer(Pos),
	number(Pos),
	Pos =< 0.
positions_for_file_streams(current_position(Pos),Pos,1) :-
%	integer(Pos).
	number(Pos).
positions_for_file_streams(Position,Position,0) :- 
%	integer(Position).
	number(Position).

%%
%% stream_position/4 should be extended for different stream types
%%

stream_position(file,Stream,Current_position,New_position) :-
	positions_for_file_streams(New_position, Pos, Whence),
	sio_seek(Stream,Current_position,Pos,Whence),
	!.
%%
%% Other stream types to be added here
%%

%% Error handling clauses
stream_position(_,Stream,Current_position,New_position) :-
	sio_errcode(Stream,ErrCode),
	!,
	stream_position_failure(ErrCode,Stream,Current_position,New_position).
stream_position(_,Stream,Current_position,New_position) :-
	curmod(Mod),
	domain_error(stream_or_alias, Stream,
			Mod:stream_position(Stream,
					    Current_position,
					    New_position)).

stream_position_failure(0,Stream,CurPos,NewPos) :-	%% SIOE_NORMAL
	!,
	fail.
stream_position_failure(1,Stream,CurPos,NewPos) :-	%% SIOE_SYSCALL
	!,
	curmod(Mod),
	sio_errno(Stream,ErrNo),
	system_error([Mod:stream_position(Stream,CurPos,NewPos),errno(ErrNo)]).
stream_position_failure(2,Stream,CurPos,NewPos) :-	%% SIOE_INARG
	!,
	curmod(Mod),
	domain_error(stream_position, CurPos,
			Mod:stream_position(Stream,CurPos,NewPos)).


/*
 * get_token_list(Stream_or_alias, TokenList)
 */

export get_token_list/2.

get_token_list(Stream,L1) :-
	sio_next_tokens(Stream,L2,T),
	get_token_list(T,Stream,L1,L2).
get_token_list(Alias, L1) :-
	is_input_alias(Alias, Stream),
	sio_next_tokens(Stream,L2,T),
	!,
	get_token_list(T,Stream,L1,L2).
get_token_list(Stream_or_alias, List) :-
	input_stream_or_alias_ok(Stream_or_alias, Stream),
	sio_errcode(Stream, FailCode),
	get_failure(FailCode, Stream, get_token_list(Stream_or_alias,List)).

	%%% get_token_list/4:
get_token_list(T,Stream,L1,L2) :-
	var(T),
	!,
	L1=L2,
	sio_errcode(Stream, FailCode),
	get_failure(FailCode, Stream, get_token_list(Stream,T)).
get_token_list(_,Stream,L,L).


/*
 * skip_layout(Stream)
 */

skip_layout(Stream) :-
	sio_skip_layout(Stream),
	!.
skip_layout(Alias) :-
	is_input_alias(Alias, Stream),
	sio_skip_layout(Stream),
	!.
skip_layout(Stream_or_alias) :-
	input_stream_or_alias_ok(Stream_or_alias, Stream),
	sio_errcode(Stream, FailCode),
	get_failure(FailCode, Stream, skip_layout(Stream_or_alias)).


/*
 * get_number(Stream_or_alias,InputType,Number)
 *
 *	Appropriate values for InputType are:
 *
 *	byte		signed byte (8 bit)
 *	ubyte		unsigned byte (8 bit)
 *	char		signed byte (8 bit) (synonymous with byte)
 *	uchar		unsigned byte (8 bit) (synonymous with byte)
 *	short		signed short integer (16 bit)
 *	ushort		unsigned short integer (16 bit)
 *	int		signed integer (32 bit)
 *	uint		unsigned integer (32 bit)
 *	long		signed integer (32 bit)
 *	ulong		unsigned integer (32 bit)
 *	float		floating point (32 bit)
 *	double		floating point (64 bit)
 */

num_input_type(byte) :-!.
num_input_type(ubyte) :-!.
num_input_type(char) :-!.
num_input_type(uchar) :-!.
num_input_type(short) :-!.
num_input_type(ushort) :-!.
num_input_type(int) :-!.
num_input_type(uint) :-!.
num_input_type(long) :-!.
num_input_type(ulong) :-!.
num_input_type(float) :-!.
num_input_type(double) :-!.

export get_number/3.
get_number(Stream,InputType,Number) :-
	var(InputType),
	!,
	instantiation_error(2).
get_number(Stream,InputType,Number) :-
	not(num_input_type(InputType)),
	!,
	domain_error(num_input_type,InputType,2).
get_number(Stream,InputType,Number) :-
	sio_get_number(Stream,InputType,Number),
	!.
get_number(Alias,InputType,Number) :-
	is_input_alias(Alias, Stream),
	sio_get_number(Stream,InputType,Number),
	!.
get_number(Stream_or_alias, InputType, Number) :-
	input_stream_or_alias_ok(Stream_or_alias, Stream),
	sio_errcode(Stream, FailCode),
	get_failure(FailCode, Stream,
		   get_number(Stream_or_alias, InputType, Number)).


/*
 * get_line(Line)
 *
 *	Reads from the current input stream the current line or remaining
 *	portion thereof into a UIA and unifies this UIA with Line.
 */


export get_line/1.

get_line(Line) :-
	get_current_input_stream(Stream),
	get_line(Stream,Line).

/*-------------------------------------------------------------------------*
 |	get_line(Stream_or_alias, Line)
 |
 |	Reads from Stream the current line or remaining portion thereof
 |	into a UIA and unifies this UIA with Line.
 |
 |	If end-of-file is encountered before any characters, this predicate
 |	will fail.  If end-of-file is encountered before the newline, then
 |	this predicate will unify Line with the UIA containing the characters
 |	encountered up until the end-of-file.
 *-------------------------------------------------------------------------*/

export get_line/2.

get_line(Stream,Line) :-
	get_line0(Stream,Line0,EndFlag),
	gl_more(EndFlag,Stream,Line0,Line).

gl_more(-2,_,_,stream_not_ready('')) :-
	!.
gl_more(-1,_,_,_) :- !, fail.		%% fail on end of file
gl_more(0,Stream,Line0,Line) :-		%% end of line not seen
	get_line(Stream,Line1),
	!,
	gl_attach(Line1,Line0,Line).
gl_more(_,_,Line,Line).			%% End of line seen or end-of-file
					%% encountered while attempting
					%% to get rest of line

gl_attach(stream_not_ready(L2),L1,stream_not_ready(L)) :-
	!,
	'$atom_concat'(L1,L2,L).
gl_attach(L2,L1,L) :-
	!,
	'$atom_concat'(L1,L2,L).

get_line0(Stream,Line,EndFlag) :-
	sio_readln(Stream,Line,EndFlag),
	!.
get_line0(Alias,Line,EndFlag) :-
	is_input_alias(Alias, Stream),
	sio_readln(Stream,Line,EndFlag),
	!.
get_line0(Stream_or_alias,Line,EndFlag) :-
	input_stream_or_alias_ok(Stream_or_alias, Stream),
	sio_errcode(Stream, FailCode),
	get_failure(FailCode,Stream, get_line0(Stream_or_alias,Line,EndFlag)).


/*
 * put_line(Line)
 */

export put_line/1.

put_line(Line) :-
	get_current_output_stream(Stream),
	put_line(Stream,Line).

/*
 * put_line(Stream,Line)
 */

export put_line/2.

put_line(Stream,Line) :-
	put_atom(Stream,Line),
	nl(Stream).

/*
 * get_maxdepth(Stream_or_alias,Depth)
 */

export get_maxdepth/2.

get_maxdepth(Stream_or_alias,Depth) :-
	output_stream_or_alias_ok(Stream_or_alias,Stream),
	stream_wt_maxdepth(Stream,Depth).

/*
 * set_maxdepth(Stream_or_alias,Depth)
 */

export set_maxdepth/2.

set_maxdepth(Stream_or_alias,Depth) :-
	integer_ok(Depth),
	output_stream_or_alias_ok(Stream_or_alias, Stream),
	(   Depth > 0
	;   domain_error(positive_integer,Depth,
			sio:set_maxdepth(Stream_or_Alias,Depth))),
	!,
	set_stream_wt_maxdepth(Stream,Depth).


/*
 * get_line_length(Stream_or_alias,Length)
 */

export get_line_length/2.

get_line_length(Stream_or_alias,Length) :-
	var_or_integer_ok(Length),
	output_stream_or_alias_ok(Stream_or_alias, Stream),
	stream_wt_line_length(Stream,Length).


/*
 * set_line_length(Stream_or_alias,Length)
 */

export set_line_length/2.

set_line_length(Stream_or_alias,Length) :-
	integer_ok(Length),
	output_stream_or_alias_ok(Stream_or_alias, Stream),
	(   Length > 4
	;   domain_error(line_length,Length,
			 sio:set_line_length(Stream_or_alias, Length))),
	!,
	set_stream_wt_line_length(Stream,Length).


/*
 * get_depth_computation(Stream_or_alias,DC)
 */

export get_depth_computation/2.

get_depth_computation(Stream_or_alias,DC) :-
	var_or_atom_ok(DC),
	output_stream_or_alias_ok(Stream_or_alias,Stream),
	stream_wt_depth_computation(Stream,DC).


/*
 * set_depth_computation(Stream_or_alias,DC)
 */

export set_depth_computation/2.

set_depth_computation(Stream_or_alias,DC) :-
	output_stream_or_alias_ok(Stream_or_alias, Stream),
	atom_ok(DC),
	(   DC=flat
	;   DC=nonflat
	;   domain_error(depth_computation,DC,
			 sio:set_depth_computation(Stream_or_alias,DC))),
	!,
	set_stream_wt_depth_computation(Stream,DC).

/*
 * stream_property/2
 */


export stream_property/2.

stream_property(Stream, Property) :-
	var(Stream),
	!,
	pget_stream_table(Id,Stream),
	stream_property0(Property,Stream).
stream_property(Stream_or_alias, Property) :-
	var(Property),
	stream_or_alias_ok(Stream_or_alias, Stream),
	!,
	stream_property0(Property, Stream).

stream_property(Stream_or_alias, Property) :-
	stream_or_alias_ok(Stream_or_alias, Stream),
	stream_property0(Property, Stream),
	!.

stream_property0(file_name(F), Stream) :-
	stream_type(Stream,file),
	stream_name(Stream,F).
stream_property0(stream_name(SN), Stream) :-
	stream_name(Stream,SN).

stream_property0(input, Stream) :-
	stream_mode(Stream,[input|_]).
stream_property0(output, Stream) :-
	stream_mode(Stream,[_|output]).

%% FIXME: Doesn't get append mode right.
stream_property0(mode(read), Stream) :-
	stream_mode(Stream, [input|nooutput]).
stream_property0(mode(write), Stream) :-
	stream_mode(Stream, [noinput|output]).
stream_property0(mode(read_write), Stream) :-
	stream_mode(Stream, [input|output]).

stream_property0(alias(A), Stream) :-
	current_alias(A, Stream).

stream_property0(reposition(RP), Stream) :-
	stream_repositionability(Stream,RP).

stream_property0(position(P), Stream) :-
	stream_position(Stream,P).

%% FIXME:  Blocking streams
stream_property0(end_of_stream(E), Stream) :-
	stream_mode(Stream, [input|_]),
	(   sio_errcode(Stream,8) ->  E=past
	;   at_end_of_stream(Stream) ->  E=at 
	;   E=not).

stream_property0(eof_action(A), Stream) :-
	stream_eof_action(Stream,A).

stream_property0(snr_action(A), Stream) :-
	stream_snr_action(Stream, A).

stream_property0(type(T), Stream) :-
	stream_stype(Stream, T).

stream_property0(maxdepth(D), Stream) :-
	stream_mode(Stream, [_|output]),
	stream_wt_maxdepth(Stream,Depth).

stream_property0(depth_computation(DC), Stream) :-
	stream_mode(Stream, [_|output]),
	stream_wt_depth_computation(Stream,Depth).

stream_property0(line_length(Length), Stream) :-
	stream_mode(Stream, [_|output]),
	stream_wt_line_length(Stream, Length).

stream_property0(NonProperty, Stream) :-
	nonvar(NonProperty),
	domain_error(stream_property, NonProperty, 1).

/*
 * What does this do??
 */

queue_control(MsgQID, Cmd, Perms, Info) 
	:-
	encode_ipc_cmd(Cmd, CmdCode),
	msgctl(MsgQID, CmdCode, Perms, Info).

    /*---------------------------
     * Initialization of user
     *--------------------------*/
user_prompt_goal(Stream) 
	:-
	get_user_prompt(Prompt),
%stream_name(Stream, SN), pbi_write(upg+SN + '|' + Prompt + '|'), pbi_nl,
	put_atom(Stream,Prompt),
	flush_output(Stream).

export sio_pckg_init/0.

/*
 * sio_pckg_init	-- initialization procedure for streams
 *
 */

sio_pckg_init :-
    reset_alias,
    reset_stream_table,
    set_next_stream_identifier(0),

    %% User Input/Output Streams
    set_user_prompt(''),

    open(console('standard output'),write,OutStream,
	 ['$stream_identifier'(-2), alias(user_output),
	 buffering(line), type(text)]),
    open(console('standard input'), read, InStream, 
	 ['$stream_identifier'(-1), alias(user_input),
	 prompt_goal(user_prompt_goal(user_output))]),

    %% Debugger streams

    open(console('debugger output'),write, OutDStream,
	 ['$stream_identifier'(-4), alias(debugger_output),
	 buffering(line),type(text),
	 maxdepth(8), line_length(76),
	 depth_computation(nonflat)]),
    open(console('debugger input'), read, InDStream,
	 ['$stream_identifier'(-3), alias(debugger_input),
	 prompt_goal(flush_output(debugger_output))]),

    %% Error stream

    open(console_error('error output'),write,OutEStream,
	 ['$stream_identifier'(-5), alias(error_stream),
	 buffering(line),type(text)]),

%    set_input(InStream),
%    set_output(OutStream),
    set_input(user_input),
    set_output(user_output),

    %% Establish additional aliases
    set_alias(warning_input, InDStream),
    set_alias(warning_output, OutDStream).


:- sio_pckg_init.

endmod.
