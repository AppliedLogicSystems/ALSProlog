/*=======================================================================*
 |			sio_rt.pro		
 |		Copyright (c) 1991-1992 Applied Logic Systems, Inc.
 |
 |		-- Prolog parser (read_term) for use with sio.pro
 |
 | Author: Kevin A. Buettner
 | Creation:	1993
 |
 | This module defines the following predicates which appear in the standard:
 |
 |	read/1		-- read term from default input stream
 |	read/2		-- read term from supplied stream
 |	read_term/2	-- read term from default input stream with options list
 |	read_term/3	-- read term from stream with options list
 |	op/3		-- define or delete an operator
 |	current_op/3	-- look for operator
 *=======================================================================*/
module sio.

:-	make_gv('_eof_acceptable_as_fullstop'),
	set_eof_acceptable_as_fullstop(false).


export read/1.

/*
 * read(Term)
 */

read(Term) :-
	current_input(Stream),
	read_term(Stream,Term,dec10,vt([])).

/*
 * read_term(Term,Options)
 */

export read_term/2.

read_term(Term,Options) :-
	current_input(Stream),
	read_term(Stream,Term,Options).


%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Procedures common to both old and new parser
%%
%%	These were intermixed with the old parser before it was commented
%%	out.  At some point they should probably be redistributed in an
%%	appropriate manner
%%

/*
 * inc_error_count increments the number of syntax errors seen on the stream
 */

inc_error_count(Stream_or_alias) :-
	is_stream(Stream_or_alias,Stream),
	is_input_stream(Stream),
	!,
	stream_syntax_errors(Stream,Count),
	ICount is Count+1,
	set_stream_syntax_errors(Stream,ICount).


/*
 * lexerrs enumerates the lexical errors and their messages
 */

lexerrs(0,'Unterminated (double quoted) string','"').
lexerrs(1,'Unterminated quoted symbol','\'').
lexerrs(2,'Unterminated character constant','0\'').
lexerrs(3,'Unterminated C-Style comment','<end-of-file>').

lexerrs(1001,'Internal Lexical Error 1','<internal1>').
lexerrs(1002,'Internal Lexical Error 2','<internal2>').
lexerrs(1003,'Internal Lexical Error 3','<internal3>').
lexerrs(1004,'Internal Lexical Error 4','<internal4>').


/*
 * Positional parser primitives
 *
 * pp_start(Term,Pos)	-- unifies Pos with the starting position of the term
 * pp_end(Term,Pos)	-- unifies Pos with the ending position of the term
 * pp_obj(Term,Obj)	-- unifies Obj with the actual object
 *
 */

pp_obj(Term,Obj) :-	arg(1,Term,Obj).
pp_start(Term,Pos) :-	arg(2,Term,Pos).
pp_end(Term,Pos) :-	arg(3,Term,Pos).



vars_and_names(VT,Vars,Names) :-
	arg(1,VT,VL),
	vars_and_names(VL,[],Vars,[],Names).
vars_and_names([],Vars,Vars,Names,Names) :- !.
vars_and_names([[Name | dat(Var,_)] | VL], InV, OutV, InN, OutN) :-
	vars_and_names(VL, [Var|InV], OutV, [Name|InN], OutN).


singletons(VT,Vars,Names) :-
	arg(1,VT,VL),
	singletons(VL,[],Vars,[],Names).
singletons([],Vars,Vars,Names,Names) :- !.
singletons([[Name | dat(Var,single)] | VL], InV,OutV, InN,OutN) :-
	Name \= '_',
	!,
	singletons(VL, [Var|InV], OutV, [Name|InN], OutN).
singletons([_ | VL], InV, OutV, InN, OutN) :-
	singletons(VL, InV, OutV, InN, OutN).


do_rt_opt_variables(VT,Vars) :- 
	arg(1,VT,VL),
	do_rt_opt_variables(VL,[],Vars).

do_rt_opt_variables([],Vars,Vars) :- !.
do_rt_opt_variables([[_ | dat(Var,_)] | VL], InV, OutV) :-
	do_rt_opt_variables(VL,[Var | InV], OutV).


do_rt_opt_variable_names(VT,VN_list) :-
	arg(1,VT,VL),
	do_rt_opt_variable_names(VL,[],VN_list).

do_rt_opt_variable_names([],VNL,VNL) :- !.
do_rt_opt_variable_names([[Name | dat(Var,_)] | VL], InVNL,OutVNL) :-
	Name \= '_',
	!,
	do_rt_opt_variable_names(VL, [Var = Name | InVNL], OutVNL).
do_rt_opt_variable_names([_ | VL], InVNL, OutVNL) :-
	do_rt_opt_variable_names(VL, InVNL, OutVNL).

do_rt_opt_singletons(VT,VN_list) :-
	arg(1,VT,VL),
	do_rt_opt_singletons(VL,[],VN_list).

do_rt_opt_singletons([],VNL,VNL) :- !.
do_rt_opt_singletons([[Name | dat(Var,single)] | VL], InVNL,OutVNL) :-
	Name \= '_',
	!,
	do_rt_opt_singletons(VL, [Var = Name | InVNL], OutVNL).
do_rt_opt_singletons([_ | VL], InVNL, OutVNL) :-
	do_rt_opt_singletons(VL, InVNL, OutVNL).


%% rt_defaults(rt_opts(VT,SEMech))

rt_defaults(rt_opts(vt([]),dec10, linestuff(FinalToks,S,E))).

%%%%%%%%%%%%%%%%%%
%% check_rt_options(Options,Stream,OptStruct,VT,InGoals,OutGoals)
%%%%%%%%%%%%%%%%%%

check_rt_options(Var, _,_,_,_,_) :-
	var(Var),
	!,
	instantiation_error(2).
check_rt_options([],_,_,_,Goals,Goals) :-
	!.
check_rt_options([H|T], Stream, OptStruct, VT, InGoals,OutGoals) :-
	!,
	check_rt_option(H, Stream, OptStruct, VT, InGoals, TGoals),
	check_rt_options(T, Stream, OptStruct, VT, TGoals, OutGoals).
check_rt_options(Other, _,_, _, _,_) :-
	type_error(list,Other,2).

/*---------------------------------------------------------------------
 | Read options
 |
 | The March '93 version of the draft standard provides somewhat different
 | options than the older draft which was originally used to write this
 | code.  I am providing both sets of options in addition to those
 | options which we at ALS added for our own purposes.
 |
 | March '93 Draft Standard Options
 | ==============================
 |
 |	variables(Vars)
 |
 |		--  After reading a term, Vars shall be a list of
 |	the variables in the term read, in left-to-right traversal order.
 |
 |	variable_names(VN_list)  
 |
 |		--  After reading a term, VN_list shall be
 |	unified with a list of elements where: (1) each element is a term
 |	V = A, and (2) V is a named variable of the term, and (3) A is an
 |	atom whose characters are the characters of V.
 |
 |	singletons(VN_list)  
 |
 |		--  After reading a term, VN_list shall be
 |	unified with a list of elements where: (1) each element is a term
 |	V = A, and (2) V is a named variable which occurs only once in the
 |	term, and (3) A is an atom whose characters are the characters of
 |	V.
 |
 | Earlier Draft Standard Options
 | ==============================
 |
 |	syntax_errors(Val)
 |
 |		--  indicates how to handle a syntax error.
 |
 |	vars_and_names(Vars,Names)
 |
 |		--  Unifies Vars with a list of the
 |	variables encountered in a left to right traversal of the terms.
 |	Names are the associated names of the variables. '_' is the only
 |	variable name which may occur more than once.
 |
 |	singletons(Vars,Names)
 |
 |		--  Unifies Vars with the singleton variables
 |	(those occuring only once) in the term.  Names is unified with a
 |	corresponding list consisting of the names of the singleton variables.
 |	Variables with name '_' are excluded from these lists.
 |
 | Options added by ALS
 | ====================
 |
 |	variables(Vars)
 |
 |
 |	variable_names(VNList)
 |
 |
 |
 |
 |
 |
 |	debugging
 |
 |		--  Read terms are considered to be clauses and debugging
 |	information is attached.  This is likely to be of little direct
 |	use to the application programmer.
 |
 |	blocking(Bool)
 |
 |		-- This option determines whether the read is
 |	blocking (Bool = true) or non-blocking (Bool = false); default
 |	is blocking; This option has meaning for those stream types in which the
 |	receieving (reading) buffer for the stream can possibly receive only
 |	part of the characters making up a term;  these types include:
 |	sysV_queues, sockets, windows
 |
 |	attach_fullstop(Bool)
 |
 |		--  This option determines if a fullstop is
 |	added to the tokens comprising a the term to be read.  It is most
 |	useful when used in conjunction with atom or list streams.
 |
 |	line_info(Start, Len)
 |		
 |		-- In this option, Start is the offset from the beginning of 
 |	the stream at which the first character occurrs when the attempt
 |	is begun to read the term; note that this might NOT be the first
 |	character of the term to be read, since white space and/or comments
 |	may have to be traversed first;
 |	Len is the number of characters making up the term read; in case
 |	that the stream offset cannot be obtained, Start is set to -1;
 |	Executing sio_getpos AFTER the read_term call accurately gives
 |	the offset of the following character position (if this can be
 |	calculated); combining this info with 
 *---------------------------------------------------------------------*/

check_rt_option(Var, _,_, _, _,_) :-
	var(Var),
	!,
	instantiation_error(3).
check_rt_option(variables(Vars), Stream, OptStruct, VT,
			Goals, (do_rt_opt_variables(VT,Vars),Goals))  :-
	!.
check_rt_option(variable_names(VN_list), Stream, OptStruct, VT,
			Goals, (do_rt_opt_variable_names(VT,VN_list),Goals)) :-
	!.
check_rt_option(singletons(VN_list), Stream, OptStruct, VT,
			Goals, (do_rt_opt_singletons(VT,VN_list),Goals)) :-
	!.
%% FIXME: Check syntax_errors value
check_rt_option(syntax_errors(EM), Stream, OptStruct, VT, G,G) :-
	!,
	mangle(2,OptStruct,EM).
check_rt_option(vars_and_names(Vars,Names), Stream, OptStruct, VT,
			Goals, (vars_and_names(VT,Vars,Names),Goals)) :-
	!.
check_rt_option(singletons(Vars,Names), Stream, OptStruct, VT,
			Goals, (singletons(VT,Vars,Names),Goals)) :-
	!.
check_rt_option(debugging, Stream, OptStruct, VT, G, G) :-
	!,
	mangle(1,OptStruct,pvt([])).
check_rt_option(blocking(NewBlocking), Stream, OptStruct, VT, G,OG) :-
	!,
	stream_blocking(Stream,OldBlocking),
	blocking_switch(NewBlocking, OldBlocking, Stream, G, OG).
check_rt_option(line_info(Start,End), Stream, OptStruct, VT, G,(G,H)) :-
	!,
	arg(3,OptStruct, linestuff(FinalToks,Start,End) ),
	H = ending_line(FinalToks,Start,End).
check_rt_option(attach_fullstop(true), Stream, OptStruct, VT, G,G) :-
	!,
	set_eof_acceptable_as_fullstop(true).
check_rt_option(attach_fullstop(false), Stream, OptStruct, VT, G,G) :-
	!,
	set_eof_acceptable_as_fullstop(false).
check_rt_option(Culprit, _,_, _, _,_) :-
	domain_error(read_option,Culprit,3).

blocking_switch(Block, Block, Stream, G, G) :- !.
blocking_switch(true, false, Stream, G, (set_stream_blocking(Stream,false), G)) :- 
	!,
	set_stream_blocking(Stream,true).
blocking_switch(false, true, Stream, G, (set_stream_blocking(Stream,true), G)) :-
	set_stream_blocking(Stream,false).

ending_line([],S,E).
ending_line([lineinfo(_,E,_) |_],S,E) 
	:-!.
ending_line([_ | Toks],S,E)
	:-
	ending_line(Toks,S,E).

%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% read(Stream,Term)
%%
%%

export read/2.

read(Stream_or_alias,Term) :-
	input_stream_or_alias_ok(Stream_or_alias,Stream),
	nonvar(Stream_or_alias),	%% force second usage of
					%% Stream_or_alias for error
					%% checking code
	read_term(Stream,Term,dec10,vt([])),
	!.				%% cut catch in read_term/4


/*-----------------------------------------------------------------*
 |	read_term/3
 |	read_term(Stream, Term, Options)
 |	read_term(+, -, +)
 *-----------------------------------------------------------------*/
export read_term/3.

read_term(Stream_or_alias,Term,Options) 
	:-
	input_stream_or_alias_ok(Stream_or_alias,Stream),
	rt_defaults(OptStruct),
				%% OptGoals are executed on the way out after the read:
				%% Need later occurrences to preserve
				%% vars for error reporting code
	check_rt_options(Options, Stream, OptStruct, VT, true, OptGoals),
	nonvar(Stream_or_alias),	
	nonvar(Options),			
	arg(1, OptStruct, VT),
	arg(2, OptStruct, ErrMech),
	arg(3, OptStruct, linestuff(FinalToks,S,E)),
	read_term(Stream,Term,ErrMech,VT,FinalToks),
	OptGoals,
	!,			%% <- cut choice point associated with catch in read_term/4
	set_eof_acceptable_as_fullstop(false).

	%% read_term/4:
read_term(Stream,Term,ErrMech,VT) 
	:-
	read_term(Stream,Term,ErrMech,VT,_).

/*-----------------------------------------------------------------*
 |	read_term/5
 |	read_term(Stream, Term, ErrMech, VT, FinalToks)
 |	read_term(+, -, -, -, -)
 *-----------------------------------------------------------------*/
read_term(Stream,Term,ErrMech,VT,FinalToks) 
	:-
	tp_get_token_list(Stream,[Tok1|TokRest]),
	catch(	rt_readclause(Tok1,TokRest,VT,Term, FinalToks),
		syntax_error(Token,Message),
		rt_err(ErrMech,Token,Message,[Tok1|TokRest],Stream,VT,Term)).

/*-----------------------------------------------------------------*
 |	rt_err/7
 |	rt_err(ErrMech,Token,Message,Tokens,Stream,VT,Term)
 |	rt_err(+,+,+,+,+,+,+)
 *-----------------------------------------------------------------*/
rt_err(quiet,Token,ErrorMessage,Tokens,Stream,VT,Term) :-
	inc_error_count(Stream),
	!,
	fail.
rt_err(fail,Token,ErrorMessage,Tokens,Stream,VT,Term) :-
	inc_error_count(Stream),
	rt_err_print(Token,ErrorMessage,Tokens,Stream),
	!,
	fail.
rt_err(dec10,Token,ErrorMessage,Tokens,Stream,VT,Term) :-
	inc_error_count(Stream),
	rt_err_print(Token,ErrorMessage,Tokens,Stream),
	!,
	mangle(1,VT,[]),	/* reset variable table to empty list */
	read_term(Stream,Term,dec10,VT).
rt_err(error,Token,ErrorMessage,Tokens,Stream,VT,Term) :-
	inc_error_count(Stream),
	frame_info(3,FI),
	rt_err_info(Token,ErrorMessage,Tokens,Stream, Info),
	!,
	builtins:setPrologError(error(syntax_error,[FI,Info])),
	forcePrologError.

rt_err_print(Token,ErrorMessage,Tokens,Stream) :-
	rt_err_info(Token,ErrorMessage,Tokens,Stream, Info),
	prolog_system_error(Info, []).

rt_err_info(Token,ErrorMessage,Tokens,Stream, SyntaxStruct) :-
	SyntaxStruct = syntax(0,ErrorMessage,LineNumber,Stream),
	rt_isolate_error(Tokens,Token,IL1,IL2,
			 lineinfo(StreamName,LineNumber,_)),
	open(atom(ErrString),write,ES),
	rt_startpos(IL1,IL2,StartPos),
	rt_werrln(IL1,ES,StartPos,Pos2),
	rt_startpos(IL2,[],Pos3),
	DifPos is Pos3-Pos2,
	tab(ES,DifPos),
	sio_position_in_line(ES,_,CaratPos),
	rt_werrln(IL2,ES,Pos3,_),
	nl(ES),
	tab(ES,CaratPos),
	write(ES,'^'),
	close(ES),
	mangle(1,SyntaxStruct,ErrString).


%%
%% rt_isolate_error(L,E,OL1,OL2,I)
%%

rt_isolate_error(L,E,OL1,OL2,I) :-
	rt_isolate_error(L,E,H,H,OL1,OL2,I).

%%
%% rt_isolate_error(L,E,AL,H,OL1,OL2,I)
%%	L  -- list to search
%%	E  -- element to search for
%%	A  -- list which we are accumulating information in
%%	H  -- hole at end of A
%%	OL -- output list
%%	I  -- info at end of line

rt_isolate_error([], E, A, [], A, [], lineinfo('??','??'.'??')) :-
	!.
rt_isolate_error([lineinfo(_,_,_) | T], E, _, _, OL1, OL2, I) :-
	!,
	rt_isolate_error(T, E, H, H, OL1, OL2, I).
rt_isolate_error([E | T], E, A, [], A, [E|H], I) :-
	!,
	rt_isolate_error0(T,H,I).
rt_isolate_error([C | T], E, A, [C | H], OL1, OL2, I) :-
	rt_isolate_error(T, E, A, H, OL1, OL2, I).

rt_isolate_error0([],[],lineinfo('??','??','??')) :-
	!.
rt_isolate_error0([LI | _],[],LI) :-
	functor(LI,lineinfo,3),
	!.
rt_isolate_error0([E | T1], [E | T2], I) :-
	rt_isolate_error0(T1,T2,I).


%%
%% rt_startpos/3
%%	Position of first token in error line
%%

rt_startpos([Tok|_],_,Pos) :-
	arg(2,Tok,Pos),
	!.
rt_startpos(_,[Tok|_],Pos) :-
	arg(2,Tok,Pos),
	!.
rt_startpos(_,_,0).

%%
%% rt_werrln/3
%%	Write out error line
%%

rt_werrln([],ES,Pos,Pos) :- !.
rt_werrln([Tok|Toks],ES, PPos, OPos) :-
	arg(1,Tok,WTok),
	arg(2,Tok,SPos),
	arg(3,Tok,EPos),
	DifPos is SPos-PPos,
	tab(ES,DifPos),
	write(ES,WTok),
	rt_werrln(Toks,ES,EPos,OPos).


/*
 * 	P A R S E R    C O R E
 *
 * What follows is the "core" of the parser.  This core is divided up into
 * two parts.   Each part is a parser unto itself, but one part return's
 * terms in the form we expect to see them in.  These terms are the objects
 * returned through read.   The other part returns terms in a canonicalized
 * form with positional information attached.  (The positional information
 * here is information about the position that each part of the term came
 * from from within the stream.)
 *
 * This information takes the form
 *
 *	atom(Atom,Start,End)
 *	list(List,Start,End)
 *	struct(Struct,Start,End)
 *	var(Var,Start,End,VarName)
 *
 * So a stream starting with the term
 *
 *	p([a,bc,X],9,X,Y)
 *
 * would return via the positional parser (hence the pp_ prefix's)
 *
 *	struct( p(	list([atom(a,3,3),atom(bc,5,6),var(_1,8,8,'X')],2,9),
 *			atom(9,11,11),
 *			var(_1,13,13,'X'),
 *			var(_2,15,15,'Y'))),
 *		0,16)
 *
 * The positional parser below is virtually identical to the ordinary parser
 * except that positional information is recorded in the structures described
 * above.  Any procedure which used exclusively in the implementation of the
 * positional parser is prefixed with pp_. Moreover, the positional parser
 * procedures are intermixed with the normal parser procedures so that
 * maintenance is easier.  A fix to the normal parser must also be made to
 * the positional parser.
 */


%%
%% rt_readclause/5	-- parse a clause (including fullstop)
%% rt_readclause(Tok1,TokRest,VT,Term)
%%
%%
%%	Tok1	-- first token to consume
%%	TokRest	-- all remaining tokens in the list to consume
%%	VT	-- Variable table; normally the principal functor on
%%		   this term is vt/1.  pvt/1 as the principal functor
%%		   indicates that the positional parser should be used.
%%	Term	-- Term that results from parsing.
%%	FinalToks -- (unconsumed) List of tokens following the fullstop
%%
%%

rt_readclause(Tok1,TokRest,VT,Term,FinalToks)
	:-
	VT=pvt(_),		%% use positional parser instead
	!,
	pp_rt_readclause(Tok1,TokRest,VT,Term,FinalToks).

rt_readclause(lineinfo(_,_,_),[H|T],VT,Term,FinalToks)
	:- !,
	rt_readclause(H,T,VT,Term,FinalToks).	%% skip line number info

rt_readclause(end_of_file(Stream,_,_),_,VT,OutTerm,[])
	:- !,
	stream_eof_action(Stream, EOFAction),
	rt_readclause_eof(EOFAction,Stream,OutTerm).

rt_readclause(Tok1,TokRest,VT,OutTerm,FinalToks)
	:-
	rt_primary(Tok1,TokRest,VT,1200,TokRemain,Term),
	rt_fullstop(TokRemain,FinalToks),
	OutTerm=Term.

pp_rt_readclause(lineinfo(_,_,_),[H|T],VT,Term,FinalToks)
	:- !,
	pp_rt_readclause(H,T,VT,Term,FinalToks).	%% skip line number info

pp_rt_readclause(end_of_file(Stream,_,_),_,VT,OutTerm,[])
	:- !,
	stream_eof_action(Stream, EOFAction),
	rt_readclause_eof(EOFAction, Stream, OutTerm).

pp_rt_readclause(Tok1,TokRest,VT,OutTerm,FinalToks)
	:-
	pp_rt_primary(Tok1,TokRest,VT,1200,TokRemain,Term),
	rt_fullstop(TokRemain,FinalToks),
	!,
	OutTerm=Term.		

rt_readclause_eof(error,Stream,OutTerm)
	:- !,
	existence_error(past_end_of_stream,Stream,3).
rt_readclause_eof(_, Stream, end_of_file).


%%
%% rt_fullstop/2
%%
%%	Checks for presence of a fullstop at the end of a clause
%%

rt_fullstop([lineinfo(A,B,C)|T],FinalToks) :-
	!,
	rt_fullstop(T,FinalToks).
rt_fullstop([fullstop(_,_,_)|FinalToks],FinalToks) :-
	!.
rt_fullstop([end_of_file(_,_,_)|FinalToks],FinalToks) :-
	get_eof_acceptable_as_fullstop(true),
	!.
rt_fullstop([Tok|FinalToks],FinalToks) :-
	parser_error(Tok,'Fullstop (period) expected').


/*
 * parser_error/2
 *	Called by the parser to signal a syntax error.  It
 * 	performs a throw in order to let a surrounding catch handle the
 *	error.
 */
 
parser_error(Token,_) :-
	Token=lexerr(Value,_,_),
	lexerrs(Value,Message,Value2),
	!,
	mangle(1,Token,Value2),
	throw(syntax_error(Token,Message)).
parser_error(Token,Message) :-
	throw(syntax_error(Token,Message)).

	
%%
%% rt_primary/6		-- consume tokens constituting primary of given
%%			   precedence
%% rt_primary(Tok1,TokRest,VT,Prec,TokRemain,Ans)
%%
%%	Tok1	-- initial token in the primary expression
%%	TokRest	-- all tokens after initial token in the primary expression
%%	VT	-- variable table
%%	Prec	-- maximum precedence of unparenthesized operators in
%%		   the primary expression
%%	TokRemain --
%%		   the tokens which follow the parsed primary expression
%%	Ans	-- the term to return as the parsed primary expression
%%

rt_primary(lineinfo(_,_,_),[X1|XR], VT,Prec, TR, Ans) :-
	!,
	rt_primary(X1,XR,VT,Prec,TR,Ans).
rt_primary(integer(I,_,_),[W1|WR], VT,Prec, TR,Ans) :-
	!,
	rt_rops(W1,WR, I,0, VT,Prec, TR,Ans).
rt_primary(float(F,_,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	rt_rops(W1,WR, F,0, VT,Prec, TR,Ans).
rt_primary(string(S,_,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	rt_rops(W1,WR, S,0, VT,Prec, TR,Ans).
rt_primary(long_string(S1,_,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	rt_string(W1,WR,S1,[X1|XR],S),
	rt_rops(X1,XR, S,0, VT, Prec, TR,Ans).
rt_primary(var(VName,_,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	vlookup(VName,VT,V),
	rt_rops(W1,WR, V,0, VT,Prec, TR,Ans).
rt_primary('['(_,_,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	rt_list(W1,WR,VT,[X1|XR],List),
	rt_rops(X1,XR, List,0, VT,Prec, TR,Ans).
rt_primary(symbol(-,_,_),[W1|WR], VT,Prec, TR,Ans) :-
	rt_number(W1,WR, Num, [X1|XR]),
	!,
	NegNum is -Num,
	rt_rops(X1,XR, NegNum,0, VT,Prec, TR,Ans).
rt_primary(symbol(F,_,_),[W1|WR], VT,Prec, TR,Ans) :-
	preop(F,FN,FR),
	Prec >= FN,
	rt_preop_ok(W1,WR,X1,XR),
	!,
	rt_primary(X1,XR, VT,FR, [Y1|YR], Arg),
	functor(F_of_Arg,F,1),
	arg(1,F_of_Arg,Arg),
	rt_rops(Y1,YR, F_of_Arg,FN, VT,Prec, TR,Ans).
rt_primary(symbol(Sym,_,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	rt_struct(W1,WR,Sym,VT,[X1|XR],S),
	rt_rops(X1,XR, S,0, VT,Prec, TR,Ans).
rt_primary(long_qsymbol(S1,_,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	rt_qsymbol(W1,WR,S1,[X1|XR],Sym),
	rt_struct(X1,XR,Sym,VT,[Y1|YR],S),
	rt_rops(Y1,YR, S,0, VT,Prec, TR,Ans).
rt_primary(functor(Sym,_,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	rt_struct(W1,WR,Sym,VT,[X1|XR],S),
	rt_rops(X1,XR, S,0, VT,Prec, TR,Ans).
rt_primary(','(Sym,_,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	rt_struct(W1,WR,Sym,VT,[X1|XR],S),
	rt_rops(X1,XR, S,0, VT,Prec, TR,Ans).
rt_primary(';'(Sym,_,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	rt_struct(W1,WR,Sym,VT,[X1|XR],S),
	rt_rops(X1,XR, S,0, VT,Prec, TR,Ans).
rt_primary('!'(Sym,_,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	rt_struct(W1,WR,Sym,VT,[X1|XR],S),
	rt_rops(X1,XR, S,0, VT,Prec, TR,Ans).
rt_primary('('(_,_,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	rt_primary(W1,WR,VT,1200,[X1|XR],S),
	rt_rparen(X1,XR,[Y1|YR]),
	rt_rops(Y1,YR, S,0, VT,Prec, TR,Ans).
rt_primary('{'(_,_,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	rt_primary(W1,WR,VT,1200,[X1|XR],S),
	rt_rcurly(X1,XR,[Y1|YR]),
	rt_rops(Y1,YR, '{}'(S),0, VT,Prec, TR,Ans).
rt_primary(W1,WR, VT,Prec,TR,Ans) :-
	parser_error(W1,'Term expected').


pp_rt_primary(lineinfo(_,_,_),[X1|XR], VT,Prec, TR, Ans) :-
	!,
	pp_rt_primary(X1,XR,VT,Prec,TR,Ans).
pp_rt_primary(integer(I,Pstrt,Pend),[W1|WR], VT,Prec, TR,Ans) :-
	!,
	pp_rt_rops(W1,WR, atom(I,Pstrt,Pend),0, VT,Prec, TR,Ans).
pp_rt_primary(float(F,Pstrt,Pend),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	pp_rt_rops(W1,WR, atom(F,Pstrt,Pend),0, VT,Prec, TR,Ans).
pp_rt_primary(string(S,Pstrt,Pend),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	pp_rt_rops(W1,WR, atom(S,Pstrt,Pend),0, VT,Prec, TR,Ans).
pp_rt_primary(long_string(S1,Pstrt,Pend),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	pp_rt_string(W1,WR,atom(S1,Pstrt,Pend),[X1|XR],S),
	pp_rt_rops(X1,XR, S,0, VT, Prec, TR,Ans).
pp_rt_primary(var(VName,Pstrt,Pend),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	vlookup(VName,VT,V),
	pp_rt_rops(W1,WR, var(V,Pstrt,Pend,VName),0, VT,Prec, TR,Ans).
pp_rt_primary('['(_,Pstrt,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	pp_rt_list(W1,WR,VT,[X1|XR],List),
	pp_start(List,Pstrt),
	pp_rt_rops(X1,XR, List,0, VT,Prec, TR,Ans).
pp_rt_primary(symbol(-,Pstrt,_),VR, VT,Prec, TR,Ans) :-
	pp_rt_nexttok(VR,W1,WR),
	pp_end(W1,Pend),
	rt_number(W1,WR, Num, [X1|XR]),
	!,
	NegNum is -Num,
	pp_rt_rops(X1,XR, atom(NegNum,Pstrt,Pend),0, VT,Prec, TR,Ans).
pp_rt_primary(symbol(F,Pstrt,_),[W1|WR], VT,Prec, TR,Ans) :-
	preop(F,FN,FR),
	Prec >= FN,
	rt_preop_ok(W1,WR,X1,XR),
	!,
	pp_rt_primary(X1,XR, VT,FR, [Y1|YR], Arg),
	pp_end(Arg,Pend),
	functor(F_of_Arg,F,1),
	arg(1,F_of_Arg,Arg),
	pp_rt_rops(Y1,YR, struct(F_of_Arg,Pstrt,Pend),FN, VT,Prec, TR,Ans).
pp_rt_primary(symbol(Sym,PStrt,Pend),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	pp_rt_struct(W1,WR,atom(Sym,PStrt,Pend),VT,[X1|XR],S),
	pp_rt_rops(X1,XR, S,0, VT,Prec, TR,Ans).
pp_rt_primary(long_qsymbol(S1,Pstrt,Pend),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	pp_rt_qsymbol(W1,WR,atom(S1,Pstrt,Pend),[X1|XR],Sym),
	pp_rt_struct(X1,XR,Sym,VT,[Y1|YR],S),
	pp_rt_rops(Y1,YR, S,0, VT,Prec, TR,Ans).
pp_rt_primary(functor(Sym,Pstrt,Pend),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	pp_rt_struct(W1,WR,atom(Sym,Pstrt,Pend),VT,[X1|XR],S),
	pp_rt_rops(X1,XR, S,0, VT,Prec, TR,Ans).
pp_rt_primary(','(Sym,Pstrt,Pend),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	pp_rt_struct(W1,WR,atom(Sym,Pstrt,Pend),VT,[X1|XR],S),
	pp_rt_rops(X1,XR, S,0, VT,Prec, TR,Ans).
pp_rt_primary(';'(Sym,Pstrt,Pend),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	pp_rt_struct(W1,WR,atom(Sym,Pstrt,Pend),VT,[X1|XR],S),
	pp_rt_rops(X1,XR, S,0, VT,Prec, TR,Ans).
pp_rt_primary('!'(Sym,Pstrt,Pend),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	pp_rt_struct(W1,WR,atom(Sym,Pstrt,Pend),VT,[X1|XR],S),
	pp_rt_rops(X1,XR, S,0, VT,Prec, TR,Ans).
pp_rt_primary('('(_,Pstrt,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	pp_rt_primary(W1,WR,VT,1200,WRR,S),
	pp_rt_nexttok(WRR,X1,XR),	%% skip over lineinfo tokens
	pp_end(X1,Pend),		%% get end position of right paren
	rt_rparen(X1,XR,[Y1|YR]),	%%  if indeed it is a right paren
	mangle(2,S,Pstrt),		%% We could make a copy here 
	mangle(3,S,Pend),		%%  instead of doing the mangles
	pp_rt_rops(Y1,YR, S,0, VT,Prec, TR,Ans).
pp_rt_primary('{'(_,Pstrt,_),[W1|WR],VT,Prec,TR,Ans) :-
	!,
	pp_rt_primary(W1,WR,VT,1200,[X1|XR],S),
	pp_rt_nexttok(WRR,X1,XR),
	pp_end(X1,Pend),
	rt_rcurly(X1,XR,[Y1|YR]),
	pp_rt_rops(Y1,YR, struct('{}'(S),Pstrt,Pend),0, VT,Prec, TR,Ans).
pp_rt_primary(W1,WR, VT,Prec,TR,Ans) :-
	parser_error(W1,'Term expected').


%%
%% rt_number/4	-- match a number
%%
%%

rt_number(lineinfo(_,_,_),[X1|XR],N,TR) :-
	!,
	rt_number(X1,XR,N,TR).
rt_number(float(N,_,_),TR,N,TR).
rt_number(integer(N,_,_),TR,N,TR).


%%
%% rt_rparen/3	-- match right parenthesis
%%
%%

rt_rparen(lineinfo(_,_,_),[X1|XR],TR) :-
	!,
	rt_rparen(X1,XR,TR).
rt_rparen(')'(_,_,_),TR,TR) :-
	!.
rt_rparen(W1,WR,TR) :-
	parser_error(W1,'Right paren expected').

%%
%% rt_rcurly/3	-- match right curly brace
%%
%%

rt_rcurly(lineinfo(_,_,_),[X1|XR],TR) :-
	!,
	rt_rcurly(X1,XR,TR).
rt_rcurly('}'(_,_,_),TR,TR) :-
	!.
rt_rcurly(W1,WR,TR) :-
	parser_error(W1,'Right curly brace expected after expression').


%%
%% rt_qsymbol/5	-- make a long symbol
%%
%%

rt_qsymbol(lineinfo(_,_,_),[W1|WR],Initial,TR,Final) :-
	rt_qsymbol(W1,WR,Initial,TR,Final).
rt_qsymbol(functor(S,_,_),WR,Initial,WR,Concat) :-
	!,
	'$atom_concat'(Initial,S,Concat).
rt_qsymbol(symbol(S,_,_),WR,Initial,WR,Concat) :-
	!,
	'$atom_concat'(Initial,S,Concat).
rt_qsymbol(long_qsymbol(S,_,_),[W1|WR],Initial,TR,Final) :-
	'$atom_concat'(Initial,S,Concat),
	rt_qsymbol(W1,WR,Concat,TR,Final).


pp_rt_qsymbol(lineinfo(_,_,_),[W1|WR],Initial,TR,Final) :-
	pp_rt_qsymbol(W1,WR,Initial,TR,Final).
pp_rt_qsymbol(functor(S,_,Pend),WR,atom(Initial,Pstrt,_),WR,
						atom(Concat,Pstrt,Pend)) :-
	!,
	'$atom_concat'(Initial,S,Concat).
pp_rt_qsymbol(symbol(S,_,Pend),WR,atom(Initial,Pstrt,_),WR,
						atom(Concat,Pstrt,Pend)) :-
	!,
	'$atom_concat'(Initial,S,Concat).
pp_rt_qsymbol(long_qsymbol(S,_,Pend),[W1|WR],atom(Initial,Pstrt,_),TR,Final) :-
	'$atom_concat'(Initial,S,Concat),
	pp_rt_qsymbol(W1,WR,atom(Concat,Pstrt,Pend),TR,Final).

%%
%% rt_string/5 -- make a long string
%%
%%

rt_string(lineinfo(_,_,_),[W1|WR],Initial,TR,Final) :-
	rt_string(W1,WR,Initial,TR,Final).
rt_string(string(S,_,_),WR,Initial,WR,Concat) :-
	!,
	dappend(Initial,S,Concat).
rt_string(long_string(S,_,_),[W1|WR],Initial,TR,Final) :-
	dappend(Initial,S,Concat),
	rt_string(W1,WR,Concat,TR,Final).

pp_rt_string(lineinfo(_,_,_),[W1|WR],Initial,TR,Final) :-
	pp_rt_string(W1,WR,Initial,TR,Final).
pp_rt_string(string(S,_,_),WR,atom(Initial,Pstrt,_),WR,
				atom(Concat,Pstrt,Pend)) :-
	!,
	dappend(Initial,S,Concat).
pp_rt_string(long_string(S,_,Pend),[W1|WR],atom(Initial,Pstrt,_),TR,Final) :-
	dappend(Initial,S,Concat),
	rt_string(W1,WR,atom(Concat,Pstrt,Pend),TR,Final).


%%
%% rt_list/5	-- match a list
%%
%%

rt_list(lineinfo(_,_,_),[X1|XR],VT,TR,List) :-
	!,
	rt_list(X1,XR,VT,TR,List).
rt_list(']'(_,_,_), TR, VT, TR, []) :-
	!.
rt_list(W1,WR, VT, TR, [Head|Tail]) :-
	rt_primary(W1,WR, VT,999, [X1|X2], Head),
	rt_list_tail(X1,X2, VT, TR, Tail).

rt_list_tail(lineinfo(_,_,_),[X1|XR],VT,TR,Tail) :-
	!,
	rt_list_tail(X1,XR,VT,TR,Tail).
rt_list_tail(']'(_,_,_),TR,VT,TR,[]) :-
	!.
rt_list_tail('|'(_,_,_),[W1|WR],VT,TR,Tail) :-
	!,
	rt_primary(W1,WR, VT,999, [X1|X2], Tail),
	rt_rbrac(X1,X2,TR).
rt_list_tail(','(_,_,_),[W1|WR],VT,TR,[Head|Tail]) :-
	!,
	rt_primary(W1,WR, VT,999, [X1|X2], Head),
	rt_list_tail(X1,X2, VT,TR, Tail).
rt_list_tail(W1,WR, VT,TR, Tail) :-
	parser_error(W1,
	    'Comma, right square bracket, or vertical bar expected in list').

pp_rt_list(lineinfo(_,_,_),[X1|XR],VT,TR,List) :-
	!,
	pp_rt_list(X1,XR,VT,TR,List).
pp_rt_list(']'(_,_,Pend), TR, VT, TR, atom([],_,Pend)) :-
	!.
pp_rt_list(W1,WR, VT, TR, list([Head|Tail],_,Pend)) :-
	pp_rt_primary(W1,WR, VT,999, [X1|XR], Head),
	pp_rt_list_tail(X1,XR, VT, [Y1|YR], Tail),
	pp_end(Y1,Pend),
	rt_rbrac(Y1,YR,TR).

pp_rt_list_tail(lineinfo(_,_,_),[X1|XR],VT,TR,Tail) :-
	!,
	pp_rt_list_tail(X1,XR,VT,TR,Tail).
pp_rt_list_tail(X1,XR,VT,[X1|XR],[]) :-
	X1 = ']'(_,_,_),
	!.
pp_rt_list_tail('|'(_,_,_),[W1|WR],VT,TR,Tail) :-
	!,
	pp_rt_primary(W1,WR, VT,999, TR, Tail).
pp_rt_list_tail(','(_,_,_),[W1|WR],VT,TR,[Head|Tail]) :-
	!,
	pp_rt_primary(W1,WR, VT,999, [X1|X2], Head),
	pp_rt_list_tail(X1,X2, VT,TR, Tail).
pp_rt_list_tail(W1,WR, VT,TR, Tail) :-
	parser_error(W1,
	    'Comma, right square bracket, or vertical bar expected in list').


%%
%% rt_rbrac/3		-- match right square bracket
%%

rt_rbrac(lineinfo(_,_,_),[X1|XR],TR) :-
	!,
	rt_rbrac(X1,XR,TR).
rt_rbrac(']'(_,_,_),TR,TR) :-
	!.
rt_rbrac(W1,WR,TR) :-
	parser_error(W1,'Right square bracket expected after list tail').


%%
%% rt_struct/6, rt_args/5
%%	Parse structured term and arguments
%%

rt_struct(lineinfo(_,_,_),[X1|XR],Functor,VT,TR,Struct) :-
	!,
	rt_struct(X1,XR,Functor,VT,TR,Struct).
rt_struct('('(_,_,_),[W1|WR],Functor,VT,TR,Struct) :-
	!,
	rt_primary(W1,WR, VT,999, [X1|XR], FirstArg),
	rt_args(X1,XR, VT, TR, RestArgs),
	Struct =.. [Functor,FirstArg|RestArgs].
rt_struct(X1,XR, Atom, VT, [X1|XR], Atom).


rt_args(lineinfo(_,_,_),[X1|XR], VT,TR,Args) :-
	!,
	rt_args(X1,XR,VT,TR,Args).
rt_args(')'(_,_,_), TR, VT, TR, []) :-
	!.
rt_args(','(_,_,_), [W1|WR], VT, TR, [Arg|Rest]) :-
	!,
	rt_primary(W1,WR, VT,999, [X1|XR], Arg),
	rt_args(X1,XR, VT,TR, Rest).
rt_args(W1,WR, VT,TR, RestArgs) :-
	parser_error(W1,
		'Comma or right paren expected after structure argument').


pp_rt_struct(lineinfo(_,_,_),[X1|XR],Functor,VT,TR,Struct) :-
	!,
	pp_rt_struct(X1,XR,Functor,VT,TR,Struct).
pp_rt_struct('('(_,_,_),[W1|WR],atom(Functor,Pstrt,_),VT,TR,
				struct(Struct,Pstrt,Pend)) :-
	!,
	pp_rt_primary(W1,WR, VT,999, [X1|XR], FirstArg),
	pp_rt_args(X1,XR, VT, [T1|TR], RestArgs),
	pp_end(T1,Pend),
	Struct =.. [Functor,FirstArg|RestArgs].
pp_rt_struct(X1,XR, Atom, VT, [X1|XR], Atom).


pp_rt_args(lineinfo(_,_,_),[X1|XR], VT,TR,Args) :-
	!,
	pp_rt_args(X1,XR,VT,TR,Args).
pp_rt_args(T1, TR, VT, [T1|TR], []) :-
	T1 = ')'(_,_,_),
	!.
pp_rt_args(','(_,_,_), [W1|WR], VT, TR, [Arg|Rest]) :-
	!,
	pp_rt_primary(W1,WR, VT,999, [X1|XR], Arg),
	pp_rt_args(X1,XR, VT,TR, Rest).
pp_rt_args(W1,WR, VT,TR, RestArgs) :-
	parser_error(W1,
		'Comma or right paren expected after structure argument').


%%
%% rt_rops(Tok,Toks, LArg,LPrec, VT,Prec, TR,Ans)
%%
%%	Tok	-- next token to consume
%%	Toks	-- list with remaining tokens in it
%%	LArg	-- Left argument to binary (or postfix) operator
%%	LPrec	-- Precedence of LArg
%%	VT	-- Variable table
%%	Prec	-- Precedence that operator must be less than
%%	TR	-- Remaining tokens to consume
%%	Ans	-- Output term
%%
%% This predicate is called to consume all legally consumable operators
%% to the right of an expression.  An operator will be legally consumable
%% iff its precedence is less than or equal to Prec (see above).
%%

rt_rops(lineinfo(_,_,_),[X1|XR], LArg,LPrec, VT,Prec, TR,Ans) :-
	!,
	rt_rops(X1,XR, LArg,LPrec, VT,Prec, TR,Ans).

rt_rops(X1,XR, LArg,LPrec, VT,Prec, TR,Ans) :-
	possible_binary_operator(X1,F),
	binop(F,FP,FL,FR),
	Prec >= FP,
%	LPrec =< FL,		%% This will cause failure on assoc conflicts
	rt_check_binop_conflict(LPrec,FL,X1),
	!,
	rt_nexttok(XR,Y1,YR),
	rt_primary(Y1,YR, VT,FR, [Z1|ZR],RArg),
	functor(Arg,F,2),
	arg(1,Arg,LArg),
	arg(2,Arg,RArg),
	rt_rops(Z1,ZR, Arg,FP, VT,Prec, TR,Ans).

rt_rops(X1,XR, LArg,LPrec, VT,Prec, TR,Ans) :-
	functor(X1,symbol,3),
	arg(1,X1,F),
	postop(F,FP,FL),
	Prec >= FP,
%%	LPrec =< FL,		%% This will cause failure on assoc conflicts
	rt_check_postop_conflict(LPrec,FL,X1),
	!,
	functor(Arg,F,1),
	arg(1,Arg,LArg),
	rt_nexttok(XR,Y1,YR),
	rt_rops(Y1,YR, Arg,FP, VT,Prec, TR,Ans).

rt_rops(X1,XR, LArg,LPrec, VT,Prec, TR, Ans) :-
	rt_rops_illegal(X1),
	!,
	parser_error(X1,
		'Illegal token after primary expression').
	
rt_rops(X1,XR, Ans,LPrec, VT,Prec, [X1|XR],Ans).


pp_rt_rops(lineinfo(_,_,_),[X1|XR], LArg,LPrec, VT,Prec, TR,Ans) :-
	!,
	pp_rt_rops(X1,XR, LArg,LPrec, VT,Prec, TR,Ans).

pp_rt_rops(X1,XR, LArg,LPrec, VT,Prec, TR,Ans) :-
	possible_binary_operator(X1,F),
	binop(F,FP,FL,FR),
	Prec >= FP,
%	LPrec =< FL,		%% This will cause failure on assoc conflicts
	rt_check_binop_conflict(LPrec,FL,X1),
	!,
	rt_nexttok(XR,Y1,YR),
	pp_rt_primary(Y1,YR, VT,FR, [Z1|ZR],RArg),
	functor(Arg,F,2),
	arg(1,Arg,LArg),
	arg(2,Arg,RArg),
	pp_start(LArg,Pstrt),
	pp_end(RArg,Pend),
	pp_rt_rops(Z1,ZR, struct(Arg,Pstrt,Pend),FP, VT,Prec, TR,Ans).

pp_rt_rops(X1,XR, LArg,LPrec, VT,Prec, TR,Ans) :-
	functor(X1,symbol,3),
	arg(1,X1,F),
	postop(F,FP,FL),
	Prec >= FP,
%%	LPrec =< FL,		%% This will cause failure on assoc conflicts
	rt_check_postop_conflict(LPrec,FL,X1),
	!,
	functor(Arg,F,1),
	arg(1,Arg,LArg),
	rt_nexttok(XR,Y1,YR),
	pp_start(LArg,Pstrt),
	pp_end(X1,Pend),
	pp_rt_rops(Y1,YR, struct(Arg,Pstrt,Pend),FP, VT,Prec, TR,Ans).

pp_rt_rops(X1,XR, LArg,LPrec, VT,Prec, TR, Ans) :-
	rt_rops_illegal(X1),
	!,
	parser_error(W1,
		'Illegal token after primary expression').
	
pp_rt_rops(X1,XR, Ans,LPrec, VT,Prec, [X1|XR],Ans).




rt_rops_illegal('('(_,_,_)).
rt_rops_illegal('['(_,_,_)).
rt_rops_illegal('{'(_,_,_)).
rt_rops_illegal(integer(_,_,_)).
rt_rops_illegal(float(_,_,_)).
rt_rops_illegal(string(_,_,_)).
rt_rops_illegal(var(_,_,_)).

possible_binary_operator(symbol(F,_,_),F).
possible_binary_operator(functor(F,_,_),F).
possible_binary_operator(','(F,_,_),F).
possible_binary_operator('|'(F,_,_),F).
possible_binary_operator('!'(F,_,_),F).
possible_binary_operator(';'(F,_,_),F).

rt_check_binop_conflict(P1,P2,Tok) :-
	P1 =< P2,
	!.
rt_check_binop_conflict(_,_,Tok) :-
	parser_error(Tok,'Associativity conflict to left of infix operator').

rt_check_postop_conflict(P1,P2,Tok) :-
	P1 =< P2,
	!.
rt_check_postop_conflict(_,_,Tok) :-
	parser_error(Tok,'Associativity conflict to left of postfix operator').

rt_nexttok([X1|XR],X1,XR).

pp_rt_nexttok([lineinfo(_,_,_)|XR],Y1,YR) :-
	!,
	pp_rt_nexttok(XR,Y1,YR).
pp_rt_nexttok([X1|XR],X1,XR).

%%
%% rt_preop_ok(W1,WR,X1,XR)
%%
%%	Succeeds if token after prefix operator is permissible
%%

rt_preop_ok(lineinfo(_,_,_),[X1|XR],W1,WR) :-
	!,
	rt_preop_ok(X1,XR,W1,WR).
rt_preop_ok(X1,_,_,_) :-
	rt_preop_stopper(X1),
	!,
	fail.
rt_preop_ok(X1,XR,X1,XR).

%%
%% rt_preop_stopper(Tok)
%%
%%	Succeeds when the token after the found prefix operator indicates
%%	that the operator should be considered as a symbol.
%%

rt_preop_stopper(')'(_,_,_)).
rt_preop_stopper(']'(_,_,_)).
rt_preop_stopper('}'(_,_,_)).
rt_preop_stopper(fullstop(_,_,_)).
rt_preop_stopper(end_of_file(_,_,_)).
rt_preop_stopper(T) :-			%% prefix operator comes before
	possible_binary_operator(T,F),	%% binary operator
	binop(F,_,_,_).


%%%%%%%%%%
%%%%%%%%%%

/*
 * vlookup(Name,VT,Var)
 *
 *	Looks up variable with name Name in the variable table VT, adds it
 *	if it is not there and unifies Var with the variable found in the
 *	table.  Note that this implementation is not very efficient.   It takes
 *	linear time (wrt the number of variables in the clause) to lookup or
 *	add a variable.   Since the number of variables in any given clause
 *	is not that large, however, it is not too bad.
 */

vlookup('_',VT,V) :-
	!,
	arg(1,VT,L),
	mangle(1,VT,[['_'|dat(V,single)]|L]).
vlookup(Name,VT,V) :-
	arg(1,VT,VL),
	member([Name|DAT],VL),
	arg(1,DAT,V),
	mangle(2,DAT,multiple),
	!.
vlookup(Name,VT,V) :-
	arg(1,VT,L),
	mangle(1,VT,[[Name|dat(V,single)]|L]).


/*
 * binop(Operator,Priority,LeftPri,RightPri)
 *
 *	binop/4 is the interal form in which binary operators are stored in.
 *	Storing them in the xfx, yfx, xfy format would lead to too much
 *	computation regarding the left and right precedences.  So we store
 *	them precomputed.
 *
 *	Operator is the symbol representing the name of the operator.
 *	Priority is the base priority of the operator.
 *	LeftPri is the priority below or equal to which expressions may
 *		appear to the left of the operator.
 *	RightPri is the priority below or equal to which expressions may
 *		appear on the right hand side.
 */

binop( (:-),	1200,	1199,	1199).
binop( (-->),	1200,	1199,	1199).
binop( (;),	1100,	1099,	1100).
binop( (|),	1100,	1099,	1100).
binop( (->),	1050,	1049,	1050).
binop( (,),	1000,	999,	1000).

binop( .,	800,	799,	800).
binop( =,	700,	699,	699).
binop( \=,	700,	699,	699).
binop( ==,	700,	699,	699).
binop( \==,	700,	699,	699).
binop( @<,	700,	699,	699).
binop( @=<,	700,	699,	699).
binop( @>,	700,	699,	699).
binop( @>=,	700,	699,	699).
binop( =..,	700,	699,	699).
binop( is,	700,	699,	699).
binop( =:=,	700,	699,	699).
binop( =\=,	700,	699,	699).
binop( <,	700,	699,	699).
binop( =<,	700,	699,	699).
binop( >,	700,	699,	699).
binop( >=,	700,	699,	699).

binop( +,	500,	500,	499).
binop( -,	500,	500,	499).
binop( /\,	500,	500,	499).
binop( \/,	500,	500,	499).
binop( xor,	500,	500,	499).

binop( *,	400,	400,	399).
binop( /,	400,	400,	399).
binop( //,	400,	400,	399).
binop( div,	400,	400,	399).
binop( rem,	400,	400,	399).
binop( mod,	400,	400,	399).
binop( <<,	400,	400,	399).
binop( >>,	400,	400,	399).

binop( **,	200,	199,	199).

%%
%% Nonstandard infix operators
%%

%%binop( ::,	700,	699,	699).			%% CLP
binop( :,	950,	949,	950).
binop( ^,	200,	199,	200).


/*
 * preop(Operator, Priority, RightPri)
 *
 *	preop/3 is the internal form in which prefix operators are stored in.
 *
 *	Operator is the symbol representing the operator
 *	Priority is the base priority of the operator
 *	RightPri is the priority of terms which may be to the right of the
 *		 operator.
 */

preop( (:-),	1200,	1199).
preop( (?-),	1200,	1199).
preop( -,	200,	200).
preop( +,	200,	200).
preop( \,	200,	200).

%%
%% Nonstandard prefix operators
%%
preop( export,	1200,	1199).
preop( use,	1200,	1199).
preop( module,	1200,	1199).


/*
 * postop(Operator, Priority, LeftPri)
 *
 *	postop/3 is the internal form in which postfix operators are stored
 *		in.
 *
 *	Operator is the symbol representing the operator
 *	Priority is the base priority of the operator
 *	LeftPri	is the priority of terms which may be to the left of the 
 *		operator
 *
 * Note: There is no initial set of postfix operators, but we will initially
 *	 define a couple for testing purposes.
 */

postop( po1,	500,	500).
postop( po2,	500,	499).
postop( po3,	400,	400).
postop( po4,	400,	399).
postop( po5,	200,	200).
postop( po6,	200,	199).


/*
 * op(Priority,Op_specifier,Operator)
 *
 * Adds or deletes an operator from the set of operators.
 */

export op/3.
				
op(Priority,Specifier,Operator) :-
	%% FIXME:  Call curmod.  But be careful to have curmod defined
	%%	before op is first called.
	OPCall = (sio:op(Priority,Specifier,Operator)),
	integer_ok(Priority),
	atom_ok(Specifier),
	(   0 =< Priority,
	    Priority =< 1200,
	    !
	;   domain_error(operator_priority,Priority,OPCall)),
	(   is_op_specifier(Specifier,OpType,LeftAdj,RightAdj),
	    !
	;   domain_error(operator_specifier,Specifier,OPCall)),
	(   atom(Operator), Operator \= []
	->  OpList = [Operator]
	;   op_atom_list_ok(Operator,OPCall), OpList = Operator),
	(   dmember(',',OpList)
	->  permission_error(modify,operator,',')
	;   true),
	op(OpList, Priority, Specifier, OpType, LeftAdj, RightAdj, OPCall).

op([], Priority, Specifier, OpType, LeftAdj, RightAdj, OPCall) :-
	!.
op([Operator | Ops], Priority, Specifier, OpType, LeftAdj, RightAdj, OPCall) :-

	%%
	%% FIXME: Add an operator conflict testing mechanism here
	%%

	%% Call the old version of op in order to add operators to old table
	pbi_op(Priority,Specifier,Operator),
	op0(Priority,OpType,LeftAdj,RightAdj,Operator),
	op(Ops, Priority, Specifier, OpType, LeftAdj, RightAdj, OPCall).

op_atom_list_ok(Var, Call) :-
	var(Var),
	!,
	instantiation_error(Call).
op_atom_list_ok([], _) :-
	!.
op_atom_list_ok([V | T], Call) :-
	var(V),
	!,
	instantiation_error(Call).
op_atom_list_ok([H | T], Call) :-
	atom(H),
	H \= [],
	!,
	op_atom_list_ok(T, Call).
op_atom_list_ok([Culprit | T], Call) :-
	!,
	type_error(atom, Culprit, Call).
op_atom_list_ok(Culprit, Call) :-
	type_error(list, Culprit).

op0(0,OpType,_,_,Operator) :-
	!,
	remove_op(OpType,Operator).
op0(Priority,OpType,LeftAdj,RightAdj,Operator) :-
	!,
	remove_op(OpType,Operator),
	LeftPri is Priority-LeftAdj,
	RightPri is Priority-RightAdj,
	add_op(OpType,Operator,Priority,LeftPri,RightPri).

remove_op(infix,Operator) :-
	retract(binop(Operator,_,_,_)),
	!.
remove_op(prefix,Operator) :-
	retract(preop(Operator,_,_)),
	!.
remove_op(postfix,Operator) :-
	retract(postop(Operator,_)),
	!.
remove_op(_,_).


add_op(infix,Operator,Priority,LeftPri,RightPri) :-
	addclause(sio,binop(Operator,Priority,LeftPri,RightPri)),
	at_load_time.
add_op(prefix,Operator,Priority,LeftPri,RightPri) :-
	addclause(sio,preop(Operator,Priority,RightPri)),
	at_load_time.
add_op(postfix,Operator,Priority,LeftPri,RightPri) :-
	addclause(sio,postop(Operator,Priority,LeftPri)),
	at_load_time.


is_op_specifier(xfx,infix,1,1).
is_op_specifier(xfy,infix,1,0).
is_op_specifier(yfx,infix,0,1).
is_op_specifier(fx,prefix,0,1).
is_op_specifier(fy,prefix,0,0).
is_op_specifier(xf,postfix,1,0).
is_op_specifier(yf,postfix,0,0).


/*
 * current_op(Priority,Op_specifier,Operator)
 *
 */

export current_op/3.

current_op(Priority,Specifier,Operator) :-
	binop(Operator,Priority,LeftPri,RightPri),
	binop_specifier(Priority,LeftPri,RightPri,Specifier).
current_op(Priority,Specifier,Operator) :-
	preop(Operator,Priority,RightPri),
	preop_specifier(Priority,RightPri,Specifier).
current_op(Priority,Specifier,Operator) :-
	postop(Operator,Priority,LeftPri),
	postop_specifier(Priority,LeftPri,Specifier).

binop_specifier(Priority,SubPri,SubPri,xfx) :-
	SubPri < Priority,
	!.
binop_specifier(Pri,Pri,_,yfx) :-
	!.
binop_specifier(_,_,_,xfy).

preop_specifier(Pri,Pri,fy) :- !.
preop_specifier(_,_,fx).

postop_specifier(Pri,Pri,yf) :- !.
postop_specifier(_,_,xf).


/*
 * pp_flatten is given a term produced by the positional parser.  It returns
 * as its second argument, the term with all positional information removed.
 * I.e, it will return what we would have read if we had called the normal
 * parser instead of the positional parser.
 */

export pp_flatten/2.

pp_flatten(atom(Atom,_,_),Atom) :- !.
pp_flatten(var(Var,_,_,_),Var) :- !.
pp_flatten(list([H|T],_,_),[FH|FT]) :-
	!,
	pp_flatten(H,FH),
	pp_flatten_list(T,FT).
pp_flatten(struct(S,_,_),FS) :-
	!,
	functor(S,F,A),
	functor(FS,F,A),
	pp_flatten_struct(A,S,FS).
pp_flatten(end_of_file,end_of_file).


pp_flatten_list([],[]) :-
	!.
pp_flatten_list([H|T],[FH|FT]) :-
	!,
	pp_flatten(H,FH),
	pp_flatten_list(T,FT).
pp_flatten_list(T,FT) :-
	pp_flatten(T,FT).


pp_flatten_struct(A,S,FS) :-
	A > 0,
	!,
	PA is A-1,
	arg(A,S,Arg),
	pp_flatten(Arg,FArg),
	arg(A,FS,FArg),
	pp_flatten_struct(PA,S,FS).
pp_flatten_struct(_,_,_).

/*-------------------------------------------------------------------------------*
 * pp_xform_clause is given a term (clause) produced by the positional parser.
 * It returns as its third argument a clause suitably transformed with
 * positional information remaining for head and goals.  The remaining
 * positional information will take the form of goals which announce the
 * position after a head match and before each goal is executed.  Thus
 * a fact will be transformed into a rule with a single goal, that goal
 * being the goal announcing the fact that the head has been matched.
 * A rule with N goals will be transformed into a rule with 2N+1 goals,
 * the first goal announcing the fact that the head has successfully matched.
 * Each goal in the orignal clause will be preceded by a goal announcing
 * the file position of the upcoming original goal.
 *
 * It is the responsiblity of the debugger to handle these announcement
 * goals.   The form of a positional announcement goal for a head will be
 *
 *	'$dbg_aph'(ClauseGroupId,StartPos,EndPos)
 *
 * A goal will take a similar form except that the functor is '$dbg_apg'.
 *-------------------------------------------------------------------------------*/

export pp_xform_clause/3.

pp_xform_clause(end_of_file,_,end_of_file) :-
	!.
pp_xform_clause(struct((H :- B),_,_),CID, (XH :- '$dbg_aph'(CID,SH,EH),XB)) :-
	!,
	pp_flatten(H,XH),
	arg(2,H,SH),
	arg(3,H,EH),
	pp_xform_body(B,CID,XB).
pp_xform_clause(H, CID, (XH :- '$dbg_aph'(CID,SH,EH))) :-
	!,
	arg(2,H,SH),
	arg(3,H,EH),
	pp_flatten(H,XH).

pp_xform_body(struct((G1,G2),_,_),CID,(XG1,XG2)) :-
	!,
	pp_xform_body(G1,CID,XG1),
	pp_xform_body(G2,CID,XG2).
pp_xform_body(struct((G1;G2),_,_),CID,(XG1;XG2)) :-
	!,
	pp_xform_body(G1,CID,XG1),
	pp_xform_body(G2,CID,XG2).
pp_xform_body(struct((G1->G2),_,_),CID,(XG1->XG2)) :-
	!,
	pp_xform_body(G1,CID,XG1),
	pp_xform_body(G2,CID,XG2).
pp_xform_body(G,CID,('$dbg_apg'(CID,SG,EG),XG)) :-
	arg(2,G,SG),
	arg(3,G,EG),
	pp_flatten(G,XG).

/*
 * Token Preprocessor
 *
 *	The following predicates are prefixed with tp_ as they comprise
 *	the token preprocessor.
 */

export tp_get_token_list/2.

tp_get_token_list(Stream_or_alias,OutToks) :-
	%%
	%% The following two lines may be taken out if we can ensure that
	%% a valid stream is passed to us.
	%%
	is_stream(Stream_or_alias,Stream),
	is_input_stream(Stream),
	!,
	stream_token_list(Stream,TL),
	tp_process(TL,Stream,Toks,Remainder),
	tp_check_readiness(Remainder, Stream, Toks, OutToks).

%%
%% tp_check_readiness/4
%%
%% tp_check_readiness(Remainder, Stream, ToksIn, ToksOut)
%%

tp_check_readiness(stream_not_ready, Stream,Toks,
		   [symbol(stream_not_ready,0,0), fullstop(.,0,0)]) :-
	!,
	set_stream_token_list(Stream, Toks),
	stream_snr_action(Stream, SNRAction),
	tp_stream_not_ready(SNRAction, Stream).
tp_check_readiness(Remainder,Stream,OutToks,OutToks) :-
	set_stream_token_list(Stream, Remainder).

%%
%% tp_stream_not_ready/2
%%

tp_stream_not_ready(snr_code,_) :- !.
tp_stream_not_ready(error,Stream) :-
	existence_error(stream_not_ready, Stream, 2).

%%
%% tp_process/4
%%

tp_process(stream_not_ready, Stream, [], stream_not_ready) :-
	!.
tp_process([],Stream,OutToks,Remainder) :-
	!,
	get_token_list(Stream,TL),
	tp_process(TL,Stream,OutToks,Remainder).
tp_process([H|T],Stream,OutToks,Remainder) :-
	tp_process(H,T,Stream,OutToks,Remainder).


%%
%% tp_process/5
%%

tp_process(EndOfClause,XR,Stream,[EndOfClause|LR],Remainder) :-
	tp_is_endofclause(EndOfClause),
	tp_lineinfo(XR,LR,Remainder),
	!.
tp_process(preproc(_,_,_),Toks0,Stream,OutToks,Remainder) :-
	!,
	tp_preproc(Toks0,Stream,Toks1),
	tp_process(Toks1,Stream,OutToks,Remainder).
tp_process(Token,Tokens,Stream,[Token|OutToks],Remainder) :-
	tp_process(Tokens,Stream,OutToks,Remainder).

%%
%% tp_lineinfo/3
%%

tp_lineinfo([LineInfo|Remainder],[LineInfo],Remainder) :-
	functor(LineInfo,lineinfo,3),
	!.
tp_lineinfo(Remainder,[],Remainder).


%%
%% tp_is_endofclause/1
%%
%%	Those tokens which indicate the end of a clause
%%

tp_is_endofclause(end_of_file(_,_,_)).
tp_is_endofclause(fullstop(_,_,_)).
tp_is_endofclause(lexerr(_,_,_)).

%%
%% tp_preproc/3
%%
%%	Handle preprocessor directives
%%

tp_preproc( [symbol(include,_,_), string(NameString,_,_), eoln(_,_,_) | Toks],
	    Stream, OutToks ) :-
	!,
	atom_codes(Name,NameString),
	tp_include(Name,Toks,OutToks).
tp_preproc( [symbol(if,_,_) | Toks], Stream, OutToks) :-
	!,
	tp_increment_ifdef_nesting_counter(Stream),
	tp_if(Toks,Stream,OutToks).
tp_preproc( [symbol(Elif_or_else,_,_) | Toks], Stream, OutToks) :-
	tp_elif_or_else(Elif_or_else),
	!,
	tp_skip_to_endif(Toks,Stream,OutToks),
	tp_decrement_ifdef_nesting_counter(Stream).
tp_preproc( [symbol(endif,_,_) | Toks], Stream, OutToks) :-
	!,
	tp_to_eoln(Toks,_,_,OutToks).
tp_preproc( InToks, Stream, OutToks) :-
	tp_to_eoln(InToks,_,_,OutToks),
	als_advise('Warning: Did not understand preprocessor directive.\n',[]).

%%
%% tp_include/3, tp_include/4
%%
%%	Load an include file
%%

tp_include(Name,Hole,OutToks) :-
	catch(open(Name,read,Stream), _, fail),
	!,
	get_token_list(Stream,TokL0),
	tp_include(TokL0,Stream,Hole,OutToks),
	close(Stream).
tp_include(Name,Hole,Hole) :-
	als_advise('Warning: Could not include file \"%s\".\n',[Name]).

tp_include([],Stream,Hole,OutToks) :-
	!,
	get_token_list(Stream,Toks),
	tp_include(Toks,Stream,Hole,OutToks).
tp_include([end_of_file(_,_,_)|_],Stream,Hole,Hole) :- 
	!.
tp_include([Tok|Toks0],Stream,Hole,[Tok|Toks1]) :-
	tp_include(Toks0,Stream,Hole,Toks1).


%%
%% tp_to_eoln/4
%%
%%	Consume remainder of preprocessor directive
%%

tp_to_eoln([eoln(_,_,_) | Rest], Hole, Hole, Rest) :- !.
tp_to_eoln([H|T0], Hole, [H|T1], Rest) :-
	tp_to_eoln(T0,Hole,T1,Rest).


%%
%% tp_if/3
%%
%%	deal with #if statement
%%	-- the call to rt_readclause/5 here is to read the term governed
%%	by the #if (in order to tp_eval_if it), and so we don't need the
%%	returned FinalToks here.
%%

tp_if(Toks0,Stream,OutToks) :-
	tp_to_eoln(Toks0, [fullstop('.',-1,0)],[EvTok|EvToks],Toks1),
	rt_readclause(EvTok,EvToks,vt([]),EvTerm,_),
	tp_eval_if(EvTerm,Stream),
	!,
	Toks1 = OutToks.
tp_if(Toks,Stream,OutToks) :-
	tp_if_false(Toks,Stream,OutToks).

%%
%% tp_eval_if/2
%%
%%	Evaluates an expression obtained from an if or elif
%%

tp_eval_if(defined(D),Stream) :-
	!,
	do_later.	%% need to look up D in the define database
tp_eval_if(EvTerm,Stream) :-
	call(EvTerm),
	!.

%%
%% tp_if_false(Toks,Stream,OutToks)
%%
%%	skip over body of #if (or #elif) code and look for #elif, #else, or
%%		#endif
%%

tp_if_false([],Stream,OutToks) :-
	!,
	get_token_list(Stream,Toks),
	tp_if_false(Toks,Stream,OutToks).
tp_if_false([H|T],Stream,OutToks) :-
	tp_if_false(H,T,Stream,OutToks).


%%
%% tp_if_false(Tok,Toks,Stream,OutToks)
%%

tp_if_false(preproc(_,_,_),[symbol(Sym,_,_)|T],Stream,OutToks) :-
	!,
	tp_if_false0(Sym,T,Stream,OutToks).
tp_if_false(end_of_file(A,B,C),_,Stream,[end_of_file(A,B,C)]) :-
	!,
	als_advise('Warning: Premature end-of-file encountered before #elif, #else, or #endif directive\n').
tp_if_false(_,T,Stream,OutToks) :-
	tp_if_false(T,Stream,OutToks).

%%
%% tp_if_false0(Tok,Toks,Stream,OutToks)
%%
%%	Decode preprocessor directives when skipping
%%

tp_if_false0(If,Toks0,Stream,OutToks) :-
	tp_if_or_ifdef(If),
	!,
	tp_skip_to_endif(Toks0,Stream,Toks1),
	tp_if_false(Toks1,Stream,OutToks).
tp_if_false0(elif,Toks,Stream,OutToks) :-
	!,
	tp_if(Toks,Stream,OutToks).
tp_if_false0(else,Toks,Stream,OutToks) :-
	!,
	tp_to_eoln(Toks,_,_,OutToks).
tp_if_false0(endif,Toks,Stream,OutToks) :-
	!,
	tp_decrement_ifdef_nesting_counter(Stream),
	tp_to_eoln(Toks,_,_,OutToks).

%%
%% tp_if_or_ifdef(Sym)
%%

tp_if_or_ifdef(if).
tp_if_or_ifdef(ifdef).
tp_if_or_ifdef(ifndef).


%%
%% tp_elif_or_else(Sym)
%%

tp_elif_or_else(elif).
tp_elif_or_else(else).


%%
%% tp_skip_to_endif/3
%%

tp_skip_to_endif([],Stream,OutToks) :-
	!,
	get_token_list(Stream,Toks),
	tp_skip_to_endif(Toks,Stream,OutToks).
tp_skip_to_endif([end_of_file(A,B,C)],Stream,[end_of_file(A,B,C)]) :-
	!.
tp_skip_to_endif([preproc(_,_,_),symbol(If,_,_)|T0],Stream,OutToks) :-
	tp_if_or_ifdef(If),
	!,
	tp_skip_to_endif(T0,Stream,T1),
	tp_skip_to_endif(T1,Stream,OutToks).
tp_skip_to_endif([preproc(_,_,_),symbol(endif,_,_)|T],Stream,OutToks) :-
	!,
	tp_to_eoln(T,_,_,OutToks).
tp_skip_to_endif([H|T],Stream,OutToks) :-
	!,
	tp_skip_to_endif(T,Stream,OutToks).

%%
%% tp_decrement_ifdef_nesting_counter(Stream)
%% tp_increment_ifdef_nesting_counter(Stream)
%%
%%	Increments or decrements the nesting counter for ifdefs
%%

tp_decrement_ifdef_nesting_counter(Stream).	%% define later
tp_increment_ifdef_nesting_counter(Stream).	%% define later


endmod.

module builtins.

export '$dbg_apg'/3.
export '$dbg_aph'/3.

'$dbg_apg'(_,_,_).
'$dbg_aph'(_,_,_).

endmod.
