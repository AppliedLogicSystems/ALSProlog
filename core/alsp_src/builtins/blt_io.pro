/*==============================================================
 |		blt_io.pro
 | Copyright (c) 1986-96 Applied Logic Systems, Inc.
 |
 | Builtin predicates for dealing with I/O
 |
 | Authors: Kevin A. Buettner, Ken Bowen, Chris White,
 |	    Keith Hughes, Ilyas Cicekli
 | Original Creation Date: 3/20/86
 | Merge3 Revision: Begun 3/6/92 - Ken Bowen
 *==============================================================*/

module builtins.
use xconsult.

/*---------------------------------------------------------------------------*
 * printf/1, printf/2, printf/3, printf/4
 *
 *	Enhanced version of C's printf.  Handles everything that C does
 *	with exception of *'s in formats.  Even this should be handled
 *	someday.  Other bug:  Need field widths and precisions in
 *	printing of terms.
 *
 *	
 *	Formats special to this version of printf:
 *
 *	%t	-- prints Prolog Term
 *	%p	-- calls arg as print procedure
 *
 *	Any other characters will be either printed as is or will be
 *	considered part of a C-style format.
 *
 *	printf/4 allows options (the 4th arg) to be passed to underlying
 *	calls to write_term/[]
 *---------------------------------------------------------------------------*/

%% FIXME:  Add type checking for ArgList and more type checking for Format

export printf/1.
export printf/2.
export printf/3.
export printf_opt/3.
export printf/4.

printf(Format) :- 
	current_output(Stream),
	printf_check_format(Format, FormatList),
	printf0(FormatList,Stream,[],[]),
	!.

printf(Format,ArgList) :- 
	current_output(Stream),
	printf_check_format(Format, FormatList),
	printf0(FormatList,Stream,ArgList,[]),
	!.

printf_opt(Format,ArgList,Options) :- 
	current_output(Stream),
	printf_check_format(Format, FormatList),
	printf0(FormatList,Stream,ArgList,Options),
	!.

printf(Stream_or_alias,Format,ArgList) :-
	sio:output_stream_or_alias_ok(Stream_or_alias, Stream),
	printf_check_format(Format, FormatList),
	printf0(FormatList,Stream,ArgList,[]).

printf(Stream_or_alias,Format,ArgList,Options) :-
	sio:output_stream_or_alias_ok(Stream_or_alias, Stream),
	printf_check_format(Format, FormatList),
	!,
	printf0(FormatList,Stream,ArgList,Options).

printf_check_format(Var, _) :-
	var(Var),
	!,
	instantiation_error(2).
printf_check_format(Atom, List) :-
	atom(Atom),
	!,
	atom_codes(Atom,List).
printf_check_format(FormatList, FormatList) :-
	FormatList = [_ | _],
	!.
printf_check_format(Culprit,_) :-
	type_error(list,Culprit,2).

printf0([],Stream,_,_) 
	:- !.

%	sio:tk_setmark(Stream).

%%%%		%t -- print Prolog term
printf0([0'%,0't |Format], Stream, [Term|ArgList],Options) :-
	!,
%	sio:write_term(Stream,Term,[quoted(false) | Options]),
	sio:write_term(Stream,Term,Options),
	printf0(Format,Stream,ArgList,Options).

%%%%		%p -- call print procedure as argument
printf0([0'%,0'p |Format], Stream, [PrintGoal|ArgList],Options)  
	:-!,
	(PrintGoal = Stream^PrintGoal0 ->
		call(PrintGoal0)
		;
		(PrintGoal = [Stream,Options]^PrintGoal0 ->
			call(PrintGoal0)
			;
			call(PrintGoal)
		)
		
	), 
	printf0(Format,Stream,ArgList,Options).

%%%%		%% -- write out single percent
printf0([0'%,0'% | Format], Stream, ArgList,Options) :-
	!,
	put_code(Stream,0'%),
	printf0(Format, Stream, ArgList,Options).

%%%%		%% -- special case newlines (quoted or not):
printf0([0'\n | Format], Stream, ArgList,Options) 
	:-!,
	(dmember(line_end(false), Options) ->
		put_code(Stream, 0'\\) ; true ),
	nl(Stream),
	printf0(Format, Stream, ArgList,Options).

%%%%		%k -- print interval
printf0([0'%,0'k |Format], Stream, [PrintItem|ArgList],Options)  
	:-!,
	printf_intv(Stream, '%f', PrintItem, Options),
	printf0(Format,Stream,ArgList,Options).

%%%%		Handle remaining formats:
printf0([0'% | Format], Stream, [Arg | ArgList],Options) 
	:-
	isformat(Format,CFormat,RestFormat,FormatStopper,EndSlot),
	!,
	disp_printf0(FormatStopper, CFormat, EndSlot, RestFormat, Stream, [Arg | ArgList],Options).

%%%%		%<WP>k -- print interval, with width/precision:
disp_printf0(0'k, KFormat, EndSlot, RestFormat, Stream, [Arg | ArgList],Options)
	:-!,
	EndSlot = 0'f,
	atomicize_arg([0'% | KFormat], AFormat),
	printf_intv(Stream, AFormat, Arg, Options),
	printf0(RestFormat,Stream,ArgList,Options).

%%%%		Pass all other formats to C (via sio_sprintf):
disp_printf0(FormatStopper, CFormat, EndSlot, RestFormat, Stream, [Arg | ArgList],Options)
	:-
	EndSlot = FormatStopper,
	atomicize_arg(Arg,AArg),
	atomicize_arg([0'% | CFormat], ACFormat),
	sio_sprintf(ACFormat,AArg,ArgBuf,_),
	put_atom(Stream,ArgBuf),
	printf0(RestFormat,Stream,ArgList,Options).

%%%%		Write any other character as is
printf0([Char |Format], Stream, ArgList,Options) 
	:-
	put_code(Stream,Char),
	printf0(Format,Stream,ArgList,Options).


isformat([Char | RestFormat],[End],RestFormat, Char, End) 
	:-
	isformatstopper(Char),
	!.
isformat([C | Cs], [C | Fs], RestFormat, Stopper, End) 
	:-
	isformat(Cs,Fs,RestFormat, Stopper, End).

isformatstopper(0'd).
isformatstopper(0'i).
isformatstopper(0'o).
isformatstopper(0'u).
isformatstopper(0'x).
isformatstopper(0'X).
isformatstopper(0'f).
isformatstopper(0'e).
isformatstopper(0'E).
isformatstopper(0'g).
isformatstopper(0'G).
isformatstopper(0'c).
isformatstopper(0's).
isformatstopper(0'k).

atomicize_arg(Arg,Arg) 
	:-
	atomic(Arg),
	Arg \= [],
	!.
atomicize_arg(List,Atom) 
	:-
	atom_codes(Atom,List),
	!.
atomicize_arg(_,bad_arg).

:-rel_arith:dynamic('$domain_term'/2).

printf_intv(Stream, Fmt, Item, Options)
	:-
	'$is_delay_var'(Item),
	'$delay_term_for'(Item, Var_DelayTerm),
	arg(4, Var_DelayTerm, ConstraintTerm),
	rel_arith:domain_term_from_constr(ConstraintTerm, DomainTerm),
	rel_arith:valid_domain(DomainTerm, Type, LArg, UArg),
	!,
	epsilon_show(Eps),
	Width is abs(UArg - LArg),
	(Width < Eps ->
		SPrt is (UArg + LArg)/2,
		sio_sprintf(Fmt, SPrt, Buf,_),
		put_atom(Stream, Buf)
		;
		SPrt = [LArg, UArg],
		sio_sprintf(Fmt, LArg, BufL,_),
		sio_sprintf(Fmt, UArg, BufU,_),
		put_code(Stream, 0'[),
		put_atom(Stream, BufL),
		put_code(Stream, 0',),
		put_atom(Stream, BufU),
		put_code(Stream, 0'])
	).

printf_intv(Stream, Fmt, Item, Options)
	:-
	put_atom(Stream, bad_arg).





/*
 *
 * sprintf(List,Format,Args)
 *
 *	Calls printf to put its output into List.
 */

export sprintf/3.
sprintf(Output,Format,Args) 
	:-
	sprintf(Output,Format,Args,[]).

export sprintf/4.
sprintf(Output,Format,Args,Opts) 
	:-
	nonvar(Output),
	!,
	(Output = atom(A) ->
		open(atom(A),write,Stream)
		;
		((Output = list(S) ; Output = string(S) ) ->
			open(string(S),write,Stream)
			;
			fail
		)
	),
	printf(Stream,Format,Args,Opts),
	close(Stream).

sprintf(Output,Format,Args,Opts) 
	:-
	open(string(Output),write,Stream),
	printf(Stream,Format,Args,Opts),
	close(Stream).

/*
 * bufwrite/2 and bufwriteq/2
 */

export bufwrite/2.
export bufwriteq/2.

bufwrite(String,Term) :-
	open(string(String),write,Stream),
	write_term(Stream,Term, [line_length(10000),quoted(false),
		maxdepth(20000), quoted_strings(false)]),
	close(Stream).

bufwriteq(String,Term) :-
	open(string(String),write,Stream),
	write_term(Stream,Term, [line_length(10000), quoted(true),
		maxdepth(20000), quoted_strings(false)]),
	close(Stream).

/*
 * old_bufread/2
 * bufread/2
 * bufread/3
 */

export old_bufread/2.
export bufread/2.
export bufread/3.

old_bufread(String,[Term|VarNames]) :- 
	open(string(String),read,Stream),
	read_term(Stream,Term,
		  [attach_fullstop(true),vars_and_names(_,VarNames)]),
	close(Stream).

bufread(String,Term) 
	:-
	bufread(String,Term,[]).

bufread(String,Term,Options) 
	:-
	open(string(String),read,Stream),
	read_term(Stream,Term,[attach_fullstop(true)|Options]),
	close(Stream).

export atomread/2.
export atomread/3.

atomread(Atom,Term) 
	:-
	atomread(Atom,Term,[]).

atomread(Atom,Term,Options) 
	:-
	open(atom(Atom),read,Stream),
	read_term(Stream,Term,[attach_fullstop(true)|Options]),
	close(Stream).

%
% Instantiate the variables in the ThawedTerm with their lexical
% names, and return the new term in FrozenTerm. The NameList, VarList,
% are assumed to be in the form provided by readvnv/3.
%

export varsubst/4.

varsubst(ThawedTerm,FrozenTerm,NameList,VarList) :-
	nonvar(NameList),
	nonvar(VarList),
	nonvar(ThawedTerm),
	subst(ThawedTerm,FrozenTerm,NameList,VarList), !.

subst(ThawedTerm,FrozenTerm,NameList,VarList) :-
	ThawedTerm =.. ThawedList,
	substList(ThawedList,FrozenList,NameList,VarList),
	FrozenTerm =.. FrozenList.


substList([],[],_,_) :- !.
substList([SubTerm|RestThawed],[NewSubTerm|RestFrozen],NameList,VarList) :-
	functor(SubTerm,Functor,Arity), Arity > 0,
	!,
	subst(SubTerm,NewSubTerm,NameList,VarList),
	substList(RestThawed,RestFrozen,NameList,VarList).
substList([SubTerm|RestThawed],[Name|RestFrozen],NameList,VarList) :-
	var(SubTerm), 
	member_identical(SubTerm,VarList,Name,NameList),
	!,
	substList(RestThawed,RestFrozen,NameList,VarList).
substList([SubTerm|RestThawed],[SubTerm|RestFrozen],NameList,VarList) :-
	substList(RestThawed,RestFrozen,NameList,VarList).

member_identical(Item,[H1|_],H2,[H2|_]) :-
	Item == H1,
	!.
member_identical(Item,[_|L1],Item2,[_|L2]) :-
	member_identical(Item,L1,Item2,L2).

endmod.		%% blt_io.pro: I/O Builtins File
