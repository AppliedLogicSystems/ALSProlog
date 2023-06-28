/*=====================================================================
 |		blt_std.pro
 |	Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |
 |	Standard builtin predicates not fitting any other categories
 |
 |	Authors: Kevin A. Buettner, Ken Bowen, Chris White,
 |	         Keith Hughes, Ilyas Cicekli
 |	Original Creation Date: 3/20/86
 *====================================================================*/

module builtins.

/*------------------------------------*
 |	Support predicates for expanded
 |	DCG clauses:
 *------------------------------------*/
strip_module((M:G), M, G) :-!.
strip_module(G, _, G).

export dcg_phrase/3.
dcg_phrase(A, InVar, OutVar)
	:-
	strip_module(A, _, G),
	( (G = [_|_] ; G = []) ->
		append(G, OutVar, InVar)
		;
		call(A), 
		InVar = OutVar
	).

/*------------------------------------*
 |	listing/[0,1,2] and listing_to/1.
 |	dblisting/[1,2].
 *------------------------------------*/

export listing/0.
export listing/1.
export listing/2.
export listing_to/1.

listing :-
	sio:get_current_output_stream(Stream),
	listing_to(Stream).

listing_to(Stream) :-
	clauses_for_listing(_:_/_,DBref, Stream),
	'$source'(DBref, Structure),
	write_out(Stream, Structure),
	fail.
listing_to(_).

listing(X) 
	:-
	sio:get_current_output_stream(Stream),
	listing(X, Stream, hide_pp).

listing(X, Stream)
	:-
	listing(X, Stream, hide_pp).

listing(X, Stream, Mode)
	:-
	clauses_for_listing(X,DBref,Stream),
	'$source'(DBref, Structure, Mode),
	write_out(Stream, Structure),
	fail.
listing(_, _, _).  

export dblisting/1.
dblisting(X) :-
	sio:get_current_output_stream(Stream),
	listing(X, Stream, show_pp).


export dblisting/0.
dblisting :-
	sio:get_current_output_stream(Stream),
	dblisting_to(Stream).

dblisting_to(Stream) :-
	clauses_for_listing(_:_/_,DBref, Stream),
	'$source'(DBref, Structure, show_pp),
	write_out(Stream, Structure),
	fail.
dblisting_to(_).

export db_dump/1.
db_dump(Mod)
	:-
	sio:get_current_output_stream(Stream),
	procedures(Mod,P,A,_,0),
	clauses_for_listing(Mod:P/A,DBRef, Stream, show_pp),
	'$source'(DBRef, Structure, show_pp),
	write_out(Stream, Structure),
	fail.
db_dump(Mod).

%%%%------ listing support predicates ----------

clauses_for_listing(X, DBref)
	:-
	sio:get_current_output_stream(Stream),
	clauses_for_listing(X, DBref, Stream, hide_pp).

clauses_for_listing(MPA, DBref, Stream) :-
	clauses_for_listing(MPA, DBref, Stream, hide_pp).

clauses_for_listing(M:P/A, DBref, Stream, Mode) :-
	var(M),
	!,
	procedures(M,P,A,First),
	showmod(M),
	fin_clauses_for_listing(Mode,M,P,A,First,DBref,Stream).

clauses_for_listing(M:P/A, DBref, Stream, Mode) :-
	nonvar(M),
	!,
	procedures(M,P,A,First),
	fin_clauses_for_listing(Mode,M,P,A,First,DBref,Stream).

clauses_for_listing(P/A, DBref, Stream, Mode) :-
	!,
	clauses_for_listing(_:P/A, DBref, Stream, Mode).

clauses_for_listing(P, DBref, Stream, Mode) :-
	!,
	clauses_for_listing(_:P/_, DBref, Stream, Mode).
 
fin_clauses_for_listing(hide_pp,M,P,A,First,DBref,Stream)
	:-
	nonsemi(P),
	printf(Stream, '\n%% %t',[M:P/A]), 
	check_exported(M,P,A,Stream),
	nl(Stream),
	clauses(First,DBref).

fin_clauses_for_listing(show_pp,M,P,A,First,DBref,Stream)
	:-
	printf(Stream, '\n%% %t',[M:P/A]), 
	check_exported(M,P,A,Stream),
	nl(Stream),
	clauses(First,DBref).



check_exported(M,P,A, Stream) :-
	'$exported_proc'(M,P,A),
	!,
	write_term(Stream,'--exported',[]).

check_exported(_,_,_, Stream).

showmod(M) :-
	noshowmod(M),
	!,
	fail.
showmod(M).

/*
 * noshowmod gives the modules which we don't wish to see with listing.
 *		-- NEEDS TO BE COMBINED/COORDINATED WITH DEBUGGER.PRO
 */

noshowmod(builtins).
noshowmod(sio).
noshowmod(syscfg).
noshowmod(xconsult).
noshowmod(global_gv_info).
noshowmod(alsdev).
noshowmod(alsshell).
noshowmod(debugger).
noshowmod(rel_arith).
noshowmod(pgm_info).
noshowmod(sys_maint).
%noshowmod(object_classes).
noshowmod(utilities).
noshowmod(windows).
noshowmod(tcltk).
noshowmod(tk_alslib).
noshowmod(curl).

nonsemi(P) :-
	isgensym(semi,P),
	!,
	fail.
nonsemi(P).


/*
 * statistics/2:     Quintus compatability
 */

export statistics/2.

statistics(runtime,[Total,SinceLast]) :-
	Total is cputime,			%% used to be * 1000
	retract(statistics_runtime_total(OldTotal)),
	!,
	assert(statistics_runtime_total(Total)),
	SinceLast is Total-OldTotal.

statistics_runtime_total(0).

/*
 * instance/2
 *
 *   instance(Ref,Clause)
 *
 */

export instance/2.

instance(Ref,(H :- B)) :- 
	'$source'(Ref,Clause), 
	clauseParts(Clause,H,B).


/*
 * '$getenv'/2:		Compatiblity with older versions of ALS-Prolog
 *
 *	We now use getenv/2 and encourage its use over $getenv.
 *	$getenv/2 will accept a list as the first arg and return
 *	a list (always) as the second arg.  getenv/2 takes an atom
 *	as the first arg and returns an atom.
 */

export '$getenv'/2.

'$getenv'(Name,ValueAsList) :-
	atom(Name),
	!,
	getenv(Name,Value),
	atom_codes(Value,ValueAsList).
'$getenv'(NameAsList,ValueAsList) :-
	atom_codes(Name,NameAsList),
	'$getenv'(Name,ValueAsList).


endmod.		%% blt_std.pro:  Standard Builtins File
