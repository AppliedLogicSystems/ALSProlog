/*=====================================================================
 |		blt_std.pro
 |	Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	Standard builtin predicates not fitting any other categories
 |
 |	Authors: Kevin A. Buettner, Ken Bowen, Chris White,
 |	         Keith Hughes, Ilyas Cicekli
 |	Original Creation Date: 3/20/86
 *====================================================================*/

module builtins.

/*
 * listing/0 and listing/1.
 */

export listing/0.
export listing/1.

listing :-
	clauses_for_listing(_:_/_,DBref),
	'$source'(DBref, Structure),
	write_out(Structure),
	fail.
listing.

listing(X) :-
	clauses_for_listing(X,DBref),
	'$source'(DBref, Structure),
	write_out(Structure),
	fail.
listing(X).  

export xlisting/1.
xlisting(X) :-
	clauses_for_listing(X,DBref),
	'$source'(DBref, Structure,show_pp),
	write_out(Structure),
	fail.
xlisting(X).  

%%%%------ listing support predicates ----------

clauses_for_listing(M:P/A,DBref) :-
	var(M),
	!,
	procedures(M,P,A,First),
	showmod(M),
	nonsemi(P),
	nl,write('% '),write(M:P/A),
	check_exported(M,P,A),nl,
	clauses(First,DBref).

clauses_for_listing(M:P/A,DBref) :-
	nonvar(M),
	!,
	procedures(M,P,A,First),
	nonsemi(P),
	nl,write('% '),write(M:P/A),
	check_exported(M,P,A),nl,
	clauses(First,DBref).

clauses_for_listing(P/A,DBref)   :-
	!,
	clauses_for_listing(_:P/A,DBref).

clauses_for_listing(P,DBref)     :-
	!,
	clauses_for_listing(_:P/_,DBref).
 

check_exported(M,P,A) :-
	'$exported_proc'(M,P,A),!,
	write('--exported').
check_exported(_,_,_).

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
