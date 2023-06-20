/*=====================================================================
 |			blt_flgs.pro
 |		Copyright (c) 1995-1996 Applied Logic Systems, Inc.
 |
 |	Builtin predicates for manipulating the Prolog flags
 |		[Extracted from blt_db.pro and extended]
 |
 |	Authors: Kevin A. Buettner, Ken Bowen
 |	Date: 5/96
 *====================================================================*/


module builtins.

export current_prolog_flag/2.
current_prolog_flag(Flag, Value) 
	:-
	var_or_atom_ok(Flag),
	pget_PROLOG_flag(Flag, Value).

export set_prolog_flag/2.
set_prolog_flag(Flag, Value) 
	:-
	do_set_prolog_flag(Flag, Value),
	check_for_alsdev_impact(Flag, Value).

do_set_prolog_flag(Flag, Value) 
	:-
	atom_ok(Flag),
	(get_PROLOG_flag(Flag, _) -> true
		;   
		domain_error(prolog_flag, Flag, 1) 
	),
	(prolog_flag_value_check(Flag, Value) -> true
		;   
		domain_error(flag_value, Flag+Value, 1) 
	),
	apply_requisites(Flag,Value),
	!,
	set_PROLOG_flag(Flag, Value).

init_prolog_flags 
	:-
	make_hash_table('_PROLOG_flag'),
	set_default_flags.

set_default_flags
	:-
	default_prolog_flag_value(Flag, Value),
	(Flag == heap_word_bytes -> 
		als_system(L), member(heapWordBytes = Value, L)
		;
		true
	),
	(Flag == address_bits -> 
		als_system(L), 
		member(heapWordBytes = VV, L), 
		Value is VV * 8
		;
		true
	),
	set_PROLOG_flag(Flag, Value),
	fail.
set_default_flags.


/*--------------------------------------------------------------------*
 |	prolog_flag_value_check/2
 |	prolog_flag_value_check(FlagName, Value)
 |	prolog_flag_value_check(?, ?)
 |
 |	prolog_flag_value_check/2 defines the Prolog flags and their values.
 |
 |	default_prolog_flag_value/2
 |	default_prolog_flag_value(FlagName, Value)
 |	default_prolog_flag_value(?, ?)
 |
 |	For those flags whose value is set (here) at the prolog level,
 |	the initial (default) value is given by default_prolog_flag_value/2.
 *--------------------------------------------------------------------*/
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%	Flags defining integer type I (7.11.1)
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

max_iv(134217727).   %% 2^28-1 

	%%---------------------------------
	%%	bounded  (7.11.1.1)
	%%---------------------------------
prolog_flag_value_check(bounded, true).
prolog_flag_value_check(bounded, false).

default_prolog_flag_value_check(bounded, true).
changeable(bounded, no).

	%%---------------------------------
	%%	max_integer  (7.11.1.2)
	%%---------------------------------
default_prolog_flag_value(max_integer, Value)
	:-
	max_iv(Value).
changeable(max_integer, no).

	%%---------------------------------
	%%	min_integer  (7.11.1.3)
	%%---------------------------------
default_prolog_flag_value(min_integer, Value)
	:-
	max_iv(MaxValue),
	Value is - (MaxValue + 1).
changeable(min_integer, no).


	%%---------------------------------
	%%	integer_rounding_function  (7.11.1.4)
	%%---------------------------------
prolog_flag_value_check(integer_rounding_function, down).  
prolog_flag_value_check(integer_rounding_function, toward_zero).
default_prolog_flag_value(integer_rounding_function, toward_zero).
changeable(integer_rounding_function, no).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%	Other Flags (7.11.2)
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%---------------------------------
	%%	char_conversion  (7.11.2.1)
	%%---------------------------------
prolog_flag_value_check(char_conversion, on).  
prolog_flag_value_check(char_conversion, off).  
	%% should be: default_prolog_flag_value(char_conversion, on).
default_prolog_flag_value(char_conversion, off).
changeable(char_conversion, yes).

	%%---------------------------------
	%%	debug (7.11.2.2)
	%%---------------------------------
prolog_flag_value_check(debug, off).  
prolog_flag_value_check(debug, on).  
default_prolog_flag_value(debug,off).
changeable(debug, yes).

	%%---------------------------------
	%%	max_arity (7.11.2.3)
	%%---------------------------------
default_prolog_flag_value(max_arity, Value)
	:-
	max_iv(Value).
changeable(max_arity, no).

	%%---------------------------------
	%%	unknown		(7.11.2.4)
	%%	-- previously called:
	%%		undefined_predicate
	%%---------------------------------
prolog_flag_value_check(unknown, error).
prolog_flag_value_check(unknown, fail).
prolog_flag_value_check(unknown, warning).
prolog_flag_value_check(unknown, break).
default_prolog_flag_value(unknown, error).
changeable(unknown, yes).

% Old compatability stuff:
prolog_flag_value_check(undefined_predicate, Value) 
	:- 
	prolog_flag_value_check(unknown, Value).

	%%---------------------------------
	%%	double_quotes 	(7.11.2.5)
	%%---------------------------------
prolog_flag_value_check(double_quotes, chars).
prolog_flag_value_check(double_quotes, codes).
prolog_flag_value_check(double_quotes, atom).
default_prolog_flag_value(double_quotes, codes).
changeable(double_quotes, yes).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% ALS Extension Flags:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%---------------------------------
	%%	heap_word_bytes
	%%---------------------------------
prolog_flag_value_check(heap_word_bytes, HWB) :-
	als_system(L),
	!,
	member(heapWordBytes=HWB, L).
default_prolog_flag_value(heap_word_bytes, _).
changeable(heap_word_bytes, no).
	

	%%---------------------------------
	%%	address_bits
	%%---------------------------------
prolog_flag_value_check(address_bits, ABits) :-
	prolog_flag_value_check(heap_word_bytes, HWB),
	ABits is HWB * 8.
default_prolog_flag_value(address_bits, _).
changeable(address_bits, no).

	%%---------------------------------
	%%	windows_system
	%%
	%%	Returns value "nowins" if no
	%%  windowing system extension is
	%%  present; otherwise returns the
	%%  identifier of the windowing
	%%  extension
	%%---------------------------------

default_prolog_flag_value(windows_system, tcltk)
	:-
	all_procedures(user,tcleval,3,0), !.

	%% Keep this one BEFORE the one for X:
default_prolog_flag_value(windows_system, motif)
	:-
	all_procedures(user,motif60,10,0), !.

default_prolog_flag_value(windows_system, xwins)
	:-
	all_procedures(user,x10,3,0), !.

default_prolog_flag_value(windows_system, mswins)
	:-
	all_procedures(user,user322,6,0), !.

default_prolog_flag_value(windows_system, macwins)
	:-
	all_procedures(user,macos15,4,0), !.

default_prolog_flag_value(windows_system, nowins).

prolog_flag_value_check(windows_system, Which)
	:-
	clause(default_prolog_flag_value(windows_system,Which), _),
	!.
changeable(windows_system, no).

	%%---------------------------------
	%%	anonymous solution reporting
	%%---------------------------------
prolog_flag_value_check(anonymous_solutions, true).
prolog_flag_value_check(anonymous_solutions, false).

default_prolog_flag_value(anonymous_solutions, false).

changeable(anonymous_solutions, yes).

	%%---------------------------------
	%%	default syntax error behavior:
	%%---------------------------------
prolog_flag_value_check(syntax_errors, fail).
prolog_flag_value_check(syntax_errors, error).
prolog_flag_value_check(syntax_errors, quiet).
prolog_flag_value_check(syntax_errors, dec10).

default_prolog_flag_value(syntax_errors, error).

changeable(syntax_errors, yes).

	%%---------------------------------
	%%	location of generated obp files:
	%%---------------------------------
prolog_flag_value_check(obp_location, gic).
prolog_flag_value_check(obp_location, gis).
prolog_flag_value_check(obp_location, giac).
prolog_flag_value_check(obp_location, gias).
prolog_flag_value_check(obp_location, no_obp).

default_prolog_flag_value(obp_location, giac).

changeable(obp_location, yes).

	%%---------------------------------
	%%	freeze
	%%
	%%	true if freeze is available,
	%%  false otherwise.
	%%---------------------------------
prolog_flag_value_check(freeze, true).
prolog_flag_value_check(freeze, false).
default_prolog_flag_value(freeze, Value)
	:-
	(all_procedures(builtins,'$delay',4,_) ->
		Value = true
		; 
		Value = false
	).
changeable(freeze, no).

	%%-----------------------------------------------------
	%%    show_delay - control write[_term]/_:
	%% Options are:
	%%     -- true - prints the delay structure
	%%     -- false - prints _$_NNNN form
	%%-----------------------------------------------------

prolog_flag_value_check(show_delay, true).
prolog_flag_value_check(show_delay, false).
default_prolog_flag_value(show_delay, true).
changeable(show_delay, yes).
#if (all_procedures(syscfg,intconstr,0,_))
apply_requisites(show_delay,false)
	:-
	set_PROLOG_flag(show_interval, false).

#endif

	%%---------------------------------
	%%	constraints
	%%
	%%	true if constraints are available,
	%%  false otherwise.
	%%---------------------------------
prolog_flag_value_check(constraints, true).
prolog_flag_value_check(constraints, fail).
default_prolog_flag_value(constraints, Value)
	:-
	(all_procedures(_,'$iter_link_net',5,_) ->
		Value = true
		; 
		Value = fail
	).
changeable(constraints, no).

/*
:-syscfg:dynamic(instconstr/0).

%#if (syscfg:intconstr)

*/

#if (all_procedures(syscfg,intconstr,0,_))

	%%-----------------------------------------------------
	%% For CLP(BNR): 
	%%    iters_max_exceeded control;
	%% When max is exceeded, options are:
	%%     -- succeed (leaves network in place); 
	%%     -- fail (quiet; backtracking resets net);
	%%     -- warning (fails & issues warning; 
	%%                        backtracking resets net);
	%%     -- exception (backtracking resets net);
	%%-----------------------------------------------------

prolog_flag_value_check(iters_max_exceeded, succeed).
prolog_flag_value_check(iters_max_exceeded, fail).
prolog_flag_value_check(iters_max_exceeded, warning).
prolog_flag_value_check(iters_max_exceeded, exception).
default_prolog_flag_value(iters_max_exceeded, succeed).
changeable(iters_max_exceeded, yes).

	%%-----------------------------------------------------
	%% For CLP(BNR): 
	%%    show_interval - control write[_term]/_:
	%% Options are:
	%%     -- true - prints the interval
	%%     -- false - prints _$_NNNN form
	%%-----------------------------------------------------

prolog_flag_value_check(show_interval, true).
prolog_flag_value_check(show_interval, false).
default_prolog_flag_value(show_interval, true).
changeable(show_interval, yes).
apply_requisites(show_interval,true)
	:-
	set_PROLOG_flag(show_delay, true).

#endif

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% DEFAULT VALUE:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
apply_requisites(_,_).


	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Handle interations of flag
	%% changes with the alsdev
	%% environment:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(alsdev_running/0).
check_for_alsdev_impact(Flag, Value)
	:-
	alsdev_running,
	!,
	builtins:get_primary_manager(ALSIDEmgr),
	send(ALSIDEmgr, change_prolog_flag(Flag,Value)),
	((Flag = debug, Value = on) ->
		alsdev:check_reload_consults
		;
		true
	),
	((Flag = show_interval, Value = true) ->
		send(ALSIDEmgr, change_prolog_flag(show_delay,true))
		;
		true
	),
	((Flag = show_delay, Value = false) ->
		send(ALSIDEmgr, change_prolog_flag(show_interval,false))
		;
		true
	).

check_for_alsdev_impact(_, _).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% ACTUALLY INITIALIZE THE FLAGS:
	%%    Moved to init_prolog_shell in
	%%	blt_shl.pro because has problems
	%%  getting windows_system correct
	%%  when run here (?)
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- init_prolog_flags.

export changable_flags_info/1.
changable_flags_info(List)
	:-
	findall(Info, changable_flag_info(Info), List).

changable_flag_info([Flag,PossVals,Value])
	:-
	changeable(Flag, yes),
	findall(V, prolog_flag_value_check(Flag, V), PossVals),
	current_prolog_flag(Flag, Value).

export static_flags_info/1.
static_flags_info(List)
	:-
	findall([Flag,Value], 
			(changeable(Flag, no),
				current_prolog_flag(Flag,Value)), 
			List).

endmod.		%% blt_flts.pro: Prolog Flags Builtins File

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%% FIXUP rt_defaults:
	%%% This has to be done here, else we
	%%% can't load.....
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

module sio.

:- asserta(
	(rt_defaults(rt_opts(vt([]),ErrMech, linestuff(FinalToks,S,E))) :-
		current_prolog_flag(syntax_errors, ErrMech),!) ).

endmod.





