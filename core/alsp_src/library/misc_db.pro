/*=====================================================================
 | 			misc_db.pro		
 |	Copyright (c) 1990-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Various Prolog database utilities for the library
 *====================================================================*/

module builtins.

/*!---------------------------------------------------------------
 |	assert_all/1
 |	assert_all(ClauseList)
 |	assert_all(+)
 |	
 |	- asserts each clause on ClauseList in the current module
 |	
 |	If ClauseList is a list of clauses, asserts each of these clauses
 |	in the current module.
 *!--------------------------------------------------------------*/

assert_all(Module,ClauseList)
	:-
	assert_all_refs0(ClauseList, [Module], _).

/*!---------------------------------------------------------------
 |	assert_all0/2
 |	assert_all0(ClauseList,Module)
 |	assert_all0(+,+)
 |	
 |	- asserts each clause on ClauseList in module Module
 |	
 |	If ClauseList is a list of clauses, asserts each of these clauses
 |	in module Module.
 *!--------------------------------------------------------------*/

assert_all0([],_).
assert_all0([Clause | RestClauseList],Module)
	:-
	builtins:'$assertz'(Module,Clause),
	assert_all0(RestClauseList,Module).

assert_all_refs(Module,ClauseList, RefsList)
	:-
	assert_all_refs0(ClauseList,[Module], RefsList).

assert_all_refs0([],_,[]).

assert_all_refs0([nl | RestClauseList],Modules, RestRefs)
	:-!,
	assert_all_refs0(RestClauseList,Modules,RestRefs).

assert_all_refs0([endmod | RestClauseList],[_ | Modules], RestRefs)
	:-!,
	assert_all_refs0(RestClauseList,Modules,RestRefs).

assert_all_refs0([(module WhichMod) | RestClauseList], Modules, RestRefs)
	:-!,
	assert_all_refs0(RestClauseList,[WhichMod | Modules],RestRefs).

assert_all_refs0([(':-' Goal) | RestClauseList], Modules, RestRefs)
	:-!,
	Modules = [Mod | _],
	Mod:call(Goal),
	assert_all_refs0(RestClauseList,Modules,RestRefs).

assert_all_refs0([Clause | RestClauseList],Modules, [Ref | RestRefs])
	:-
	Modules = [Module | _],
	builtins:'$assertz3'(Module,Clause,Ref),
	assert_all_refs0(RestClauseList,Modules,RestRefs).

erase_all([]).
erase_all([Ref | RefsList])
	:-
	erase(Ref),
	erase_all(RefsList).

/*
abolish_list([]).
abolish_list([P/A | More]) :-
	abolish(P,A),
	abolish_list(More).
*/
abolish_list(Module, List)
	:-
	abolish_list0(List, Module).

abolish_list0([], Module).
abolish_list0([P/A | More], Module) 
	:-
	Module:abolish(P,A),
	abolish_list0(More, Module).

retract_all(Module, Pattern)
	:-
	retract_all0(Pattern, Module).

retract_all0(Pattern, Module)
	:-
	retract_each(Pattern, Module),
	!,
	retract_all0(Pattern, Module).
retract_all0(_, _).
	
export retract_each/2.
retract_each(Module, Pattern)
	:-
	Module:retract(Pattern),
	fail.

retract_each(_, _).

endmod.
