/*=====================================================================
 *	blt_ctl.pro
 *		Copyright (c) 1986-1992 Applied Logic Systems, Inc.
 *
 *	Builtin predicates for dealing with control
 *
 *	Authors: Kevin A. Buettner, Ken Bowen, Chris White,
 *	         Keith Hughes, Ilyas Cicekli
 *	Original Creation Date: 3/20/86
 *====================================================================*/

module builtins.

/*-----------------------------------------------------------------------*
 | findall/3, findall/4, setof/3, setof/4, bagof/3, bagof/4
 |
 | findall/3 and findall/4 replace $$bagof/3 and $$bagof/4 on the PC
 | version. findall/4 is used in the implementation of both setof and bagof.
 |
 | Note that mangle/3 is used for collecting the list of solutions.
 *-----------------------------------------------------------------------*/

:- 
%	compiletime,
	op(200,xfy,^),
	module_closure(findall,3,findall),
	module_closure(setof,3,setof),
	module_closure(bagof,3,bagof).

export findall/3.
export setof/3.
export bagof/3.

findall(Module,Template,Goal,Bag) :-
	nonvar(Goal),
	S = [[]],
	fa_(Module,Template,Goal,S),
	arg(1,S,Bag).

fa_(Module,Template,Goal,S) :-
	fa_call(Module,Goal),
	copy_term(Template,Copy),
	fa_add(S,Copy),
	fail.
fa_(_,_,_,_).


/*-----------------------------------------------------------------------*
 | fa_call(Module,Goal) calls the goal Goal in module Module.  It was
 | previously defined as
 |
 |	fa_call(Module,Goal) :- Module:Goal.
 |
 | It has been changed to the arcane form that it is in today so
 | that the debugger will stop when creeping through code involving
 | a findall, setof, or bagof.
 |
 | The "call" to 'als$cd/3 with 0 as the first argument directs our compiler
 | to unify the current cutpt in the clause with the last argument (in
 | this case CutPt).
 *-----------------------------------------------------------------------*/

fa_call(Module,Goal) :-
	'als$cd'(0,'$colon'(Module,Goal,CutPt),CutPt).

export copy_term/2.
copy_term(In,Out) :-
	fa_copy(In,Out,_).

fa_copy(VIn,VOut,VAssoc) :-
	var(VIn),
	!,
	fa_vassoc(VAssoc,VIn,VOut).
fa_copy(Atom,Atom,VAssoc) :-
	atomic(Atom),
	!.
fa_copy(SIn,SOut,VAssoc) :-
	functor(SIn,F,A),
	functor(SOut,F,A),
	fa_copyargs(A,SIn,SOut,VAssoc).

fa_copyargs(0,_,_,VAssoc) :-    
	!.
fa_copyargs(N,SIn,SOut,VAssoc) :-
	arg(N,SIn,AIn),
	arg(N,SOut,AOut),
	fa_copy(AIn,AOut,VAssoc),
	NP is N-1,
	fa_copyargs(NP,SIn,SOut,VAssoc).

fa_vassoc(T,VIn,VOut) :-         
	var(T),
	!,
	T = [VIn, VOut | _].
fa_vassoc([X, VOut | _], VIn, VOut) :-
	eq(X,VIn),
	!.
fa_vassoc([_, _ | More], VIn, VOut) :-
	fa_vassoc(More, VIn, VOut).


fa_add(S,E) :-              
	arg(1,S,[]),
	!,
	Cell = [E],
	mangle(1,S,Cell),
	mangle(2,S,Cell).
fa_add(S,E) :-
	arg(2,S,LE),
	Cell = [E],
	mangle(2,S,Cell),
	mangle(2,LE,Cell).


setof(Module,OutVars,P,Set) :-
	nonvar(P),
	bagof(Module,OutVars,P,Bag),
	sort(Bag,Set).

bagof(Module,OutVars,P,Bag) :-
	nonvar(P),
	excess_vars(P,OutVars,NewP,ExcessVars),
	bagof(ExcessVars,OutVars,NewP,Bag,Module).

bagof([],OutVars,P,Bag,Module) :-
	!,
	findall(Module,OutVars,P,Bag),
	Bag \= [].
bagof(ExcessVars,OutVars,P,Bag,Module) :-
	findall(Module,ExcessVars-OutVars,P,Bags0),
	keysort(Bags0,Bags),
	pick(Bags,ExcessVars,Bag).

retrieve(InterBag,FinalBag) :-
	retract('$'('$bag',OutVars)), !,
	retrieve1(OutVars,InterBag,FinalBag).

retrieve1(OutVars,InterBag,FinalBag) :-
	OutVars \= '$bag', !,
	retrieve([OutVars|InterBag],FinalBag).
retrieve1(_,FinalBag,FinalBag).

pick(Bags,ExcessKey,OneBag) :-
	Bags \= [],
	select(Bags,Key1,Bag1,RestBags),
	decide(Key1,Bag1,RestBags,ExcessKey,OneBag).

select([Key-OutVars|RestBags],Key,[OutVars|RestOutVars],LeftOverBags) :-
	!,
	select(RestBags,Key,RestOutVars,LeftOverBags).
select(LeftOverBags,Key,[],LeftOverBags).

decide(Key,Bag,Bags,Key,Bag) :- (Bags=[], ! ; true).
decide(_,_,Bags,Key,Bag) :- pick(Bags,Key,Bag).

excess_vars(Goal,OutVars,NewGoal,Excess) :-
	filterquant(Goal,OutVars,ModGoal,ModOutVars),
	excess_vars1(ModGoal,ModOutVars,NewGoal,Excess).

filterquant(Goal,OutVars,ModGoal,ModOutVars) :-
	quantifier_type(non_prefix),
	!,
	filterquant_goal(Goal,OutVars,ModGoal,ModOutVars).
filterquant(Goal,OutVars,ModGoal,ModOutVars) :-
	filterquant_prefix(Goal,OutVars,ModGoal,ModOutVars).

%% Quantifiers accepted throughout Goal:
filterquant_goal(Goal,OutVars,Goal,OutVars) :- var(Goal), !.
filterquant_goal(Goal,OutVars,Goal,OutVars) :- atomic(Goal), !.
filterquant_goal(X^Goal,OutVars,NewGoal,[X|NewOutVars]) :- !,
	filterquant_goal(Goal,OutVars,NewGoal,NewOutVars).
filterquant_goal([],OutVars,[],OutVars) :- !.
filterquant_goal([Goal|Rest],OutVars,[NewGoal|NewRest],NewOutVars) :- !,
	filterquant_goal(Goal,OutVars,NewGoal,TempOutVars),
	filterquant_goal(Rest,TempOutVars,NewRest,NewOutVars).
filterquant_goal(Goal,OutVars,NewGoal,NewOutVars) :-
	functor(Goal,F,N), N>0, !,
	Goal =.. [_|Args],
	filterquant_goal(Args,OutVars,NewArgs,NewOutVars),
	NewGoal =.. [F|NewArgs].

%% Quantifiers only accepted in prefix:
filterquant_prefix(Goal,OutVars,Goal,OutVars) :- var(Goal), !.
filterquant_prefix(Goal,OutVars,Goal,OutVars) :- atomic(Goal), !.
filterquant_prefix(X^Goal,OutVars,NewGoal,[X|NewOutVars]) :- !,
	filterquant_prefix(Goal,OutVars,NewGoal,NewOutVars).
filterquant_prefix(Goal,OutVars,Goal,OutVars).

%% Default is throughout Goal(options = non_prefix, prefix):
export quantifier_type/1.
quantifier_type(non_prefix).

export set_quantifier_type/1.
set_quantifier_type(Type) :-
	dmember(Type, [prefix,non_prefix]),
	!,
	retract(quantifier_type(_)),
	assert(quantifier_type(Type)).
set_quantifier_type(Type) :-
	prolog_system_error(bad_qnt,[Type]).

excess_vars1(setof(NewOutVars,PP,NewSet),OutVars,
			setof(NewOutVars,PP,NewSet),Excess) :-
	!,
	excess_vars1((PP,NewSet),(NewOutVars,OutVars),_,Excess).
excess_vars1(bagof(NewOutVars,PP,NewSet),OutVars,
			bagof(NewOutVars,PP,NewSet),Excess) :-
	!,
	excess_vars1((PP,NewSet),(NewOutVars,OutVars),_,Excess).
excess_vars1(Goal,Template,Goal,Excess) :-
	get_vars(Goal,[],Vars),
	get_vars(Template,[],OutVarList),
	filter(Vars,OutVarList,[],Excess).

%************ replaced excess vars to accomodate CHAT
% excess_vars(setof(NewOutVars,PP,NewSet),OutVars,
%     			setof(NewOutVars,PP,NewSet),Excess) :-
%     !,
%     excess_vars((PP,NewSet),(NewOutVars,OutVars),_,Excess).
% excess_vars(bagof(NewOutVars,PP,NewSet),OutVars, 
% 			bagof(NewOutVars,PP,NewSet),Excess) :-
%     !,
%     excess_vars((PP,NewSet),(NewOutVars,OutVars),_,Excess).
% 
% excess_vars(X^Goal,OutVars,NewGoal,Excess) :- !,
%	excess_vars(Goal,[X|OutVars],NewGoal,Excess).
% excess_vars(Goal,Template,Goal,Excess) :-
%     get_vars(Goal,[],Vars),
%     get_vars(Template,[],OutVarList),
%     filter(Vars,OutVarList,[],Excess).
%

export get_vars/3.
get_vars(V, VL, VL) :-
	var(V),
	already_in(V,VL),
	!.
get_vars(V, InVL, [V | InVL]) :-
	var(V),
	!.
get_vars(A, VL, VL) :-
	atomic(A),
	!.
get_vars(S, InVL, OutVL) :-
	functor(S,_,A),
	get_vars_in_structure(A,S,InVL,OutVL).

get_vars_in_structure(0, S, VL, VL) :- !.
get_vars_in_structure(N, S, InVL, OutVL) :-
	arg(N,S,Arg),
	get_vars(Arg, InVL, IntVL),
	PrevN is N-1,
	get_vars_in_structure(PrevN,S, IntVL, OutVL).

filter([],_,Filtered,Filtered).
filter([Var|Vars],Filter,Inter,Filtered) :-
	already_in(Var,Filter), !,
	filter(Vars,Filter,Inter,Filtered).
filter([Var|Vars],Filter,Inter,Filtered) :-
	filter(Vars,Filter,[Var|Inter],Filtered).


already_in(X,[H|_]) :- X == H.
already_in(H,[_|T]) :- already_in(H,T).


/*
 * keysort/2
 */

export keysort/2.

keysort(L,R) :- length(L,N), keysort(N,L,_,R).

keysort(2,[X1|L1],L,R) :- !,
	comprises(L1,X2,L),
	compare_keys(Delta,X1,X2),
	keysort1(Delta,X1,X2,R).

% keysort1(Delta, X1, X2, R)
keysort1( '>', X1, X2, [X2,X1]) :- !.
keysort1( _,   X1, X2, [X1,X2]).

keysort(1,[X|L],L,[X]) :- !.
keysort(0,L,L,[]) :- !.
keysort(N,L1,L3,R) :-
	N1 is N div 2, N2 is N-N1,
	keysort(N1,L1,L2,R1),
	keysort(N2,L2,L3,R2),
	keymerge(R1,R2,R).

keymerge([],R,R) :- !.
keymerge(R,[],R) :- !.
keymerge(R1,R2,[X|R]) :-
	comprises(R1,X1,R1a), comprises(R2,X2,R2a),
	compare_keys(Delta,X1,X2),
	keymerge1(Delta, X, X1, X2, R1, R2, R1a, R2a, R).

% keymerge1(Delta, X, X1, X2, R1, R2, R1a, R2a, R)

keymerge1( '>',  X, _,  X,  R1, _,  _,   R2a, R) :-
	!,
	keymerge(R1,R2a,R).
keymerge1( _,    X, X,  _,  _,  R2, R1a, _,   R) :-
	keymerge(R1a,R2,R).


compare_keys(Delta,K1-X1,K2-X2) :- compare(Delta,K1,K2).


/*
 * repeat/0
 */

export repeat/0.
  
repeat.          
repeat :- repeat.

endmod.		%% blt_ctl.pro:  Control Predicates File
