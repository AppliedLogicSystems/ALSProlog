/*===================================================================*
 | 		builtins.pro         
 | 	Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |	Builtin predicates for ALS-Prolog -- Principal File
 |
 |	Authors: Kevin A. Buettner, Ken Bowen, Chris White, 
 |	         Keith Hughes, Ilyas Cicekli
 |	Original Creation Date: 3/20/86
 *===================================================================*/

module builtins.

/*--------------------------------------------------------------------*
 | Part 1:	Operating System / Machine specific stuff.
 *--------------------------------------------------------------------*/


/*--------------------------------------------------------------------*
 | Things for hiding '$icode' weirdness.
 |
 | compiletime 
 |
 | This predicate is meant to be used in this file (builtins.pro)
 | and other files to prevent the effects of a command from
 | happening twice.  When a source file is initially consulted, this
 | is not a problem.  But when a .obp file is consulted, it is possible
 | to get both the command and the effects of the command placed into
 | the .obp file.  Calling compiletime early in a command will erase
 | the command from the .obp file.  Any effects of the command (such
 | as asserts or op declarations) will be put into the .obp file. This
 | is especially useful in tools like the dcg expander where a significant
 | amount of work may be done in transforming a grammar rule into a
 | clause which gets asserted.  The only thing which we are interested
 | in loading is the clause which gets asserted.  We are not interested
 | in doing the work of tranforming the rule over again.  In fact, if
 | we did, we would end up with two copies of the clause in the database.
 |
 | If it is desired to assert something at load time, but not have the
 | assertion placed in a .obp file, then assert_at_load_time/1 should
 | be called.  See below.
 *--------------------------------------------------------------------*/

export compiletime/0.

compiletime :- '$icode'(-18,0,0,0,0).

export at_load_time/0.

at_load_time :- '$icode'(-18,0,0,0,0).

/*--------------------------------------------------------------------*
 | module_closure(Pred,Arity)
 | module_closure(Pred,Arity,Pred2)
 |
 | These predicates are used to create "module closures."  In certain
 | situations, it is important to know from what module a predicate
 | has been called.  In the first form of the call, a predicate
 | name is provided (Pred) and the arity of the predicate (Arity).  When
 | executed a new predicate is created which will in effect call a 
 | predicate of the same name but with one more argument.  The extra
 | argument (the first) will be the name of the module from which the
 | call occurred.
 |
 | The second form is the same as the first, but Pred2 specifies the
 | name of the predicate to be called as something (possibly) different
 | from Pred.
 *--------------------------------------------------------------------*/

export module_closure/2.
export module_closure/3.

module_closure(UserPredicate,Arity) 
	:-
	module_closure(UserPredicate,Arity,UserPredicate).

module_closure(Name,Arity,Procedure) 
	:- 
	functor(IName,Name,0),
	functor(IProcedure,Procedure,0),
	'$icode'(-24,IName,Arity,IProcedure,0).

/*-------------------------------------------------------------*
 | auto_use(ModuleName)
 |
 | Puts ModuleName on the "autouse" list.   This is a list of 
 | module which are automatically used by all other modules.
 *-------------------------------------------------------------*/

export auto_use/1.

auto_use(ModuleName) :- '$icode'(-21,ModuleName,0,0,0).

/*-------------------------------------------------------------*
 | newmodule(ModuleName)
 |
 | Pushs ModuleName onto the module stack.
 *-------------------------------------------------------------*/

export newmodule/1.

newmodule(ModuleName) 
	:-
	functor(IModuleName,ModuleName,0),
	'$icode'(-10,IModuleName,0,0,0).

/*-------------------------------------------------------------*
 | endmodule
 |
 | Pops the top module off of the module stack.
 *-------------------------------------------------------------*/

export endmodule/0.

endmodule :- '$icode'(-9,0,0,0,0).

/*-------------------------------------------------------------*
 | exportpred(P,A)
 |
 | Exports P/A from the current module.
 *-------------------------------------------------------------*/

export exportpred/2.

exportpred(P,A) :- $icode(-11,P,A,0,0).

/*-------------------------------------------------------------*
 * Negation by failure
 *
 * Set up the module closures for not/1 and '\+'/1
 *-------------------------------------------------------------*/

:-  
	compiletime,
	module_closure(not,1,not),
	module_closure(\+,1,not).

not(Mod,Goal) :- call2(Mod,Goal), !, fail.
not(Mod,Goal).

%------------------------------------------------------------
% Using call2 instead of M:G is to avoid the caller cutting
% out the not/2 choice point.
%
% It would cut out the call2 choicepoint, but since there
% is no call2 choicepoint it doesn't matter.
%------------------------------------------------------------

call2(M,G) :- M:G.

/*----------------------------------------------------------------------*
 | assert/1, assert/2, 
 | asserta/1, asserta/2,
 | assertz/1, assertz/2,
 | assert_at_load_time/1
 |
 | assert/1 and assert/2 are defined the same as assertz/1 and assertz/2
 | respectively.  
 |
 | asserta/3 and assertz/3 are builtins which are defined in C and take
 | the form (for assertz)
 |           assertz(Module,Predicate,DBRef).
 |
 | Both assert/2 and assertz/2 really end up calling assertz/3 through 
 | the magic of module closures
 |
 | Similarly, asserta/2 calls asserta/3.
 |
 | assert_at_load_time/1 uses the ICRESET facility to remove asserted
 | clauses from .obp files.  It may be used in settings where it is
 | inappropriate to perform the assert at load time due to environmental
 | considerations.
 *----------------------------------------------------------------------*/
 
:-
	compiletime,
	module_closure(assert,1,'$assertz'),
	module_closure(assert,2,'$assertz3'),
	module_closure(asserta,1,'$asserta'),
	module_closure(asserta,2,'$asserta3'),
	module_closure(assertz,1,'$assertz'),
	module_closure(assertz,2,'$assertz3'),
	module_closure(assert_at_load_time,1,assertz_at_load_time),
	module_closure(asserta_at_load_time,1,asserta_at_load_time),
	module_closure(assertz_at_load_time,1,assertz_at_load_time).
 
'$asserta'(M,P) :- asserta(M,P,_,0).
'$assertz'(M,P) :- assertz(M,P,_,0).

'$asserta3'(M,P,R) :- asserta(M,P,R,0).
'$assertz3'(M,P,R) :- assertz(M,P,R,0).

asserta_at_load_time(M,P) 
	:-
	asserta(M,P,_,0),
	'$icode'(-18,0,0,0,0).

assertz_at_load_time(M,P) 
	:-
	assertz(M,P,_,0),
	'$icode'(-18,0,0,0,0).

/*-----------------*
 * dynamic/1
 *-----------------*/

:-
	compiletime,
	module_closure(dynamic,1).

dynamic(M,P/A) :-
	!,
	functor(_,P,0),
	'$dynamic'(M,P,A).
dynamic(_,M:P/A) :-
	functor(_,P,0),
	'$dynamic'(M,P,A).


    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%%% Prolog Interrupt Initialization 
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
/*-------------------------------------------*
 | Create the getPrologInterrupt and 
 | setPrologInterrupt access predicates.
 *-------------------------------------------*/

:-
	gv_alloc(VN),
	assert_at_load_time((setPrologInterrupt(X) :- gv_set(VN,X))),
	assert_at_load_time((getPrologInterrupt(X) :- gv_get(VN,X))).

export setPrologInterrupt/1.
export getPrologInterrupt/1.

/*---------------------------------------------------------------------------*
 | '$interrupt'/3
 | '$interrupt'(SigNum,Module,Goal)
 | '$interrupt'(+,+,+)
 |
 | - Base level interrupt handler.
 |
 | SigNum is the signal number which caused the interrupt.  This value is 0
 | for interrupts caused by (Prolog) program control.  Any other values 
 | indicate that the cause of the interrupt is from a caught signal on the
 | C side of things, or is due to delay variable bindings.
 |
 | The non-0 value initially implemented was 2 which is SIGINT.
 | The non-0 value 3 was added to support freeze and constraint mgmt.
 *---------------------------------------------------------------------------*/

		%% -------------------------------------
		%% Interrupt called as a result of
		%% forcePrologInterrupt (= old ouch):
		%% -------------------------------------
'$interrupt'(0,M,G) 			
	:-!,						
	getPrologInterrupt(Magic),	%% get the magic value
	'$int'(Magic,M,G).			%% and call the interrupt mechanism

		%% -------------------------------------
		%% Interrupt generated from outside 
		%% (C) [default case]: Actually N = 2:
		%% -------------------------------------
'$interrupt'(N,M,G) 
	:-
	signal_handler(N,M,G).

:- setInterruptVector('$interrupt').

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Prolog-level interrupts:
	%%%% Handle interrupts generated by 
	%%%% the application
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 
'$int'(cause_break(Old),M,G) 
	:-!,
	setPrologInterrupt(Old),
	alarm(0,0),
	trigger_event(sigint,M:G).

'$int'(application_interrupt(Old),M,G) 
	:-!,
	setPrologInterrupt(Old),
	trigger_event(application_interrupt,M:G).
 

%------------------------------------------------
% '$source'/2 interrupt.
%------------------------------------------------

'$int'('$source'(ForReal,CMod,Goals),GMod,Goal) 
	:-!,
	'$source0'(ForReal,CMod,Goals,GMod,Goal).

%--------------------------------------------------------------------------
% When the source extracter was called, a variable was given which is used
% to identify the end of extraction. This variable is ForReal, and the
% '$endsource' atom will have this variable as its value when the extracter 
% is brought to an end. Here we check it. This is done so that we can 
% extract the extracter:
%--------------------------------------------------------------------------

		%% At the end:
'$source0'(ForReal,_,_,_,'$endSource'(Check))
	:-
	Check == ForReal,
	!.

		%% Not at the end:
'$source0'(ForReal,CMod,Goals,GMod,Goal)
	:-
		% Set s/2 interrupt with the latest goal tacked on.

    '$source0_buildgoal'(CMod,GMod,Goal,NewGoal),
    setPrologInterrupt('$source'(ForReal,CMod,[NewGoal|Goals])),

		% Since the continuation pointer points into the clause
	 	% that is being extracted, all we have to do is return
		% from this one without starting up the sub-goal we have
		% just extracted, after setting up the next interrupt.

    forcePrologInterrupt.

'$source0_buildgoal'(Mod,Mod,Goal,Goal) :- !.
'$source0_buildgoal'(_,_,'!'(C),'!'(C)) :- !.
'$source0_buildgoal'(_,GMod,Goal,GMod:Goal).

/*------------------------------------------------------------------*
 |	'$source'/2
 |	'$source'(DBRef,Clause)
 |	'$source'(+,-)
 |
 |	'$source'/3
 |	'$source'(DBRef,Clause, Mode)
 |	'$source'(+,-,+)
 |
 |	Mode: show_pp, hide_pp
 |		-- determines whether the source extractor retains positional
 |		parser info in the clause (show_pp), or removes it (hide_pp).
 |		Default is hide_pp.
 |
 | The source extracter
 *------------------------------------------------------------------*/

'$source'(DBRef,Clause)
	:-
	'$source'(DBRef,Clause, hide_pp).

'$source'(DBRef,Clause, Mode)
	:-
		% Get old magic value so it can be restored when
		% we are done.
	getPrologInterrupt(OldMagic),
		% Source of the ForReal variable used to mark the end
		% of extraction.
	'$source'(DBRef,ForReal,Clause,OldMagic, Mode).

'$source'(DBRef,ForReal,Clause,OldMagic, Mode)
	:-
		% Create the Head template for the jump. Creating
		% it here simplifies the failing behaviour of
		% '$source' by not having anything fail until after
		% the '$endSource'/1.

	'$clauseinfo'(DBRef,_,ProcHandle,_),
	'$procinfo'(ProcHandle,CMod,F,A,_,_),
	functor(Head,F,A),

		% Set the s/2 interrupt. We have no subgoals and
		% ForReal is used to stop the decompiler.

	setPrologInterrupt('$source'(ForReal,CMod,[])),

		% Start extracting.

	callWithDelayedInterrupt(builtins,jump(DBRef,Head)),

		% This goal is never run, but is used to stop the
		% extracter.

	'$endSource'(ForReal),
	getPrologInterrupt('$source'(_,_,Goals)),

		% Make the clause to be returned.

	fixBody(Goals,Head,Clause,Mode),

		% Reset old magic value.

	setPrologInterrupt(OldMagic).

    	%% Failed for some reason. Fix interrupt 
		%% register and really die:

'$source'(DBRef,ForReal,Clause,OldMagic,Mode)
	:-
		% Reset old magic value.
	setPrologInterrupt(OldMagic),
	!,	
	fail.
     

fixBody([],Head,Head,Mode)
	:- !.
fixBody(['$dbg_aph'(_,_,_)],Head,Head,Mode)
	:- !.
fixBody([First|Rest],Head,(Head :- Body),Mode)
	:-
	goalFix(First,XFirst,Mode),
	fixBody0(Rest,XFirst,Body,Mode).
 
fixBody0([],Last,Last,Mode)
	:- !.
fixBody0([Current|Rest],SoFar,Body,Mode)
	:-
	goalFix(Current,XCurrent,Mode),
	!,
	fixBody0(Rest,(XCurrent,SoFar),Body,Mode).
fixBody0([_ | More],SoFar,Body,Mode)
	:-
	fixBody0(More,SoFar,Body,Mode).

goalFix(V,call(V),Mode)			:- var(V), !. 
goalFix(M:G,M:FG,Mode)			:- !, goalFix(G,FG,Mode).
goalFix('!'(_),!,Mode)			:- !.
goalFix(call(A,_),call(A),Mode)	:- !.
goalFix(':'(A,S,_),(A:S),Mode)	:- !.
goalFix('->'(A,S,_),(A->S),Mode)	:- !.
goalFix(';'(A,S,_),(A;S),Mode)	:- !.
goalFix(','(A,S,_),(A,S),Mode)	:- !.
goalFix('|'(A,S,_),(A|S),Mode)	:- !.
goalFix(dbg_call(M,G,_),dbg_call(M,G),Mode)			 :- !.
goalFix('$dbg_aph'(A,B,C),'$dbg_aph'(A,B,C),show_pp) :- !.
goalFix('$dbg_apg'(A,B,C),'$dbg_apg'(A,B,C),show_pp) :- !.
goalFix('$dbg_aph'(_,_,_),_,hide_pp) :- !, fail.
goalFix('$dbg_apg'(_,_,_),_,hide_pp) :- !, fail.
goalFix(callWithDelayedInterrupt(M,G,_),callWithDelayedInterrupt(M,G),Mode) :- !.
goalFix(freeze_list_ground(_,_,Goal),'{}'(Goal),Mode) :-!.
goalFix(freeze_list_ground(_,Goal),'{}'(Goal),Mode) :-!.
goalFix(SemiGoal,SemiFixed,Mode)
	:-
	functor(SemiGoal,P,A),
		%	name(P,[~s,~e,~m,~i,~_,~_,~_|_]):
	isgensym(semi,P),
	!,
	semiFix(SemiGoal,P,A,SemiFixed,Mode).
goalFix(A,A,Mode).

/*------------------------------------------------------------------*
 | catch0/3
 |
 | catch0(Module,Goal,ExceptionGoal) is the primitive
 | mechanism for providing a controlled abort.
 |
 | The following code relies on the catch22 mechanism and 
 | is *very* tricky.  Do not muck with it.
 *------------------------------------------------------------------*/
 
catch0(Module, Goal, Exception) 
	:-
	SWIG = foo(1),      % Structure When In Goal 
	catch0(Module, Goal, Exception, SWIG),
	(mangle(1,SWIG,2) ; mangle(1,SWIG,1), fail).

catch0(Module, Goal, Exception, SWIG) 
	:-
	catch22,
	(Module:Goal ; !, fail).

catch0(Module, Goal, Exception, SWIG) 
	:-
	SWIG = foo(2),
	throw.

catch0(Module, Goal, Exception, SWIG) 
	:-
	Module:Exception.
    

/*---------------------------------------*
 | catch/3
 | throw/1
 *---------------------------------------*/

export catch/3.
export throw/1.

:-
	compiletime,
	module_closure(catch,3).

:-
	gv_alloc(VarNum),
	assert_at_load_time( (setCatchVariable(Value) :- gv_set(VarNum,Value)) ),
	assert_at_load_time( (getCatchVariable(Value) :- gv_get(VarNum,Value)) ).

catch(Module,Goal,Pat,ExGoal) 
	:-
	nonvar_ok(Goal),
	catch0(Module,Goal,builtins:catcher(Pat,ExGoal,Module)).

catcher(Pat,ExGoal,Module) 
	:-
	getCatchVariable(Pat),
	setCatchVariable([]),	%% cleanup the catch variable
	!,
	Module:ExGoal.

catcher(Pat,ExGoal,Module) 
	:-
	throw.

throw(Pat) 
	:-
	setCatchVariable(Pat),
	throw.

/*----------------------------------------------------------------------*
 | make_gv/1
 |
 | Name should be a string (list of small numbers) with the name of the
 | global variable.
 |
 | make_gv will build two access routines setName and getName where Name
 | is the list denoting the name of the global variable.
 |
 | make_gv is defined here because it is called from some of the other
 | builtins files.  Heretofore, we have done explicit gv_alloc's and
 | asserts.
 *----------------------------------------------------------------------*/

:-	
	compiletime,
	module_closure(make_gv,1),
	module_closure(free_gv,1),
	module_closure(gv_number,2).
    
make_gv(Mod,Name) 
	:- 
	atom(Name), 
	!, 
	name(Name,NameList), 
	make_gv(Mod, NameList).

make_gv(Mod,Name) 
	:-
	name(SetFunc,[0's,0'e,0't | Name]),
	name(GetFunc,[0'g,0'e,0't | Name]),
	functor(SetHead,SetFunc,1),
	functor(GetHead,GetFunc,1),
	arg(1,SetHead,SetVar),
	arg(1,GetHead,GetVar),
	gv_alloc(VN),
	Mod:assert_at_load_time((SetHead :- gv_set(VN,SetVar))),
	Mod:assert_at_load_time((GetHead :- gv_get(VN,GetVar))).

free_gv(Mod,Name) 
	:- 
	atom(Name), 
	!, 
	name(Name,NameList), 
	free_gv(Mod, NameList).

free_gv(Mod,Name) 
	:-
	name(SetFunc,[0's,0'e,0't | Name]),
	name(GetFunc,[0'g,0'e,0't | Name]),
	functor(SetHead,SetFunc,1),
	functor(GetHead,GetFunc,1),
	arg(1,SetHead,SetVar),
	arg(1,GetHead,GetVar),
	Mod:retract((SetHead :- gv_set(VN,SetVar))),
	Mod:retract((GetHead :- gv_get(VN,GetVar))),
	!,
	gv_free(VN).

gv_number(Mod,Name,VN) 
	:- 
	atom(Name), 
	!, 
	name(Name,NameList), 
	gv_number(Mod, NameList, VN).

gv_number(Mod,Name,VN) 
	:-
	name(GetFunc,[0'g,0'e,0't | Name]),
	functor(GetHead,GetFunc,1),
	arg(1,GetHead,GetVar),
	Mod:clause(GetHead, gv_get(VN,GetVar)),
	!.

/*----------------------------------------------------------------
 | Part 3: Commands which set up certain environment parameters
 *----------------------------------------------------------------*/

/*----------------------------------------------------------*
 | The following builds the primitive predicates for the 
 | portable (byte / threaded) system.
 *----------------------------------------------------------*/

build_primitive_predicates :-
	assertz((
		'$comma'(Mod,A,B,Cutpt) :-
			'$colon'(Mod,A,Cutpt),
			'$colon'(Mod,B,Cutpt)) ),
	assertz((
		'$semicolon'(Mod,(A->B),C,Cutpt) :-
			!,
			call5(Mod,A,B,C,Cutpt)) ),
	assertz((
		'$semicolon'(Mod,A,B,Cutpt) :-
			'$colon'(Mod,A,Cutpt)) ),
	assertz((
		'$semicolon'(Mod,A,B,Cutpt) :-
			'$colon'(Mod,B,Cutpt)) ),
	assertz((
		call5(Mod,A,B,C,Cutpt) :- 
		    '$colon'(Mod,A,Cutpt), !, '$colon'(Mod,B,Cutpt)) ),
	assertz((
		call5(Mod,A,B,C,Cutpt) :-
			'$colon'(Mod,C,Cutpt)) ),
	assertz((
		'$arrow'(Mod,A,B,Cutpt) :- 
			'$colon'(Mod,A,Cutpt), !, '$colon'(Mod,B,Cutpt)) ).

/*-------------------------------------------------------------------------*
 | Module closures for the primitive control features need to be defined
 | for certain systems.  The following definition and command take care of
 | this.
 *-------------------------------------------------------------------------*/


build_primitive_closures(portable) 
	:-
	!,
	module_closure(';',2,'$semicolon'),
	module_closure('|',2,'$semicolon'),
	module_closure(',',2,'$comma'),
	module_closure('->',2,'$arrow'),
	module_closure(call,1,'$colon').


:-	compiletime,
	build_primitive_predicates,
	build_primitive_closures(portable).

:-	abolish(builtins,build_primitive_closures,1),
	abolish(builtins,build_primitive_predicates,0).

/*----------------------------------------------------------------
 | Part 4: LOADING THE REST OF THE BUILTINS FILES
 *----------------------------------------------------------------*/

/* For kernal - these are chunks of code needed by the kernal. */

/*
 * Comparison predicates:
 *   @< /2
 *   @> /2
 *   @=< /2
 *   @>= /2
 *   == /2
 *   \== /2
 *
 * Note: noneq is a very fast way to test if two things are not literally
 *       (pointer) identical.  eq is a quick way to test if two things
 *       point to the same object.  eq and noneq both work properly on atoms
 *       and integers.
 */

export @< /2.
export @> /2.
export @=< /2.
export @>= /2.

T1 @< T2     :- compare(<,T1,T2).
T1 @> T2     :- compare(>,T1,T2).
T1 @=< T2    :- compare(R,T1,T2), noneq(R,'>').
T1 @>= T2    :- compare(R,T1,T2), noneq(R,'<').

/*----------  Defined in assembler:------------
export '=='/2.
export '\=='/2.
T1 == T2     :- compare(=,T1,T2).
T1 \== T2    :- compare(R,T1,T2), noneq(R,'=').
*----------------------------------------------*/



/*
 * does not unify (\=)
 */

export \= /2.
   
X \= X :- !, fail.
_ \= _.


export length/2.

length(List,Length) :- length(List,0,Length).

length([],Length,Length) :- !.
length([_|Rest],Old,Length) :-
    New is Old+1,
    length(Rest,New,Length).

/*
 * univ (=..)
 *
 * 	Term =.. List
 */

export '=..'/2.

S =.. [F|Args] :-
    functor(S,F,A),
    !,
    univ_install(S,1,A,Args).
S =.. [F|Args] :-
    atomic(F),
    length(Args,Arity),
    !,
    functor(S,F,Arity),
    univ_install(S,1,Arity,Args).

univ_install(_,N,Arity,[]) :-
    N > Arity,
    !.
univ_install(S,N,Arity,[Arg|Args]) :-
    arg(N,S,Arg),
    !,
    NN is N+1,
    univ_install(S,NN,Arity,Args).


/*
 * abolish/2.
 *  
 * This procedure is defined in terms of abolish/3 which is a C-defined
 * builtin.  abolish/3 takes a module, procedure name, and arity.
 *  
 */


:-
	compiletime,
	module_closure(abolish,2,abolish).

/*
 * clause/2, clause/3
 *
 * Starting with the SUN implementation, the ability to get something
 * like
 *           clause(foo:f(X),B)
 * has been completely phased out.  The proper way to do the above is to
 * call clause from within module foo as
 *           foo:clause(f(X),B)
 *
 * This has simplified the resulting code greatly.  If you have
 * compatibility problems with either ported PC code (because the code was
 * written badly) or with Columbus code (because that was the only way to
 * it), you can write your own version of clause or, better still, modify
 * the source to work properly.
 *
 * See the listing/0 and listing/1  code for definitions of next_clause/2
 * and rest_clauses/3.
 */

:-
	compiletime,
	module_closure(clause,2,'$clause'),
	module_closure(clause,3,clause).
   
'$clause'(Module,Head,Body) :-
	clause(Module,Head,Body,_).

	% DBRef is ground.
clause(Module,Head,Body,DBRef) :-
	nonvar(DBRef),
	!,
	$source(DBRef,Clause),		%% decompile the DBRef
	clauseParts(Clause,Head,Body).

	% Don't have a DBRef yet.
clause(Module,Head,Body,DBRef) :-
	functor(Head,F,A),
	get_firstarg(A,Head,FirstArg),
			% Get the first clause of the procedure.
	$procinfo(_,Module,F,A,First,_),!,
			% Cycle through DBRefs until find one that works.
	get_clauses(First,DBRef,FirstArg),
	$source(DBRef,Clause),
	clauseParts(Clause,Head,Body).

clauseParts((Head :- Body),Head,Body) :- !.
clauseParts(Head,Head,true).

/*
 * clauses/2
 *
 *       Recursively get all database references of clauses starting
 *       from the given first clause.
 */

clauses(0,_) :-
        !,
	fail.
clauses(FirstDBRef,DBRef) :-
        '$clauseinfo'(FirstDBRef,NextDBRef,_,_),
        clauses(FirstDBRef,NextDBRef,DBRef).

clauses(0,_,_) :-
	!,
	fail.
clauses(DBRef,Next,DBRef).
clauses(Previous,Current,DBRef) :-
        '$clauseinfo'(Current,Next,_,_),
        clauses(Current,Next,DBRef).


/*
 * Filter clauses whose first arguments don't match the first argument
 * of the template while their data base references are collected.
 * If the first argument of the template is a variable, cycle all clauses.
 * Otherwise, get clauses whose first arguments match with the first argument
 * of the template.
 */

get_clauses(First,DBRef,FirstArg) :-
	nonvar(FirstArg), !,
	filter_clauses(First,DBRef,FirstArg).
get_clauses(First,DBRef,FirstArg) :-
	clauses(First,DBRef).

get_firstarg(0,_,_) :- !.
get_firstarg(_,Head,FirstArg) :- arg(1,Head,FirstArg).

/*
 * filter_clauses/2
 * 
 * 	 The function of this procedure is same as the function of clauses/2
 *	 except that this procedure filters out clauses whose first arguments
 *  	 don't match with the given first argument.
 */ 

filter_clauses(0,_,_) :-
	!,fail.
filter_clauses(FirstDBRef,DBRef,FirstArg) :-
	'$clauseinfo'(FirstDBRef,NextDBRef,_,_),
	filter_clauses(FirstDBRef,NextDBRef,DBRef,FirstArg).

filter_clauses(0,_,_,_) :- !,fail.
filter_clauses(DBRef,Next,DBRef,FirstArg) :- 
	'$firstargkey'(DBRef,FirstArg).
filter_clauses(Previous,Current,DBRef,FirstArg) :-
	'$clauseinfo'(Current,Next,_,_),
	filter_clauses(Current,Next,DBRef,FirstArg).

/*
 * retract/1, retract/2
 */

:- 
	compiletime,
	module_closure(retract,2,retract),
	module_closure(retract,1,$retract).

$retract(Module,X) :-
	retract(Module,X,_).

retract(Module,Clause,DBRef) :-
	nonvar(DBRef),!,
		% Decompile and delete if necessary.
	$source(DBRef,Clause),
	erase(DBRef).

	% DBRef is variable. Must do some searching.
retract(Module,Clause,DBRef) :-
		% Get the head, take it apart, and find the first clause.
	clauseParts(Clause,Head,_),
	functor(Head,F,A),
	$procinfo(_,Module,F,A,First,_),
		% Start searching for retraction
	get_firstarg(A,Head,FirstArg),
	get_clauses(First,DBRef,FirstArg),
	$source(DBRef,Clause),
	erase(DBRef).


/* From bld_evt */

/*-----------------------------------------------------------*
 | global_handler/2 is the entry to the global handler.
 *-----------------------------------------------------------*/

global_handler(EventId,Goal) :-
	global_handler(EventId,Module,Proc),
	!,
	call_handler(EventId,Goal,global_context,Module,Proc).
global_handler(EventId,Goal) :-
	ThrowTerm =.. [EventId,Goal],
	throw(ThrowTerm).

/*-----------------------------------------------------------*
 | global_handler/3 is a local database of event names and 
 | the handlers to use for those events.
 |
 | global_handler(EventId, Module, Proc)
 *-----------------------------------------------------------*/

global_handler(prolog_error,builtins,prolog_error).
global_handler(undefined_predicate,builtins,undefined_predicate).

/*--------------------------------------------------*
 | call_handler(Event,Goal,Context,Module,Proc)
 |
 | call_handler/5 builds a handler and calls it.
 *--------------------------------------------------*/

call_handler(Event,Goal,Context,Module,Proc) 
	:-
	functor(Call,Proc,3),
	arg(1,Call,Event),
	arg(2,Call,Goal),
	arg(3,Call,Context),
	Module:Call.

/*--------------------------------------------------------*
 | signal_handler is called by the primitive interrupt
 | mechanism to deal with signals.
 *--------------------------------------------------------*/

signal_handler(SigNum,Module,Goal) 
	:-
	signal_name(SigNum,SigName),
	!,
	get_context(Context),
	propagate_event(SigName,Module:Goal,Context).

/*-----------------------------------------------------*
 | propagate_event/3 is called to propagate events.
 |
 | propagate_event(EventId,Goal,Context)
 |
 *-----------------------------------------------------*/
export propagate_event/3.

propagate_event(EventId,Goal,context(Module,Proc,PrevContext)) :-
	!,
	call_handler(EventId,Goal,PrevContext,Module,Proc).
propagate_event(EventId,Goal,global_context) :-
	global_handler(EventId,Goal).

/*-------------------------------------------------------*
 | trigger_event/2 is called to start the event handler 
 | for the given event name and goal.
 *-------------------------------------------------------*/

export trigger_event/2.

trigger_event(EventId,ModuleAndGoal) :-
	get_context(Context),
	propagate_event(EventId,ModuleAndGoal,Context).


/*-------------------------------------------------------------*
 | set_context/1 and get_context/1 use global variables and 
 | are built by the following call to make_gv.
 *-------------------------------------------------------------*/
:- make_gv('_context'), set_context(global_context).

/*------------------------------------------------------------*
 | PrologError is a global variable which contains the last 
 | prolog | error encountered.
 *------------------------------------------------------------*/

:- make_gv('PrologError').

/*------------------------------------------------------------*
 | decompose_handler(Handler,Module,HMod,HProc) 
 | -- called to get the module and procedure in a handler.  
 | This permits a handler in a different module to be used.
 *------------------------------------------------------------*/

decompose_handler(M:P,_,M,P) :- !.
decompose_handler(P,M,M,P).


/*--------------------------*
 | Specific handlers
 *--------------------------*/

silent_abort(_,_,_) :-
    seen, told, abort.

prolog_error(_,_,_) :-
	getPrologError(PE),
	throw(PE).

undefined_predicate(_,Goal,_) :-
	get_PROLOG_flag(unknown,Val),
	undefined_predicate(Val,Goal).

undefined_predicate_fail(_,Goal,_) :-
	fail.

undefined_predicate(fail,Goal) :-
	!,
	fail.
undefined_predicate(error, Goal) :-
	!,
	Goal = (M:G),
	functor(G,P,_),
	pbi_debug('existence error for:'),
	pbi_debug(P),
	existence_error(procedure,Goal,Goal).
undefined_predicate(warning, M:G) :-
	!,
	functor(G,P,A),
	als_advise('\nWarning: Undefined procedure %s:%s/%d called.\n',[M,P,A]),
	fail.
undefined_predicate(break, M:G) :-
	!,
	functor(G,P,A),
	als_advise('\nWarning: Undefined procedure %s:%s/%d called.\n',[M,P,A]),
	breakhandler(M,G).

/********************************************************** 
 | Triggering certain predefined events
 |
 | The following predicates are used to trigger events
 **********************************************************/


export existence_error/3.
existence_error(ObType,Culprit,Depth) :-
	integer(Depth),
	!,
	frame_info(Depth,FI),
	setPrologError(error(existence_error(ObType,Culprit),[FI])),
	forcePrologError.
existence_error(ObType,Culprit,Goal) :-
	setPrologError(error(existence_error(ObType,Culprit),[Goal])),
	forcePrologError.

export nonvar_ok/1.
nonvar_ok(Var) :-
	nonvar(Var),
	!.
nonvar_ok(Other) :-
	instantiation_error(2).

/* Hash tables from from bld_db */

/*---------------------------------------------------------------------
 | 				Hash table (expandable) predicates
 |
 |	The core hash tables are physically simply terms of the form
 |
 |		hashArray(.........)
 |
 |	We are exploiting the fact that the implementation of terms is
 |	such that a term is an array of (pointers to) its arguments. So
 |	what makes a hash table a hash table below is the access routines 
 |	implemented using basic hashing techniques.  We also exploit the
 |	destructive update feature mangle/3.
 |	Each argument (entry) in a hash table here is a (pointer) to a 
 |	list [E1, E2, ....] where each Ei is a cons term of the form
 |
 |			[Key | Value]
 |	
 |	So a bucket looks like:	
 |	
 |		[ [Key1 | Val1], [Key2 | Val2], ....]
 |
 |	where each Keyi hashes into the index (argument number) of this 
 |	bucket in the term
 |	
 |		hashArray(.........)
 |
 |	The complete hash tables are terms of the form
 |
 |		hastTable(Depth,Size,RehashCount,hashArray(....))
 |
 |	where:
 |		Depth		= the hashing depth of keys going in;
 |		Size		= arity of the hashArray(...) term;
 |		RehashCount	= counts (down) the number of hash entries
 |						which have been made; when then counter
 |						reaches 0, the table is expanded and
 |						rehashed.
 |
 |	The basic (non-multi) versions of these predicates overwrite
 |	existing key values; i.e., if Key-Value0 is already present
 |	in the table, then hash inserting Key-Value1 will cause the
 |	physical entry for Value0 to be physcially altered to become
 |	Value1 (uses mangle/3).
 |
 |	The "-multi" versions of these predicates do NOT overwrite
 |	existing values, but instead treat the Key-___ cons items as
 |	tagged pushdown lists, so that if [Key | Value0] was present,
 |	then after hash_multi_inserting Key-Value1, the Key part of the
 |	bucket looks like: [Key | [Value1 | Value0] ]; i.e., it is
 |
 |		[Key, Value1 | Value0]
 |
 |	Key hashing is performed by the predicate
 |
 |		hashN(Key,Size,Depth,Index),
 |
 *--------------------------------------------------------------------*/

/*---------------------------------------------------------------*
 |	hash_create/1 
 |	hash_create(Table)
 |	hash_create(-)
 |
 |	- creates a small hash table, hashing on keys to depth 3.
 *---------------------------------------------------------------*/
export hash_create/1.

hash_create(Table) 
	:-
	hash_create(3,Table).

hash_create(Depth, hashTable(Depth,Size,RehashCount,Array)) 
	:-
	hashTableSizes(Size,RehashCount),
	!,
	functor(Array,hashArray,Size),
	initHashTable(Size,Array).

initHashTable(0,_) 
	:- !.
initHashTable(N,T) 
	:-
	arg(N,T,[]),
	NN is N-1,
	initHashTable(NN,T).


/*---------------------------------------------------------------*
 |	hash_member/3
 |	hash_member(Key,Bucket,Pair)
 |	hash_member(+,+,-)
 |
 |	- seeks first Pair in Bucket with left element = Key
 |
 |	Resatisfiable
 *---------------------------------------------------------------*/
hash_member(Key,[Pair|_],Pair) 
	:- 
	arg(1,Pair,Key).
hash_member(Key,[_|BucketTail],Pair) 
	:- 
	hash_member(Key,BucketTail,Pair).

/*---------------------------------------------------------------*
 |	hash_insert/3
 |	hash_insert(Key,Value,Table)
 |	hash_insert(+,+,+)
 |
 |	-	inserts Key-Value pair in hash Table
 |
 |	Unitary: overwrites any existing key-value entry 
 *---------------------------------------------------------------*/
export hash_insert/3.

hash_insert(Key,Value,Table) 
	:-
	arg(1,Table,Depth),
	arg(2,Table,Size),
	arg(4,Table,Array),
	hashN(Key,Size,Depth,Index),
	arg(Index,Array,Bucket),
	hash_insert(Bucket,Array,Index,Key,Value,Table).

/*---------------------------------------------------------------*
 |	hash_insert/6
 |	hash_insert(Bucket,Array,Index,Key,Value,Table)
 |	hash_insert(+,+,+,+,+,+)
 | 
 |	- inserts key-value pair in hash table bucket
 |
 |	Unitary: overwrites any existing key-value entry 
 *---------------------------------------------------------------*/
hash_insert(Bucket,Array,Index,Key,Value,Table) 
	:-
	hash_member(Key,Bucket,E),
	!,
	mangle(2,E,Value).
hash_insert(Bucket,Array,Index,Key,Value,Table) 
	:-
	mangle(Index,Array,[[Key|Value]|Bucket]),
	arg(3,Table,RehashCount),
	NewRehashCount is RehashCount-1,
	hash_rehash(NewRehashCount,Table).

/*
 * hash_insert_multi(Key,Value,Table)
 */
/*---------------------------------------------------------------*
 |	hash_insert_multi/3
 |	hash_insert_multi(Key,Value,Table)
 |	hash_insert_multi(+,+,+)
 |
 |	-	inserts Key-Value pair in hash Table
 |
 |	Multi-Valued: accumulates multiple key-value entries
 *---------------------------------------------------------------*/

export hash_insert_multi/3.

hash_insert_multi(Key,Value,Table) 
	:-
	arg(1,Table,Depth),
	arg(2,Table,Size),
	arg(4,Table,Array),
	hashN(Key,Size,Depth,Index),
	arg(Index,Array,Bucket),
	hash_insert_multi(Bucket,Array,Index,Key,Value,Table).

/*---------------------------------------------------------------*
 |	hash_insert/6
 |	hash_insert(Bucket,Array,Index,Key,Value,Table)
 |	hash_insert(+,+,+,+,+,+)
 | 
 |	- inserts key-value pair in hash table bucket
 |
 |	Multi-Valued: accumulates multiple key-value entries
 *---------------------------------------------------------------*/
hash_insert_multi(Bucket,Array,Index,Key,Value,Table) 
	:-
	hash_member(Key,Bucket,E),
	!,
	arg(2,E,CurValue),
	mangle(2,E,[Value | CurValue]).

	%% Note that [Key,Value] = [Key | [Value] ]
hash_insert_multi(Bucket,Array,Index,Key,Value,Table) 
	:-
%	mangle(Index,Array,[[Key|Value]|Bucket]),
	mangle(Index,Array,[ [Key,Value] | Bucket]),
	arg(3,Table,RehashCount),
	NewRehashCount is RehashCount-1,
	hash_rehash(NewRehashCount,Table).

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
hash_rehash(Count,Table) 
	:-
	Count > 0,
	!,
	mangle(3,Table,Count).

hash_rehash(_,Table) 
	:-
	arg(2,Table,Size),
	hashTableSizes(NewSize,RHC),
	NewSize>Size,
	!,
	functor(NewArray,hashArray,NewSize),
	initHashTable(NewSize,NewArray),
	mangle(2,Table,NewSize),
	mangle(3,Table,RHC),
	arg(4,Table,Array),
	mangle(4,Table,NewArray),
	hash_rehash2(Array,Size,Table).

hash_rehash2(Array,Size,Table) 
	:-
	hash_elements(Array,Size,Key,Data),
	hash_insert(Key,Data,Table),
	fail.
hash_rehash2(_,_,_).


/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
export hash_elements/3.

hash_elements(Key,Data,Table) 
	:-
	arg(2,Table,Size),
	arg(4,Table,Array),
	hash_elements(Array,Size,Key,Data).

hash_elements(Array,N,Key,Data) 
	:-
	arg(N,Array,Bucket),
	hash_member(_,Bucket,[Key|Data]).

hash_elements(Array,N,Key,Data) 
	:-
	N > 0,
	NN is N-1,
	hash_elements(Array,NN,Key,Data).

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
export hash_lookup/3.
hash_lookup(Key,Value,Table) 
	:-
	arg(1,Table,Depth),
	arg(2,Table,Size),
	arg(4,Table,Array),
	hashN(Key,Size,Depth,Index),
	arg(Index,Array,Bucket),
	hash_member(Key,Bucket,[_|Value2]),
	!,
	Value = Value2.

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
export hash_delete/3.
hash_delete(Key,Value,Table) 
	:-
	arg(1,Table,Depth),
	arg(2,Table,Size),
	arg(4,Table,Array),
	hashN(Key,Size,Depth,Index),
	hash_delete(Array,Index,Key,Value,Table),
	!.

hash_delete(Struct,Index,Key,Value,Table) 
	:-
	arg(Index,Struct,EList),		%% get element list
	arg(1,EList,Elem),				%% get first element on list
	arg(1,Elem,Key),				%% match key
	arg(2,Elem,Value),				%% match value
	!,
	arg(2,EList,REList),			%% get rest of element list
	mangle(Index,Struct,REList),	%% delete element off of list
	arg(3,Table,RehashCount),
	NewRehashCount is RehashCount+1,
	mangle(3,Table,NewRehashCount).		%% update count

/*
hash_delete(Struct,Index,Key,Value,Table) 
	:-
	arg(Index,Struct,EList),		%% get element list
	hash_delete(EList,2,Key,Value,Table).	%% loop
*/

export hash_delete_multi/3.
hash_delete_multi(Key,Value,Table) 
	:-
	arg(1,Table,Depth),
	arg(2,Table,Size),
	arg(4,Table,Array),
	hashN(Key,Size,Depth,Index),
	arg(Index,Struct,Bucket),		%% get bucket list
	hash_delete_multi(Bucket,Key,Value,Table),
	!.

hash_delete_multi(Bucket,Key,Value,Table) 
	:-
	arg(1,Bucket,EList),				%% get first element on list
	arg(1,EList,Key),				%% match key
	rip_out(EList, Value),
	arg(3,Table,RehashCount),
	NewRehashCount is RehashCount+1,
	mangle(3,Table,NewRehashCount).		%% update count

/*

	arg(2,Elem,Value),				%% match value
	!,
hash_delete_multi(Struct,Index,Key,Value,Table) 
	:-
	arg(Index,Struct,EList),		%% get element list
	hash_delete(EList,2,Key,Value,Table).	%% loop
*/

rip_out(EList, Value)
	:-
	arg(2,EList,REList),
	arg(1,REList,RELHead),				
	disp_rip_out(RELHead, Value, EList, REList).
	
disp_rip_out(Value, Value, EList, REList)
	:-
	arg(2, REList, RETail),
	mangle(2, EList, RETail).

disp_rip_out(RELHead, Value, EList, REList)
	:-
	rip_out(REList, Value).

/*---------------------------------------------------------------*
 *---------------------------------------------------------------*/
export hash_delete_pattern/3.

hash_delete_pattern(Key,Value,Table) 
	:-
	arg(2,Table,Size),
	arg(4,Table,Array),
	hash_delete_pattern(Array,Size,Key,Value,Table).

hash_delete_pattern(Array,N,Key,Value,Table) 
	:-
	arg(N,Array,Bucket),				%% should be more efficient
	hash_member(_,Bucket,[Key|Value]),	%% way to do this...
	hash_delete(Array,N,Key,Value,Table).

hash_delete_pattern(Array,N,Key,Value,Table) 
	:-
	N > 0,
	NN is N-1,
	hash_delete_pattern(Array,NN,Key,Value,Table).

hashTableSizes(31,47).
hashTableSizes(61,92).
hashTableSizes(127,191).
hashTableSizes(251,377).
hashTableSizes(509,764).
hashTableSizes(1021,1532).
hashTableSizes(2039,3059).
hashTableSizes(4089,6134).
hashTableSizes(8191,12287).
hashTableSizes(16381,24572).
hashTableSizes(32749,49124).
hashTableSizes(65521,98282).
hashTableSizes(131071,20000000).

/*---------------------------------------------------------------------*
 | make_hash_table/1
 |
 | This procedure is similar to make_gv/1.  It creates hash table access
 | predicates which use the hashing predicates above.
 |
 | make_hash_table will build two access predicates setName and getName where
 | Name is an atom or list denoting the name of the hash table. The access
 | predicates take two arguments, the first of which is the key and the
 | second is the value.
 *---------------------------------------------------------------------*/

:-
	compiletime,
	module_closure(make_hash_table,1).

make_hash_table(Mod,Name) :-
	atom(Name),
	!,
	name(Name,NameList),
	make_hash_table(Mod,NameList).



	%% see if already present:
make_hash_table(Mod,Name) 
	:-
	name(ResetFunc,	[0'r, 0'e, 0's, 0'e, 0't | Name]),
	functor(ResetHead, ResetFunc, 0),
	Mod:clause(ResetHead,_), 		
	name(SetFunc,	[0's, 0'e, 0't | Name]),
	functor(SetHead,   SetFunc,   2),
	Mod:clause(SetHead,_),
	!.

	%% not already present:
make_hash_table(Mod,Name) 
	:-
	make_hash_framework(Mod,Name,VN),
	hash_create(InitialHashTable),
	gv_set(VN,InitialHashTable).

export make_hash_framework/3.
make_hash_framework(Mod,Name,VN)
	:-
	name(ResetFunc,	[0'r, 0'e, 0's, 0'e, 0't | Name]),
	name(SetFunc,	[0's, 0'e, 0't | Name]),
	name(SetMFunc,	[0's, 0'e, 0't, 0'm | Name]),
	name(GetFunc,	[0'g, 0'e, 0't | Name]),
	name(PGetFunc,	[0'p, 0'g, 0'e, 0't | Name]),
	name(DelFunc,	[0'd, 0'e, 0'l | Name]),
	name(PDelFunc,	[0'p, 0'd, 0'e, 0'l | Name]),

	functor(ResetHead, ResetFunc, 0),
	functor(SetHead,   SetFunc,   2),
	functor(SetMHead,  SetMFunc,  2),
	functor(GetHead,   GetFunc,   2),
	functor(PGetHead,  PGetFunc,  2),
	functor(DelHead,   DelFunc,   2),
	functor(PDelHead,  PDelFunc,  2),

	arg(1,SetHead, Key),	arg(2,SetHead, Val),
	arg(1,SetMHead, Key),	arg(2,SetMHead, Val),
	arg(1,GetHead, Key),	arg(2,GetHead, Val),
	arg(1,PGetHead,Key),	arg(2,PGetHead,Val),
	arg(1,DelHead, Key),	arg(2,DelHead, Val),
	arg(1,PDelHead,Key),	arg(2,PDelHead,Val),

	gv_alloc(VN),

	Mod:assert_at_load_time(
			(ResetHead :- hash_create(TB), gv_set(VN,TB))),
	Mod:assert_at_load_time(
			(SetHead  :- gv_get(VN,TB), hash_insert(Key,Val,TB))),
	Mod:assert_at_load_time(
			(SetMHead :- gv_get(VN,TB), hash_insert_multi(Key,Val,TB))),
	Mod:assert_at_load_time(
			(GetHead  :- gv_get(VN,TB), hash_lookup(Key,Val,TB))),
	Mod:assert_at_load_time(
	        (PGetHead :- gv_get(VN,TB), hash_elements(Key,Val,TB))),
	Mod:assert_at_load_time(
			(DelHead  :- gv_get(VN,TB), hash_delete(Key,Val,TB))),
	Mod:assert_at_load_time(
			(PDelHead :- gv_get(VN,TB), hash_delete_pattern(Key,Val,TB))).


/* from blt_flags */

export current_prolog_flag/2.
current_prolog_flag(Flag, Value) 
	:-
	var_or_atom_ok(Flag),
	pget_PROLOG_flag(Flag, Value).

export set_prolog_flag/2.
set_prolog_flag(Flag, Value) 
	:-
	atom_ok(Flag),
	(   get_PROLOG_flag(Flag, _) -> true
	;   domain_error(prolog_flag, Flag, 1) ),
	(   prolog_flag_value_check(Flag, Value) -> true
	;   domain_error(flag_value, Flag+Value, 1) ),
	!,
	set_PROLOG_flag(Flag, Value).

init_prolog_flags 
	:-
	make_hash_table('_PROLOG_flag'),
	set_default_flags.

set_default_flags
	:-
	default_prolog_flag_value(Flag, Value),
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

	%%---------------------------------
	%%	max_integer  (7.11.1.2)
	%%---------------------------------
default_prolog_flag_value(max_integer, Value)
	:-
	max_iv(Value).

	%%---------------------------------
	%%	min_integer  (7.11.1.3)
	%%---------------------------------
default_prolog_flag_value(min_integer, Value)
	:-
	max_iv(Value).

	%%---------------------------------
	%%	integer_rounding_function  (7.11.1.4)
	%%---------------------------------
prolog_flag_value_check(integer_rounding_function, down).  
prolog_flag_value_check(integer_rounding_function, toward_zero).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%	Other Flags (7.11.2)
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%---------------------------------
	%%	char_conversion  (7.11.2.1)
	%%---------------------------------
prolog_flag_value_check(char_conversion, on).  
prolog_flag_value_check(char_conversion, off).  

	%%---------------------------------
	%%	debug (7.11.2.2)
	%%---------------------------------
prolog_flag_value_check(debug, off).  
prolog_flag_value_check(debug, on).  

	%%---------------------------------
	%%	max_arity (7.11.2.3)
	%%---------------------------------
default_prolog_flag_value(max_arity, Value)
	:-
	max_iv(Value).

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

% Old compatability stuff:
prolog_flag_value_check(undefined_predicate, Value) 
	:- 
	prolog_flag_value_check(unknown, Value).

/*
default_prolog_flag_value(undefined_predicate, Value)
	:-
	default_prolog_flag_value(unknown, Value).
*/


	%%---------------------------------
	%%	double_quotes 	(7.11.2.5)
	%%---------------------------------
prolog_flag_value_check(double_quotes, chars).
prolog_flag_value_check(double_quotes, codes).
prolog_flag_value_check(double_quotes, atom).

default_prolog_flag_value(double_quotes, codes).


:- init_prolog_flags.

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

/* from xconsult */

/*---------------------------------------------------------------------------
 * listing fixup
 *
 * The procedure goalFix/2 is defined in builtins.pro.  It is used by
 * the source extracter (listing, clause, and retract) and by the debugger
 * to put certain goals in a more appropriate external form.  This is necessary
 * because certain goals contain a cut point as an additional argument.
 * goalFix/2 is used in such circumstances to return a form which does
 * not have the additional argument.  
 *
 * Semicolon and arrow expansion causes similar problems, though probably
 * much more severe.  Instead of a single argument causing a problem,
 * an entire control structure has been replaced with a procedure with
 * a funny name.
 *
 * The strategy here is to attach a new clause for goalFix/2 which will
 * detect such procedures with funny names (semi___ as a prefix) and
 * get all of the clauses which form this procedure, unify the heads of
 * all clauses together with the original goal which was to be fixed up,
 * then build semicolon structures with the clause bodies as the elements
 * of the structures.  Along the way, the clause bodies need to be
 * transformed as they will contain a ! instead of an -> and $cut/1
 * instead of cut.
 *---------------------------------------------------------------------------*/


semiFix(SemiGoal,P,A,SemiFixed,Mode)
	:-
	procedures(M,P,A,FirstRef),
	!,
	semiclause(FirstRef,SemiGoal,FirstBody,Mode),
	sf0(FirstRef,SemiGoal,FirstBody,SemiFixed,Mode).

%% === sf0 recursively gets the all clauses after the first and
%%     separates them with semicolons
sf0(Ref,Head,Body,(Body;Semis),Mode)
	:-
	'$clauseinfo'(Ref,NextRef,_,_),
	NextRef \= 0,
	!,
	semiclause(NextRef,Head,NextBody,Mode),
	sf0(NextRef,Head,NextBody,Semis,Mode).
sf0(_,_,Body,Body,Mode).

%% === semiclause is given a clause reference and a head (which
%%     will be unified with all the heads seen so far) and returns
%%     the clause body after being fixed up so that it will contain
%%     arrow instead of ! and ! instead of $cut.
semiclause(Ref,Head,FixedBody,Mode)
	:-
	clause(Head,Body,Ref),
	!,
	fixBody(Body,FixedBody,Mode).


%% === fixBody/2 is responsible for fixing the body of a clause for
%%     semiclause/3
fixBody(Body,FixedBody,Mode)
	:-
	fixBody(Body,FixedBody,T,T,Mode).

%% === fixBody(InGoal,Top,Goals,Trans,Mode)
%%		InGoal 		-- the goal we are about to look at
%%		Top		-- the variable which represents the top level
%%				   return value;  Such a mechanism is necessary
%%				   since the trees for the input structure
%%				   and output structures are dramatically
%%				   different.
%%		Goals		-- Variable representing the goals that we
%%				   have encountered before this point in
%%				   our traversal of the comma list
%%		Trans		-- The transformed InGoal
%%     This procedure is responsible for traversing a comma list and
%%     passing each goal encounted in the comma list to fixBody1 where
%%     the goals will be subjected to further scrutiny.  Note that the
%%     basis clauses come after the recursive clause and are responsible
%%	   for dealing with the various end-of-comma-list cases.
fixBody((A,B),Top,Goals,Trans,Mode)
	:- !,
	fixBody1(A,B,Top,Goals,TA,TB,Mode),
	makeConjunct(TA,TB,Trans).
fixBody(!,(Goals->true),Goals,true,Mode)
	:- !.
fixBody($cut(_),Top,Top,!,Mode)
	:- !.
fixBody(G,Top,Top,G,Mode).


%% === fixBody1(Goal,Rest,Top,Goals,TGoal,TRest)
%%		Goal		-- the Goal which we are about to transform
%%		Rest		-- the rest of the goals in the comma list
%%		Top		-- the variable which represents the top level
%%				   return value;  see above
%%		Goals		-- Variable representing the transformed goals
%%				   that we have encountered before this point
%%				   in our traversal of the comma list
%%		TGoal		-- the transformed Goal
%%		TRest		-- the transformed Rest
%%     This procedure transforms one goal and recursively calls some
%%     variant of fixBody to transform the rest.
fixBody1(!,Rest,(Goals->TRest),Goals,true,true,Mode)
	:- !,
	fixBody(Rest,TRest,Mode).
fixBody1($cut(_),Rest,Top,Goal,!,TRest,Mode)
	:- !,
	fixBody(Rest,Top,Goal,TRest,Mode).
fixBody1(Goal,Rest,Top,Goals,Goal,TRest,Mode)
	:-
	fixBody(Rest,Top,Goals,TRest,Mode).


endmod.		%% builtins.pro -- Main File for builtins

