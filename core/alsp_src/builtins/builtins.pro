/*===================================================================*
 | 		builtins.pro         
 | 	Copyright (c) 1986-1994 Applied Logic Systems, Inc.
 |
 |	Builtin predicates for ALS-Prolog -- Principal File
 |
 |	Authors: Kevin A. Buettner, Ken Bowen, Chris White, 
 |	         Keith Hughes, Ilyas Cicekli
 |	Original Creation Date: 3/20/86
 *===================================================================*/

module builtins.
use debugger.

/*--------------------------------------------------------------------*
 | Part 1:	Operating System / Machine specific stuff.
 *--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*
 |	als_system/1
 |
 |	This procedure consists of a single fact which is asserted by the
 |	underlying C substrate of the system at initialization time.  The
 |	argument is a list of 'equality' statements of the form
 |		property=value
 |	which provide important descriptive data about the environment.
 |	Here are the properties and their current possible values:
 |
 |	Property		Values
 |	--------		------
 |	os			unix, dos, vms, mac
 |	os_variation	(unix): bsd, systemV, xenix
 |			(dos):	plain, eclipse, pharlap
 |	os_version		-- 'nnn.mm' ---
 |	processor		i8086, i286, i386, i486
 |					m68k, m88k, vax, ...
 |					portable
 |	manufacturer		(.e.g. sun3).....  
 |	prolog_version		-- 'nnn.mm' --- 
 |
 |
 |
 |	command_line/1
 |
 |	This procedure consists of a single fact which is asserted by the
 |	underlying C substrate of the system at initialization time. The
 |	argument is a list of UIAs (atoms) representing the command line
 |	arguments.
 |
 |
 |	sys_searchdir/1
 |
 |	Also asserted at initialization time are facts which tells us
 |	where the builtins file might be located.
 *--------------------------------------------------------------------*/

export als_system/1.
export command_line/1.

/*--------------------------------------------------------------------*
 | Part 2:	Operating System / Machine independent stuff.
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

/*-----------------------------------------------------------------*
 * Assert Predicates which are not affected by reconsult operation
 *
 * Note: I don't think that these are necessary anymore
 *			-- Kev 2-26-92
 *-----------------------------------------------------------------*/

:-
	compiletime,
	module_closure(reconsult_assert,1,'$reconsult_assertz'),
	module_closure(reconsult_assert,2,'$reconsult_assertz3'),
	module_closure(reconsult_asserta,1,'$reconsult_asserta'),
	module_closure(reconsult_asserta,2,'$reconsult_asserta3'),
	module_closure(reconsult_assertz,1,'$reconsult_assertz'),
	module_closure(reconsult_assertz,2,reconsult_assertz),
	module_closure(reconsult_assert_at_load_time,1,
		    reconsult_assertz_at_load_time),
	module_closure(reconsult_asserta_at_load_time,1,
		    reconsult_asserta_at_load_time),
	module_closure(reconsult_assertz_at_load_time,1,
		    reconsult_assertz_at_load_time).
 
'$reconsult_asserta'(M,P) :- asserta(M,P,_,1).
'$reconsult_assertz'(M,P) :- assertz(M,P,_,1).

'$reconsult_asserta3'(M,P,R) :- asserta(M,P,R,1).
'$reconsult_assertz3'(M,P,R) :- assertz(M,P,R,1).

reconsult_asserta_at_load_time(M,P) 
	:-
	asserta(M,P,_,1),
	'$icode'(-18,0,0,0,0).

reconsult_assertz_at_load_time(M,P) 
	:-
	assertz(M,P,_,1),
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

		%% ------------------------------------------------------
		%% Interrupt called because trailing discovered a delay
		%% variable was being bound to a non-delay-variable:
		%% ------------------------------------------------------
'$interrupt'(3,M,G) 
	:-!,
		%% Suppresses interrupting on the (temporary) binding
		%% that write_term performs:
	functor(G,F,N),
	fin_delay_int(M,F,N,G).

	%% Exclude interrupts occurring because of pretty-printing:
fin_delay_int(sio,wd_infix,10,G)
	:-!,
	sio:G .
fin_delay_int(sio,tail_depth,2,G)
	:-!,
	sio:G .
fin_delay_int(sio,is,2,G)
	:-!,
	sio:G .
fin_delay_int(sio,wd_space,3,G)
	:-!,
	sio:G .

	%% Allow interrupt in all other cases:
fin_delay_int(M,_,_,G)
	:-
		  pbi_write('----------$interrupt-3------'(M,G) ), pbi_nl, pbi_ttyflush,
	clct_tr(ActiveDelays),
	delay_handler(ActiveDelays),
	M:G.

/************************************************************************
		%% -------------------------------------------------
		%% Interrupt called because trailing discovered two
		%% delay variables being bound together
		%% -------------------------------------------------
'$interrupt'(4,M,G) 
	:-!,
		  pbi_write('----------$interrupt-4------'(M,G) ), pbi_nl, pbi_ttyflush,
	combine_delay_vars(M,G).
************************************************************************/

		%% -------------------------------------
		%% Interrupt generated from outside 
		%% (C) [default case]: Actually N = 2:
		%% -------------------------------------
'$interrupt'(N,M,G) 
	:-
	signal_handler(N,M,G).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Prolog-level interrupts:
	%%%% Handle interrupts generated by 
    %%%% the application
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
'$int'(application_interrupt(Old),M,G) :-
	!,
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
	xconsult:semiFix(SemiGoal,P,A,SemiFixed,Mode).
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

/*------------------------------------------------------------*
 * Some predicates needed for the rest of the initialization.
 *------------------------------------------------------------*/

export dmember/2, dappend/3.

dmember(Item,[Item|_]) :- !.
dmember(Item,[_|Rest]) :- dmember(Item,Rest).

dappend([],L,L) :- !.
dappend([H|T],L,[H|TL]) :- dappend(T,L,TL).

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

/*-------------------------------------------------------------*
 | sys_env(OS,Processor) is a short form of the environment
 | information.  It is calculated from the als_system/1 values.
 *-------------------------------------------------------------*/

export sys_env/3.

:-	compiletime,
	als_system(SysVars),
	dmember(processor=Proc,SysVars),
	dmember(os=OS,SysVars),
	dmember(os_variation=MinorOS,SysVars),
	assert(sys_env(OS,MinorOS,Proc)).

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

build_primitive_closures(i386) 
	:-
	!,
	module_closure(';',2,'$semicolon'),
	module_closure('|',2,'$semicolon'),
	module_closure(',',2,'$comma'),
	module_closure('->',2,'$arrow'),
	module_closure(call,1,'$colon').

build_primitive_closures('port_byte') 
	:-
	build_primitive_closures(portable).

build_primitive_closures('port_thread') 
	:-
	build_primitive_closures(portable).

build_primitive_closures(portable) 
	:-
	!,
	module_closure(';',2,'$semicolon'),
	module_closure('|',2,'$semicolon'),
	module_closure(',',2,'$comma'),
	module_closure('->',2,'$arrow'),
	module_closure(call,1,'$colon').

build_primitive_closures(_) 
	:-
	!,
	module_closure(';',2,'$semicolon'),
	module_closure('|',2,'$semicolon'),
	module_closure(',',2,'$comma'),
	module_closure('->',2,'$arrow')
%	,module_closure(call,1,'$colon')     %% so call gets trapped by debugger
.

:-	compiletime,
    sys_env(_,_,Proc),
	build_primitive_predicates,
	build_primitive_closures(Proc).

:-	abolish(builtins,build_primitive_closures,1),
	abolish(builtins,build_primitive_predicates,0).

/*----------------------------------------------------------------
 | Part 4: LOADING THE REST OF THE BUILTINS FILES
 *----------------------------------------------------------------*/

load_builtins(File) 
	:-
	sys_env(OS,_,_),
	(OS = macos, !, Sepr = ':'; Sepr = '/'),
	'$atom_concat'('builtins',Sepr, BDir),
	load_builtins(BDir, File).

load_builtins(BDir, File) 
	:-
	sys_searchdir(Path),
        '$atom_concat'(BDir,File, BltFile),
	'$atom_concat'(Path,BltFile,FileAndPath),
%%pbi_write(FileAndPath),pbi_nl,pbi_ttyflush,
	'$load'(FileAndPath, 0).

:-	auto_use(sio).
:-	auto_use(debugger).
:-	auto_use(xconsult).
		%% Make this a conditional:
:-	auto_use(rel_arith).

:- 	command_line(CL), 
	dmember('-nobuilt',CL),!	% ,pbi_write(nobuilts), pbi_nl
	;
		%%pbi_write(loading_builts),pbi_nl,pbi_ttyflush,
	sys_env(OS,_,_),
	(OS = macos, !, Sepr = '\\'; Sepr = '/'),
	'$atom_concat'('builtins',Sepr, BDir),

	load_builtins(BDir, sio_rt),		%% for getting op declarations
	load_builtins(BDir, blt_evt),		%% need error checking code early
	load_builtins(BDir, blt_term),
	load_builtins(BDir, blt_db),		%% must come before blt_sys
	load_builtins(BDir, filepath),		%% also must come before blt_sys
	load_builtins(BDir, blt_io),
	load_builtins(BDir, blt_ctl),
	load_builtins(BDir, blt_sys),
	load_builtins(BDir, blt_std),
	load_builtins(BDir, blt_stk),
	load_builtins(BDir, blt_als),
	load_builtins(BDir, blt_atom),
	load_builtins(BDir, cutils),
	load_builtins(BDir, sio),
	load_builtins(BDir, sio_wt),
	load_builtins(BDir, sio_d10),
	load_builtins(BDir, blt_lib), 
	load_builtins(BDir, blt_msg),
	load_builtins(BDir, blt_brk),
	load_builtins(BDir, xconsult),
	load_builtins(BDir, fs_cmn),
	load_builtins(BDir, dcgs),
	load_builtins(BDir, blt_pckg),
	load_builtins(BDir, blt_frez),
	load_builtins(BDir, blt_shl).

%%--------------------------------------------
%% set up the operators (stream io stuff needs to be 
%% loaded in order to set up the operators)
%%--------------------------------------------

nops 
	:-
	op(900,fy,not),
	op(900,fy,\+).

ld_is
	:-	
	sys_env(_,_,Proc),
    	(   (Proc = sparc ; Proc = m88k; Proc = m68k)
	->  load_builtins(blt_is)
    	;   true
        ).

ld_fs
	:-	
	sys_env(OS,_,_),
	(   OS = unix -> load_builtins(fsunix)
	;   OS = dos  -> load_builtins(fsdos)
	;	OS = macos -> load_builtins(fsmac)
	;   true		%% Extend to other OS's
	).


ld_mth
	:-	
	sys_env(_,_,Proc),
	(   Proc = m88k
	->  load_builtins(math88k)
	;   true
	).

ld_wins
	:-	
	als_system(Sys),
	dmember(wins=WinSys,Sys),
	(   WinSys = mswins -> load_builtins(windows), load_builtins(win_sh)
	;   true
	).


:-  command_line(CL), 
	dmember('-nobuilt',CL),!	% ,pbi_write(nobuilts_no_endstuff), pbi_nl
	;
	nops, 
	ld_is, 
	ld_fs, 
	ld_mth, 
	ld_wins.

:- dynamic(intconstr/0).

:-  intconstr, !, 
	load_builtins(ra_basis),
	load_builtins(int_cstr)
	;
	true.

/*----------------------------------------------------------------
 | Part 5: Starting a shell or an entry point.
 | --------------------------------------------------------------
 |	NOTE: Packaging an application with als-mics:
 |  - 	Normally replaces (retracts/asserts) '$start' as defined
 |		below by a new definition of '$start' which jumps to the
 |		entry point of the application
 |	-	Can replace '$initialize' as defined below by
 |	
 |		'$initialize'
 |			:-
 |			pckg_init,
 |			<appl init calls>.
 |
 | These replacements are accomplished via options to save_image/2.
 *----------------------------------------------------------------*/

/*-------------------------------------------------------------*
 | '$initialize' is called to perform all of those 
 | initializations which must be done prior to calling '$start'
 *-------------------------------------------------------------*/

'$initialize' 
	:-
	pckg_init.

/*------------------------------------------------------------------------*
 | '$start' is the initial (shell) goal which is run by the prolog system.
 *------------------------------------------------------------------------*/
		%% This starts the tty shell:
'$start' 
	:-
 	command_line(CL), 
	dmember('-nobuilt',CL),!	% ,pbi_write(nobuilts_no_shell), pbi_nl
	;
	start_shell(builtins:prolog_shell).

endmod.		%% builtins.pro -- Main File for builtins
