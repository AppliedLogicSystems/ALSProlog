/*================================================================*
 |              int_cstr.pro
 |      Copyright (c) 1995 Applied Logic Systems, Inc.
 |      Copyright (c) 1995 Bell-Northern Research Ltd.
 |
 |	Lower level of the interval constraint subsystem
 |
 |      Original Author: William J. Older
 |      Original Creation Date: 08/27/92
 |      Relational Interval Arithmetic Subsystem of BNR Prolog V4.x
 |
 |  Revisions/Modifications: Ken Bowen
 |  Date begun: 05/10/95
 |
 |              Integration into ALS Prolog
 *================================================================*/

module rel_arith.

/*---------------------------------------------------------------
 *--------------------------------------------------------------*/
type_and_bounds(real(L,U),    real,    L, U).
type_and_bounds(real,         real,    L, U).
type_and_bounds(integer(L,U), integer, L, U).
type_and_bounds(integer,      integer, L, U).
type_and_bounds(boolean(L,U), boolean, 0, 1).
type_and_bounds(boolean,      boolean, 0, 1).

new_type_interval(Type,X)
	:-
	type_and_bounds(Type, BareType, L, U),
	interval_bound(lower,L,L1,BareType),
	interval_bound(upper,U,U1,BareType),
	freeze_goal_for(BareType, X, L1, U1, FreezeGoal),
%printf_opt('  -+- freezing [%t] ', [X],[lettervars(false)]), 
	freeze(X, FreezeGoal).
%printf_opt('freeze: %t -> %t\n',[X, FreezeGoal], [lettervars(false),line_length(100)]).

		        %% Check with valid_domain for format:
freeze_goal_for(real,    X, L1, U1, intvl(real,   X,[],L1,U1) ).
freeze_goal_for(integer, X, L1, U1, intvl(integer,X,[],L1,U1) ).
freeze_goal_for(boolean, X, L1, U1, intvl(boolean,X,[],L1,U1) ).

new_combined_interval(Type, X)
	:-
	type_and_bounds(Type, BareType, L, U),
	interval_bound(lower,L,L1,BareType),
	interval_bound(upper,U,U1,BareType),
	freeze_goal_for(BareType, X, L1, U1, TypeFreezeGoal),
	'$delay_term_for'(X, DelayTerm),
	arg(4, DelayTerm, OrigDomainTerm),
	trailed_mangle(4, DelayTerm, (OrigDomainTerm, TypeFreezeGoal)).


/*---------------------------------------------------------------
 *--------------------------------------------------------------*/
interval_bound(Side, Quant, Bound, Type)
	:-
	var(Quant),
	!,
	max_bound(Type, MaxBound),
	interval_bound_decide(Side, MaxBound, Bound).

interval_bound(_,Quant, Bound, _)
	:-
	Bound is float(Quant).

interval_bound_decide(upper, MaxBound, MaxBound).
interval_bound_decide(lower, MaxBound, Bound)
	:-
	Bound is - MaxBound.

max_bound(real, 1.0e100).
max_bound(integer, Y)
	:-
	maxint(X),
	Y is float(X).

%% Temporary hack until we get the maxint prolog flag stuff implemented:
maxint(123456789).









	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Domain Queries
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domain_check(real, X)           :- domain(X,real(_,_)).
domain_check(integer, X)        :- domain(X,integer(_,_)).
domain_check(boolean, X)        :- domain(X,boolean(_,_)).

export domain/2.

domain(Var, Descriptor)
	:-
	'$domain'(Var, Type, LB, UB),
	Descriptor =.. [Type, LB, UB].

	%%%%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	%%%%  WARNING!!!!
	%%%%  Do NOT attempt to coalesce the first two goals of the
	%%%%  following clause into the following natural single
	%%%%  goal:
	%%%%      '$delay_term_for'(Var, '$delay'(_,_,_,DomainTerm)),
	%%%%
	%%%%  Since the term '$delay'(_,_,_,DomainTerm) exists on the
	%%%%  heap, then when this is unified against the actual
	%%%%  delay term for Var, the (anonymous) initial variable
	%%%%  passes the delay variable test, so the anonymous
	%%%%  variable and Var are combined with combine_delays,
	%%%%  but this produces circulariy, .....
	%%%%    Also, replacing the arg(4...) call by a unification:
	%%%%
	%%%%    VarDelayTerm = '$delay'(_,_,_,DomainTerm),
	%%%%
	%%%%  has the same effect.
	%%%%-------------------------------------------------------
'$domain'(Var, Type, LB, UB)
	:-
	'$delay_term_for'(Var, VarDelayTerm),
	arg(4, VarDelayTerm, ConstraintTerm),
	domain_term_from_constr(ConstraintTerm, DomainTerm),
	valid_domain(DomainTerm, Type, LB, UB).

domain_term_from_constr((DomainTerm, _), DomainTerm) :-!.
domain_term_from_constr(DomainTerm, DomainTerm).

valid_domain(intvl(Type,Var,_,L,U), Type, LB, UB)
	:-!,
	LB is L,
	UB is U.

valid_domain(( A, B ), Type, LB, UB)
	:-
	valid_domain( A, Type, LB, UB).

%%%%%%%%%%%%% NODE BUILDING%%%%%%%%%%%%%%%

point_interval( N,  N)
	:-
	integer(N), !.

point_interval( 0.0,0.0)          % dont fuzz 0.0
	:- !.

point_interval( X, PX)
	:-
	fuzz_float(X,XL,XH),
	PX::real(XL,XH).


new_node(N)
	:-
%printf_opt('>==>new_node(%t)\n',[N], [lettervars(false),line_length(100)]),
	'$iterate'(N, link).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%hack as compare on temporary basis:
breadth_first_compare(Left, Right, Op)
	:-
	compare(COp, Left, Right),
	cadj(COp, Op).

cadj( = ,  @= ).
cadj( < ,  @< ).
cadj( > ,  @> ).

:-dynamic(it_debug_cq/0).
:-dynamic(it_debug_gl/0).

export tidc/0.
tidc :-
	it_debug_cq,
	!,
	abolish(it_debug_cq,0).
tidc :-
	assert(it_debug_cq).

export tidg/0.
tidg :-
	it_debug_gl,
	!,
	abolish(it_debug_gl,0).
tidg :-
	assert(it_debug_gl).

export '$iterate'/1.
'$iterate'(Goal)
	:-
	'$iterate'(Goal, _).

export '$iterate'/2.
'$iterate'(Goal, link)
	:-
	iterate_link([Goal | Tail],Tail,_).

iterate_link(Queue, T, T)
	:-
	var(Queue), !.

iterate_link([], T, T) :-!.

iterate_link([(QueueSegment, SegTail) | RestQueue], InTail, OutTail)
	:-!,
	iterate_link(QueueSegment, InTail, InterTail),
	!,
	iterate_link(RestQueue, InterTail, OutTail).

iterate_link([[Goal | Goals] | RestQueue], InTail, OutTail)
	:-!,
	iterate_link([Goal | Goals], InTail, InterTail_1),
	!,
	iterate_link(Goals, InterTail_1, InterTail_2),
	iterate_link(RestQueue, InterTail_2, OutTail).

iterate_link([[] | RestQueue], InTail, OutTail)
	:-!,
	iterate_link(RestQueue, InterTail, OutTail).

	%% Note: InTail is tail of RestQueue;
iterate_link([InitGoal | RestQueue], InTail, OutTail)
	:-
	add_change_flag(InitGoal, Goal, InitArgs, Flag),
		%% Debugging:
	(it_debug_cq ->
		printf_opt('->-trying: %t : %t\n', [InitGoal, Goal],[lettervars(false)]),
		printf_opt('   q-tail: %t\n', [RestQueue], 
			[lettervars(false),maxdepth(10),depth_computation(flat)]),
		debug_disp_goal('>',InitGoal)
		;
		true
	),
	call(Goal),
	!,
	(it_debug_cq ->
		debug_disp_goal('<',Goal)
		;
		true
	),
	changes(Flag, XChange, YChange, ZChange),
	consis_update_node(ZChange, 1, InitGoal, ChangedNodes, CNT1),
	consis_update_node(XChange, 2, InitGoal, CNT1, CNT2),
	consis_update_node(YChange, 3, InitGoal, CNT2, []),

	queue_nodes(ChangedNodes, [InitGoal | RestQueue], InTail, NewTail),
	iterate_link(RestQueue, NewTail, OutTail).

add_change_flag(InitGoal, Goal, Args, Flag)
	:-
	InitGoal =.. [Pred | Args],
	append(Args, [Flag], XArgs),
	Goal =.. [Pred | XArgs].

debug_disp_goal(Tag,Goal)
	:-
	it_debug_gl,
	!,
	Goal =.. [Pred | Args],
	printf('%t ACL:pred=%t - ',[Tag,Pred]),
	debug_disp_alist(Args),
	nl.
debug_disp_goal(Tag,Goal).

debug_disp_alist([]).
debug_disp_alist([Arg])
	:-!,
	debug_disp_arg(Arg).
debug_disp_alist([Arg | Args])
	:-
	debug_disp_arg(Arg),
	put_atom(','),
	debug_disp_alist(Args).

debug_disp_arg(Arg)
	:-
	'$is_delay_var'(Arg),
	!,
	 '$domain'(Arg, Type, LB, UB),
	printf_opt('DV(%t,%t,%20.16g,%20.16g)',[Arg, Type, LB, UB],[lettervars(false)]).

debug_disp_arg(Arg)
	:-
	write(Arg).

/*---------------------------------------------------*
 |		Change flag information:
 |	-- see generic/intrv.h for original information:
 |
 |	Bits are set in the change flag as follows:
 |
 |	#define xlchng()            status |= xlchange
 |	#define xhchng()            status |= xhchange
 |	#define ylchng()            status |= ylchange
 |	#define yhchng()            status |= yhchange
 |	#define zlchng()            status |= zlchange
 |	#define zhchng()            status |= zhchange
 |	#define xflipped()          status |= xflip
 |	#define yflipped()          status |= yflip
 |	#define zflipped()          status |= zflip
 |	
 |	#define xflip       (1 << 0)
 |	#define xlchange    (1 << 1)
 |	#define xhchange    (1 << 2)
 |	#define yflip       (1 << 3)
 |	#define ylchange    (1 << 4)
 |	#define yhchange    (1 << 5)
 |	#define zflip       (1 << 6)
 |	#define zlchange    (1 << 7)
 |	#define zhchange    (1 << 8)
 | 
 |	Bits are numbere:
 |		...8 7 6 5 4 3 2 1 0
 |
 |	Consolidating this, we get:
 |
 |	Change recorded	|  Bit set
 |  ----------------|----------
 |		xl			|	1
 |		xh			|	2
 |		yl			|	4
 |		yh			|	5
 |		zl			|	7
 |		zh			|	8
 |
 |	Note:	
 |		x mask is: (1 << 1) \/ (1 << 2) = 6
 |		y mask is: (1 << 3) \/ (1 << 4) = 24
 |		z mask is: (1 << 7) \/ (1 << 8) = 384
 *---------------------------------------------------*/

changes(Flag, XChange, YChange, ZChange)
	:-
	XChange is Flag /\ 6,
	YChange is Flag /\ 24,
	ZChange is Flag /\ 384.

consis_update_node(CFlg, ArgNum, Goal, ChangedNodes, ChangedNodesTail)
	:-
	arg(ArgNum,Goal,Arg),
	'$is_delay_var'(Arg),
	!,

		%%	'$domain'(Arg, _, LB, UB):
	'$delay_term_for'(Arg, ArgDelayTerm),
	arg(4, ArgDelayTerm, ConstraintTerm),
	domain_term_from_constr(ConstraintTerm, DomainTerm),
		%% The interval is consistent, and things
		%% changed here, so propagate change, and
		%% make sure Goal Node is on interval's 
		%% UsedBy list:
	arg(3, DomainTerm, UsedByList),
	fin_cun(CFlg, UsedByList, Goal, DomainTerm, ChangedNodes, ChangedNodesTail).

consis_update_node(_, _, _, ChangedNodes, ChangedNodes).

fin_cun(0, UsedByList, Goal, DomainTerm, ChangedNodes, ChangedNodes)
	:-!,
	(check_used(UsedByList, Goal) ->
		trailed_mangle(3, DomainTerm, [Goal | UsedByList])
		;
		true
	).

fin_cun(CFlg, UsedByList, Goal, DomainTerm, ChangedNodes, ChangedNodesTail)
	:-
	check_add_make(UsedByList, Goal, FoundFlag, NewUsed, 
					ChangedNodes, ChangedNodesTail),
	(FoundFlag = false ->
		trailed_mangle(3, DomainTerm, NewUsed) ; true ).
	
check_used([], Goal).

check_used([CNode | UsedByList], Goal)
	:-
	non_unif_match(CNode, Goal),
	!,
	fail.

check_used([CNode | UsedByList], Goal)
	:-
	check_used(UsedByList, Goal).

check_add_make([], Goal, FoundFlag, [Goal], ChgdNodesTl, ChgdNodesTl)
	:-
	var(FoundFlag),
	!,
	FoundFlag = false .

check_add_make([], _, _, [], ChangedNodesTail, ChangedNodesTail)
	:-!.

check_add_make([CNode | UsedByList], Goal, true, [CNode | NewUsed], 
					[CNode | ChangedNodes], ChangedNodesTail)
	:-
	non_unif_match(CNode, Goal),
	!,
	check_add_make(UsedByList, Goal, FoundFlag, NewUsed, 
					ChangedNodes, ChangedNodesTail).

check_add_make([CNode | UsedByList], Goal, FoundFlag, [CNode | NewUsed], 
					[CNode | ChangedNodes], ChangedNodesTail)
	:-
	check_add_make(UsedByList, Goal, FoundFlag, NewUsed, 
					ChangedNodes, ChangedNodesTail).

non_unif_match(Left,Right)
	:-
	var(Left),
	!,
	(Left == Right -> true ; fail).

non_unif_match(Left,Right)
	:-
	atomic(Left),
	Left == Right,
	!.

non_unif_match( [LH | LT], [RH | RT])
	:-!,
	non_unif_match( LH, RH),
	non_unif_match( LT, RT).

non_unif_match(Left,Right)
	:-
	Left =..  [LF | LArgs],
	Right =.. [RF | RArgs],
	LF == RF,
	non_unif_match( LArgs, RArgs).


non_unif_mem(Queue, Node)
	:-
	var(Queue),!, fail.

non_unif_mem([Item | Queue], Node)
	:-
	non_unif_match(Item, Node),
	!.

non_unif_mem([_ | Queue], Node)
	:-
	non_unif_mem(Queue, Node).




queue_nodes(X, _, Tail, Tail)
	:-
	var(X).

queue_nodes([], _, Tail, Tail).

queue_nodes([Node | ChangedNodes], QHead, InterTail, NewTail)
	:-
	non_unif_mem(QHead, Node),
	!,
	queue_nodes(ChangedNodes, QHead, InterTail, NewTail).

queue_nodes([Node | ChangedNodes], QHead, [Node | InterTail], NewTail)
	:-
	queue_nodes(ChangedNodes, QHead, InterTail, NewTail).




	%%% Temporary Hack:
trailed_mangle(N, Term, Val)
	:-
	arg(N, Term, OldVal),
	tm4(N, Term, Val, OldVal).

tm4(N, Term, Val, OldVal)
	:-
	mangle(N, Term, Val).
	
tm4(N, Term, Val, OldVal)
	:-
	mangle(N, Term, OldVal),
	fail.
	





	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
	%%%%% DEBUGGING STUFF
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

show_the_goal(Goal,InitArgs)
	:-
	printf_opt('%t\n',[Goal],[lettervars(false)]),
	show_the_goal_args(InitArgs,1).

show_the_goal_args([],_).
show_the_goal_args([Arg | Args],N)
	:-
	var(Arg),
	'$delay_term_for'(Arg, DT),
	!,
	printf_opt('%d: %t : %t\n',[N,Arg,DT],[lettervars(false)]),
	M is N+1,
	show_the_goal_args(Args,M).
show_the_goal_args([Arg | Args],N)
	:-
	printf_opt('%d: %t\n',[N,Arg],[lettervars(false)]),
	M is N+1,
	show_the_goal_args(Args,M).









endmod.
