/*=====================================================================*
 |			dcgs.pro     
 |	Copyright (c) 1986-1996 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |			-- dcg expander for ALS-Prolog
 |
 | Author: Kenneth A. Bowen
 | Creation: 4/28/86
 | Revision History:
 | 05/29/86 - K. Buettner - incorporated into the builtins
 | 03/18/87 - K. Buettner - switched Module argument for SUN port
 *=====================================================================*/
module builtins.

expand(Module,Pat,VL) :-
	compiletime,			% was an $icode call, chris 7/10/88
	expand_(Module,Pat,VL).

expand_(Module,Pat,VL) :-
	dcg_expand(Pat,Clause),
	!,				% cut added, kev 1/31/90
	addclause(Module,Clause).
expand_(Module,Pat,VL) :-               % default case - assert thing, chris 7/28/88
	addclause(Module,Pat).

export dcg_expand/2.
dcg_expand((H --> B), ReturnClause) :- 
   expand_head_or_goal(H,RH,InV,OutV),
   expand_body(B,RB,InV,OutV),
   ( RB=true, !, ReturnClause=RH ; 
                 ReturnClause=(RH :- RB)
   ).

expand_head_or_goal(Head, ReturnHead, InV, OutV) :-
   functor(Head, HeadFunctor, Arity),
   Arity_plus_1 is Arity+1,
   Arity_plus_2 is Arity+2,
   functor(ReturnHead, HeadFunctor, Arity_plus_2),
   arg(Arity_plus_1,ReturnHead,InV),
   arg(Arity_plus_2,ReturnHead,OutV),
   install_args(Arity,Head,ReturnHead).

install_args(0,_,_) :- !.
install_args(N,S1,S2) :-
   arg(N,S1,ArgN),
   arg(N,S2,ArgN),
   NP is N-1,
   install_args(NP,S1,S2).


expand_body( Var, (call(Var),InV=OutV), InV,OutV) :- 
   var(Var), 
   !.
expand_body( [], true, OutV, OutV) :- !.
expand_body('$$'(String), true, InV, OutV) :-
   !,
   append(String, OutV, InV).
expand_body([ListHead | ListTail], true, InV, OutV) :-
   !,
   append([ListHead | ListTail], OutV, InV).
expand_body( !, (!,InV=OutV), InV, OutV) :- !.
expand_body( (B1,B2), OutGoal, InV, OutV) :-
   !,
   expand_body(B1, OutB1, InV, ConnectingV),
   expand_body(B2, OutB2, ConnectingV, OutV),
   goal_concat(OutB1,OutB2,OutGoal).
expand_body( { UntouchableGoals }, (UntouchableGoals,InV=OutV),InV,OutV) :-
   !.
expand_body( (Alt1 ; Alt2), (OutAlt1 ; OutAlt2), InV, OutV) :-
   !,
   expand_body(Alt1, OutAlt1temp, InV1, OutV),
   expand_body(Alt2, OutAlt2temp, InV2, OutV),
   goal_concat(InV=InV1,OutAlt1temp,OutAlt1),
   goal_concat(InV=InV2,OutAlt2temp,OutAlt2).
expand_body( UnTouchable, UnTouchable, V, V) :-
   untouchable(UnTouchable),
   !.
expand_body( Expandable, Expansion, InV, OutV) :-
   expand_head_or_goal(Expandable,Expansion,InV,OutV).

goal_concat(true,G2,G2) :- !.
goal_concat(G1,true,G1) :- !.
goal_concat(G1,G2,(G1,G2)).

untouchable(true).
untouchable(fail).
untouchable(var(_)).
untouchable(nonvar(_)).
untouchable(atomic(_)).
untouchable(atom(_)).
untouchable(integer(_)).
untouchable(_=_).
untouchable(_ is _).
untouchable(_<_).
untouchable(_>_).
untouchable(_>=_).
untouchable(_=<_).
untouchable(_=:=_).
untouchable(_=\=_).
untouchable(_==_).
untouchable(_\==_).

untouchable('$dbg_aph'(_,_,_)).
untouchable('$dbg_aphe'(_,_,_)).
untouchable('$dbg_apg'(_,_,_)).
untouchable('$dbg_apge'(_,_,_)).
untouchable('$dbg_apf'(_,_,_)).

endmod.
