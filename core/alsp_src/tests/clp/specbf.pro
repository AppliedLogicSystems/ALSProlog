/*======================================================================*
 |		specbf.pro
 *======================================================================*/

module builtins.

:- module_closure(spec_b_findall,4,spec_b_findall).

spec_b_findall(Module,Template, Goal, Solns, NumSols)
	:-
    nonvar(Goal),
	integer(NumSols),
	NumSols > 0,
	S = [[]],
	NN = n(0),
	get_vars(Template,[],Vars),
	spec_b_fa_(Module,Template,Goal,S,Vars,NN,NumSols),
	arg(1,S,Solns).
								 
spec_b_fa_(Module,Template,Goal,S, Vars,NN,Bnd)
	:-
	fa_call(Module,Goal),
	spec_fa_copy(Template,Copy,_),
	builtins:fa_add(S,Copy),
	spec_b_fa_incr(NN,Bnd,Vars),
	!.
					 
spec_b_fa_(_,_,_,_,_,_,_).
					  
spec_b_fa_incr(NN,Bnd,Vars)
	:-
	arg(1,NN,CN),
	NxtN is CN+1,
	NxtN < Bnd,
	!,
	mangle(1,NN,NxtN),
	fail.
													   
spec_b_fa_incr(_,_,_).
														
kill_freeze_list([]).
kill_freeze_list([Var | Vars])
	:-
	'$kill_freeze'(Var),
	kill_freeze_list(Vars).


spec_fa_copy(VIn,VOut,VAssoc) 
	:-
	'$is_delay_var'(VIn),
	domain(VIn, Descriptor),
	!,
	arg(1,Descriptor,Low), 
	arg(2,Descriptor,High), 
	VOut = [Low,High].

spec_fa_copy(VIn,VOut,VAssoc)
	:-
	var(VIn),
	!,
	fa_vassoc(VAssoc,VIn,VOut).
	
spec_fa_copy(Atom,Atom,VAssoc) :-
	atomic(Atom),
	!.
	
spec_fa_copy(SIn,SOut,VAssoc) :-
	functor(SIn,F,A),
	functor(SOut,F,A),
	spec_fa_copyargs(A,SIn,SOut,VAssoc).
									 
spec_fa_copyargs(0,_,_,VAssoc) :-
	!.

spec_fa_copyargs(N,SIn,SOut,VAssoc) :-
	arg(N,SIn,AIn),
	arg(N,SOut,AOut),
	spec_fa_copy(AIn,AOut,VAssoc),
	NP is N-1,
	spec_fa_copyargs(NP,SIn,SOut,VAssoc).

endmod.
