module tcltk.

:- consult('tclintf.psl').

export tcl_new/1.
export tk_new/1.
export tcl_delete/1.
export tcl_delete_all/0.

export tcl_call/3.
export tcl_eval/3.

export tcl_coerce_atom/3.
export tcl_coerce_list/3.

export tk_main_loop/0.

export setup_tcltk/0.
setup_tcltk
	:-
	tk_new(shl_tcli).

/* Utility functions used by the C side of the interface. */

read_eval_results(Atom, Results) :-
	open(atom(Atom), read, Stream, []),
	read_term(Stream, Term, [variables(Results)]),
	close(Stream),
	call(Term).

eval_results(Module, Call, Vars) :-
	call(Module:Call).

term_to_string(Term, String) :-
	open(atom(String), write, S, []),
	write(S, Term),
	close(S).
endmod.
