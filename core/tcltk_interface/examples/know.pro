know(lemon, tart).
know(candy, sweet).
know(quinine, bitter).
know(pretzels, salty).
know(sky, blue).
know(taxes, inevitable).
know(politicians, scoundrels).
know(_, unknown).
know(unknown, _).

%:- load_slib(tclintf), tcl_new(i).
:- setup_tcltk.

	
go :-
	tcl_eval(shl_tcli, 'source know.tcl', _),
	tk_main_loop.
