/*========================================================================*
 |		tcl_sppt.pro
 |	Copyright (c) 1997 Applied Logic Systems, Inc.
 |
 |		Tcl/Tk Interface support routines
 |
 |	Initial Author:	Ken Bowen
 |	Date:	1997
 *========================================================================*/
module tk_alslib.

/*------------------------------------------------------------*
 |  mk_tcl_atom_list/2
 |  mk_tcl_atom_list(ProList, TclList)
 |  mk_tcl_atom_list(+, -)
 |
 |  ProList = Prolog list of atoms of form [A1,A2,...]
 |  TclList = atom of the form '{A1} {A2} ... '
 *------------------------------------------------------------*/
export mk_tcl_atom_list/2.
mk_tcl_atom_list([], "").
mk_tcl_atom_list(InProList, TclList)
	:-
	open(atom(TclList),write,TLS,[]),
	write_tcl_atom_list(InProList, TLS),
	close(TLS).
						
write_tcl_atom_list([], TLS).
write_tcl_atom_list([Item | InProList], TLS)
	:-
	printf(TLS, '{%t} ', [Item]),
	write_tcl_atom_list(InProList, TLS).

/*------------------------------------------------------------*
 |  mk_tcl_term_list/2
 |  mk_tcl_term_list(ProList, TclList)
 |  mk_tcl_term_list(+, -)
 |
 |  ProList = Prolog list of terms of form [A1,A2,...]
 |  TclList = atom of the form '{A1} {A2} ... '
 *------------------------------------------------------------*/
export mk_tcl_term_list/2.
mk_tcl_term_list([], "").
mk_tcl_term_list(InProList, TclList)
	:-
	open(atom(TclList),write,TLS,[]),
	write_tcl_term_list(InProList, TLS),
	close(TLS).
						
write_tcl_term_list([], TLS).
write_tcl_term_list([Term | InProList], TLS)
	:-
	printf(TLS, '{%t} ',[Term]),
	write_tcl_term_list(InProList, TLS).

endmod.
