/*=================================================================*
 |			html_forms.pro
 |		Copyright (c) 1999 Applied Logic Systems, Inc.
 |	
 |		Decompose input from an HTML Form construct
 |
 |	Authors: Ken Bowen & Chuck Houpt
 |
 |	This software is copyrighted by Applied Logic Systems, Inc.,
 |	and other parties.  The following terms apply to all files 
 |	associated with the software unless explicitly disclaimed in 
 |	individual files.
 |
 |	The authors hereby grant permission to use, copy, modify, distribute,
 |	and license this software and its documentation for any purpose, provided
 |	that existing copyright notices are retained in all copies and that this
 |	notice is included verbatim in any distributions. No written agreement,
 |	license, or royalty fee is required for any of the authorized uses.
 |	Modifications to this software may be copyrighted by their authors
 |	and need not follow the licensing terms described here, provided that
 |	the new terms are clearly indicated on the first page of each file where
 |	they apply.
 |	
 |	IN NO EVENT SHALL THE AUTHORS OR DISTRIBUTORS BE LIABLE TO ANY PARTY
 |	FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES
 |	ARISING OUT OF THE USE OF THIS SOFTWARE, ITS DOCUMENTATION, OR ANY
 |	DERIVATIVES THEREOF, EVEN IF THE AUTHORS HAVE BEEN ADVISED OF THE
 |	POSSIBILITY OF SUCH DAMAGE.
 |	
 |	THE AUTHORS AND DISTRIBUTORS SPECIFICALLY DISCLAIM ANY WARRANTIES,
 |	INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY,
 |	FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THIS SOFTWARE
 |	IS PROVIDED ON AN "AS IS" BASIS, AND THE AUTHORS AND DISTRIBUTORS HAVE
 |	NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
 |	MODIFICATIONS.
 *=================================================================*/

module pxml.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Decomposing form input:
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export html_form_input/2.
export html_get_value/3.

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
html_form_input('', []) :-!.
html_form_input(Line, [Left | X])
	:-
	sub_atom(Line, Init,Mid,Final, '&'),
	!,
	sub_atom(Line, 0,Init,_,Left0),
	html_eq(Left0, Left),
	Init1 is Init+1,
	sub_atom(Line, Init1,_,0,RestLine),
	html_form_input(RestLine, X).
html_form_input(Line, [Item])
	:-
	html_eq(Line, Item).

html_eq(Line, Left=Right)
	:-
	sub_atom(Line, Init,Mid,Final, '='),
	!,
	sub_atom(Line, 0,Init,_,Left0),
	html_escapes(Left0, Left),
	Init1 is Init+1,
	sub_atom(Line, Init1,_,0,Right0),
	html_escapes(Right0, Right).
html_eq(Line, Line).

html_escapes(Item0, Item)
	:-
	sub_atom(Item0, _,_,_,'+'),
	!,
	atom_codes(Item0, I0Cs),
	process_html_escapes(I0Cs, ICs),
	atom_codes(Item, ICs).
html_escapes(Item0, Item)
	:-
	sub_atom(Item0, _,_,_,'%'),
	!,
	atom_codes(Item0, I0Cs),
	process_html_escapes(I0Cs, ICs),
	atom_codes(Item, ICs).
html_escapes(Item, Item).

process_html_escapes([], []).
process_html_escapes([0'+ | I0Cs], [0'  | ICs])
	:-!,
	process_html_escapes(I0Cs, ICs).
process_html_escapes([0'%,C1,C2 | RestI0Cs], [C | ICs])
	:-!,
	hex_decimal_digit(C1, D1),
	hex_decimal_digit(C2, D2),
	C is D1 * 16 + D2,
	process_html_escapes(RestI0Cs, ICs).
process_html_escapes([C | I0Cs], [C | ICs])
	:-
	process_html_escapes(I0Cs, ICs).

hex_decimal_digit(HexCC, DecCC)
	:-
	HexCC >= 0'A,
	!,
	DecCC is ((HexCC /\ 0xdf) - 0'A) + 10.
hex_decimal_digit(HexCC, DecCC)
	:-
	DecCC is HexCC - 0'0.

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
html_get_value(Eqs, Tag, Val)
	:-
	dmember(Tag=Val0, Eqs),
	Val0 \= '',
	!,
	atomread(Val0, Val, [vars_and_names(Vs,Ns)]), Vs=Ns.
html_get_value(_, _, '').

endmod.
