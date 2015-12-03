/*=================================================================*
 |			pxml.pro
 |		Copyright (c) 1999-2004 Applied Logic Systems, Inc.
 |	
 |		Manipulations of pxml terms
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

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Output:  PXML --> HTML
	%%%% Prolog Markup Language to eXtensible Markup Language
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

export writeXml/2.
export writeXml/3.
export pml2xml/4.
export mpml2xml/2.
export mpml2xml_list/3.
export fetch_url/2.

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
writeXml(L, S)
	:-
	pml2xml(L, S, '  ', []).
/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
writeXml(L, S, Indent)
	:-
	pml2xml(L, S, Indent, []).

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
pml2xml([], S, _, _).
pml2xml([T | RT], S, Ind, IStack) 
	:-
	T = [_|_],
	!,
	pml2xml(T, S, Ind, IStack),
	pml2xml(RT, S, Ind, IStack).

pml2xml([T | RT], S, Ind, IStack) 
	:-!,
	pml2xml(T, S, Ind, IStack),
	nl(S), 
	pml2xml(RT, S, Ind, IStack).
pml2xml(comment(L), S, Ind, IStack) 
	:-!,
	write(S, '<--!'),nl(S),
	pml_cmt(L,S),
	write(S, '-->').
pml2xml(T, S, Ind, IStack) 
	:-
	functor(T, Functor, Arity),
	out_istack(IStack, S),
	pml2xml0(Arity, Functor, T, S, Ind, [Ind | IStack]),
	!.
pml2xml(T, S, Ind, IStack) 
	:-
	copy_term(pml(error(CT)), ThrowTerm),
	throw(ThrowTerm).

pml2xml0(0, newline, _, S, Ind, IStack) 
	:-!,
	nl(S). 
pml2xml0(0, Functor, _, S, Ind, IStack) 
	:-!,
	write(S, Functor).
pml2xml0(1, Functor, comment(L), S, Ind, IStack) 
	:-!,
	write(S, '<!--'),nl(S),
	pml_cmt(L,S),
	write(S, '-->').
pml2xml0(2, Functor, comment(_,L), S, Ind, IStack) 
	:-!,
	write(S, '<!--'),nl(S),
	pml_cmt(L,S),
	write(S, '-->').
pml2xml0(1, Functor, _, S, Ind, IStack) 
	:-!,
	write(S, '<'),write(S, Functor), write(S, '>').
pml2xml0(2, Functor, T, S, Ind, IStack) 
	:-
	unary_tag(Functor),
	!,
	write(S, '<'),write(S, Functor),
	arg(1, T, Options),
	write_options(Options, S),
%	pml2xml([], S),
	write(S, '>'),
	nl(S). 
pml2xml0(2, Functor, T, S, Ind, IStack) 
	:-!,
	write(S, '<'),write(S, Functor),
	arg(1, T, Options),
	write_options(Options, S),
	arg(2, T, B),
	(B = [] ->
		write(S, '/>')
		;
		write(S, '>'),
		nl(S), 
		pml2xml(B, S, Ind, IStack),
		pop_istack(IStack, PoppedIStack),
		out_istack(PoppedIStack, S),
		write(S, '</'), write(S, Functor), write(S, '>')
	).
pml2xml0(N, Functor, T, S, Ind, IStack) 
	:-
	copy_term(pml(bad_arity(CT)), ThrowTerm),
	throw(ThrowTerm).

write_options([], _).
write_options([O | RO], S) 
	:-
	write(S, ' '), 
	write_option(O, S, Ind, IStack), 
	write_options(RO, S).
write_options(R, S)
	:-
	copy_term(pml(improper_options_list(R)),ThrowTerm),
	throw(ThrowTerm).

write_option(T=V, S, Ind, IStack)
	:-!,
	escape_quotes(V, XV),
	printf(S, '%t="%t"',[T,XV]).
write_option(O, S, Ind, IStack)
	:-
	write(S, O). 

	%% Need proper implementation:
escape_quotes(V, V).

unescape_quotes(V, V).


pml_cmt([], _).
pml_cmt([T | L], S)
	:-
	write(S, T), nl(S), 
	pml_cmt(L, S).

out_istack([], _).
out_istack([Item | IStack], S)
	:-
	write(S, Item),
	out_istack(IStack, S).

pop_istack([_ | PoppedIStack], PoppedIStack) :-!.
pop_istack(IStack, IStack).

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Output:  Macro PXML --> XML 
	%%%% Prolog Markup Language to eXtensible Markup Language
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
mpml2xml([], S).
mpml2xml([T | RT], S) 
	:-
	macro_expand(T, NT),
%	pml2xml([NT], S),
	pml2xml([NT], S, '  ', []).
	mpml2xml(RT, S).

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
mpml2xml_list([], Tail, Tail).
mpml2xml_list([T | RT], [NT | RR], Tail) 
	:-
	macro_expand(T, NT),
	mpml2xml_list(RT, RR, Tail).  


macro_expand([], []).
macro_expand([T | RT], [FT | RFT]) 
	:-
	macro_expand(T, FT),
	macro_expand(RT, RFT).
macro_expand(T, FT) 
	:-
	mpml:macro(T, NT),
	(NT = [_|_] ->
		macro_expand(NT, FT)
		;
		macro_expand_body(NT, FT)
	).
macro_expand(T, FT) 
	:-
	macro_expand_body(T, FT).

macro_expand_body(T, FT) 
	:-
	functor(T, Functor, Arity),
	macro_expand_body0(Arity, Functor, T, FT).

macro_expand_body0(0, F, T, T).
macro_expand_body0(1, F, T, T).
macro_expand_body0(2, F, T, NT) 
	:-
	arg(1, T, O),
	arg(2, T, B),
	macro_expand(B, NB),
	NT =.. [F, O, NB].

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%%%% Input: Fetching Documents
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*---------------------------------------------------------------------
 *--------------------------------------------------------------------*/
fetch_url(RequestDescription, Response)
	:-
	build_http_request(RequestDescription, HTTP_Request),
	dmember(host=Host, RequestDescription),
	(dmember(port=Port, RequestDescription) -> true ; Port = 80),
	(dmember(timeout=Timeout, RequestDescription) -> true ; default_timeout(Timeout)),
	nsocket(internet, stream, 0, Socket),
	open(nsocket(Socket), read, RS, []),
	open(nsocket(Socket), write, WS, []),
	nsocket_connect(Socket, Host, Port),
	write_lines(WS, HTTP_Request),
	nl(WS),flush_output(WS),
%	nsocket_select([RS], [], [], [Mark], _, _, Timeout:0),
%	Mark = set,
	!,
	get_lines(RS, Response).


build_http_request(RequestDescription, HTTP_Request)
	:-
	(dmember(method=Method, RequestDescription) -> true ; Method = 'GET'),
	dmember(docpath=DocPath, RequestDescription),
	http_level(HTTPLev),
	sprintf(atom(HTTP_Request), '%t %t %t',[Method,DocPath,HTTPLev]). 

http_level('HTTP/1.0').


default_timeout(300).  %% time in secs

endmod.

