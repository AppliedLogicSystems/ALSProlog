/*=====================================================================
 | 			misc_io.pro		
 |	Copyright (c) 1990-96 Applied Logic Systems, Inc.
 |		Distribution rights per Copying ALS
 |
 |		Various I/O utilities 
 *====================================================================*/
module builtins.

export read_terms/1.
export read_terms/2.
export read_terms_pos/1.
export read_terms_pos/2.
export read_terms_pos/3.
export read_terms_vn/2.
export read_as_list/3.
export grab_terms/2.
export grab_lines/2.
export get_lines/2.

/*!-------------------------------------------------------------
 |	read_terms/1
 |	read_terms(Term_List)
 |	read_terms(-)
 |
 |	-	reads a list of Prolog terms from the default input stream
 |
 |	Reads a list (Term_List)  of all terms which can be read from
 |	the default input stream
 *!------------------------------------------------------------*/

read_terms(Term_List)
	:-
	current_default_stream(input, Stream),
	read_terms(Term_List, Stream).

/*!-------------------------------------------------------------
 |	read_terms/2
 |	read_terms(Stream,Term_List)
 |	read_terms(+,-)
 |
 |	-	reads a list of Prolog terms from stream Stream
 |
 |	Reads a list (Term_List)  of all terms which can be read from
 |	the stream Stream.
 *!------------------------------------------------------------*/

read_terms(Stream,Term_List)
	:-
	read_terms0(Term_List, Term_List,Stream).

read_terms0(Term_List, List_Tail,Stream)
	:-
	read(Stream, Next_Term),
	!,
	dispatch_read_terms0(Next_Term, Term_List, List_Tail,Stream).

dispatch_read_terms0(Next_Term, Term_List, List_Tail,Stream)
	:-
	var(Next_Term),!,
	List_Tail = [Next_Term | New_List_Tail],
	!,
	read_terms0(Term_List, New_List_Tail,Stream).
dispatch_read_terms0(end_of_file, _, [],_) :-!.
dispatch_read_terms0(Next_Term, Term_List, List_Tail,Stream)
	:-
	List_Tail = [Next_Term | New_List_Tail],
	!,
	read_terms0(Term_List, New_List_Tail,Stream).

/*!-------------------------------------------------------------
 |	read_terms_pos/1
 |	read_terms_pos(Term_List)
 |	read_terms_pos(-)
 |
 |	-	reads list of terms, with positions, from default input stream
 |
 |	Reads a list (Term_List)  of all terms which can be read from
 |	the default input stream, with their positions;
 *!------------------------------------------------------------*/

read_terms_pos(Term_List)
	:-
	current_default_stream(input, Stream),
	read_terms_pos(Term_List, Stream).

/*!-------------------------------------------------------------
 |	read_terms_pos/2
 |	read_terms_pos(Stream,Term_List)
 |	read_terms_pos(+,-)
 |
 |	-	reads list of terms, with positions, from stream Stream
 |
 |	Reads a list (Term_List)  of all terms, with positions  which 
 |	can be read from the stream Stream; the elements of Term_List
 |	are of the form (Term, Start, End), where:
 |	- Start is the offset in Stream of the first character of Term;
 |		if the offset cannot be meaninfully calculated, Start = -1;
 |	- End is the offset of the first character position following
 |		Term in Stream;
 *!------------------------------------------------------------*/

read_terms_pos(Stream, Term_List)
	:-
	read_terms_pos(Stream, Term_List, _).

/*!-------------------------------------------------------------
 |	read_terms_pos/3
 |	read_terms_pos(Stream, Term_List, OpDecls)
 |	read_terms_pos(+, -, -)
 |
 |	- reads list of terms with pos from Stream, with op declarations
 |
 |	Reads a list (Term_List)  of all terms, with positions which 
 |	can be read from the stream Stream; the elements of Term_List
 |	are of the form (Term, Start, End), where:
 |	- Start is the offset in Stream of the first character of Term;
 |		if the offset cannot be meaninfully calculated, Start = -1;
 |	- End is the offset of the first character position following
 |		Term in Stream;
 |
 |	In addition: Any operator declaration (:-op(...) ) which 
 |	is encountered is treated as follows:
 |	a.  When encountered, if the op declaration is not
 |		currently valid, it is executed, and the declaration
 |		which was executed is returned on OpDecls;
 |	c.  When read_terms_pos returns, it undoes all the 
 |		op declarations on OpDecls are undone.
 *!------------------------------------------------------------*/

read_terms_pos(Stream,Term_List, OpDecls)
	:-
	read_terms_pos_opd(Stream,Term_List, OpDecls),
	pbi_write(undoing(OpDecls)),pbi_nl,pbi_ttyflush,
	undo_op_decls(OpDecls).

read_terms_pos_opd(Stream,Term_List, OpDecls)
	:-
	sio:skip_layout(Stream),
	sio_getpos(Stream,StartPos),
	read_term(Stream, Next_Term, []),
	sio_getpos(Stream,LastPos),
	dispatch_read_terms_pos_opd(Next_Term, StartPos, LastPos, 
							Term_List, Stream, OpDecls).

dispatch_read_terms_pos_opd(Next_Term, StartPos, LastPos, 
							Term_List, Stream, OpDecls)
	:-
	var(Next_Term),
	!,
	Term_List = [(Next_Term,StartPos,LastPos) | List_Tail],
	read_terms_pos_opd(Stream, List_Tail, OpDecls).

dispatch_read_terms_pos_opd(end_of_file, _, _, [],_, []) :-!.

dispatch_read_terms_pos_opd(Next_Term, StartPos, LastPos, 
							Term_List, Stream, OpDecls)
	:-
	Term_List = [(Next_Term,StartPos,LastPos) | List_Tail],
	!,
	op_decl_checking(Next_Term, OpDecls, NewOpDecls),
	read_terms_pos_opd(Stream, List_Tail, NewOpDecls).

op_decl_checking(Next_Term, OpDecls, NewOpDecls)
	:-
	Next_Term = (:- Goals),
	xtr_op_decls(Goals, CandidateOpDecls),
	ck_exec_ops(CandidateOpDecls, ExecutedOpDecls),
	append(ExecutedOpDecls, NewOpDecls, OpDecls).

op_decl_checking(_, OpDecls, OpDecls).

xtr_op_decls((op(A,B,C), Goals), [op(A,B,C) |  CandidateOpDecls])
	:-!,
	xtr_op_decls(Goals, CandidateOpDecls).

xtr_op_decls((_, Goals), CandidateOpDecls)
	:-!,
	xtr_op_decls(Goals, CandidateOpDecls).

xtr_op_decls(op(A,B,C), [op(A,B,C)]) :-!.

xtr_op_decls(_, []).

ck_exec_ops([], []).

ck_exec_ops([op(Pri,Spc,Opr) | CandidateOpDecls], 
			[op(Pri,Spc,Opr) | ExecutedOpDecls])
	:-
	op_ck_exec(Pri,Spc,Opr),
	!,
	ck_exec_ops(CandidateOpDecls, ExecutedOpDecls).

ck_exec_ops([_ | CandidateOpDecls], ExecutedOpDecls)
	:-
	ck_exec_ops(CandidateOpDecls, ExecutedOpDecls).

op_ck_exec(Priority,Op_specifier,Operator)
	:-
	current_op(Priority,Op_specifier,Operator),
	!,
	fail.

op_ck_exec(Priority,Op_specifier,Operator)
	:-
	call(op(Priority,Op_specifier,Operator)).

undo_op_decls([]).
undo_op_decls([op(Pr,Spc,Opr) | OpDecls])
	:-
	op(0,Spc,Opr),
	undo_op_decls(OpDecls).

/*!-------------------------------------------------------------
 |	read_terms_vn/2
 |	read_terms_vn(Stream,Term_List)
 |	read_terms_vn(+,-)
 |
 |	-	reads list of terms from stream Stream, with vars instatiated
 |
 |	Reads a list (Term_List)  of all terms which can be read from
 |	the stream Stream, with all the variables in each term instatiated
 |	to their names.
 *!------------------------------------------------------------*/

read_terms_vn(Stream,Term_List)
	:-
	read_terms_vn0(Term_List, Term_List,Stream).

read_terms_vn0(Term_List, List_Tail,Stream)
	:-
	read_term(Stream, Next_Term, [ vars_and_names(Vars,Names) ]),
	Vars = Names,
	!,
	dispatch_read_terms_vn0(Next_Term, Term_List, List_Tail,Stream).

dispatch_read_terms_vn0(Next_Term, Term_List, List_Tail,Stream)
	:-
	var(Next_Term),!,
	List_Tail = [Next_Term | New_List_Tail],
	!,
	read_terms_vn0(Term_List, New_List_Tail,Stream).

dispatch_read_terms_vn0(end_of_file, _, [],_) :-!.

dispatch_read_terms_vn0(Next_Term, Term_List, List_Tail,Stream)
	:-
	List_Tail = [Next_Term | New_List_Tail],
	!,
	read_terms_vn0(Term_List, New_List_Tail,Stream).

/*!-------------------------------------------------------------
 |	grab_terms/2
 |	grab_terms(File, Terms)
 |	grab_terms(+, -)
 |
 |	- read terms from a file into a list
 |
 |	Opens File and reads list of Terms (as atoms) until end of 
 |	File is encountered.
 *!------------------------------------------------------------*/

grab_terms(SrcF,Terms)
	:-
	open(SrcF,read,SS,[]),
	read_terms(SS,Terms),
	close(SS).

/*!-------------------------------------------------------------
 |	grab_lines/2
 |	grab_lines(File, Lines)
 |	grab_lines(+, -)
 |
 |	- read lines from a file into a list
 |
 |	Opens File and reads list of Lines (as atoms) until end of 
 |	File is encountered.
 *!------------------------------------------------------------*/

grab_lines(SrcF,Lines)
	:-
	open(SrcF,read,SS,[]),
	get_lines(SS,Lines),
	close(SS).
				 
/*!-------------------------------------------------------------
 |	get_lines/2
 |	get_lines(Stream, Lines)
 |	get_lines(+, -)
 |
 |	- read lines from a stream into a list
 |
 |	Reads list of Lines (as atoms) from Stream until end of 
 |	Stream is encountered.
 *!------------------------------------------------------------*/

get_lines(SS,[Line | Lines])
	:-
	get_line(SS,Line),
	!,
	get_lines(SS, Lines).

get_lines(SS, []).

/*!-------------------------------------------------------------
 |	read_as_list/3
 |	read_as_list(Atom,List,Options)	
 |	read_as_list(+,-,+)
 |
 |	- process an input atom into a list, if possible	
 |
 |	Atom is a an atom, normally quoted; Options is a list of
 |	equations; currently the only supported element is
 |		delimiter=Cd,
 |	where Cd is the code of a character C.  If Atom actually appears
 |	as a sequence of readable Prolog terms separated by one or
 |	more occurrences of C, then List is the corresponding list
 |	of those terms.  Example:
 |		Atom = 'foo.bar zip.go  silly.jam'
 |		Options = [delimiter = 0' ],
 |		List = ['foo.bar','zip.go','silly.jam']
 *!------------------------------------------------------------*/

read_as_list(FLine,Fs,Options)
	:-
	atom_length(FLine,FLen),
	(dmember(delimiter=Delim, Options) -> true ; Delim = ' ' ),
	read_as_list(0,FLen,FLine,Delim,Fs,Options).

read_as_list(Cur,Size,Source,Delim,[],Options)
	:-
	Cur >= Size,
	!.

read_as_list(Cur,Size,Source,Delim,[Item | Rest],Options)
	:-
	read_item(Cur,Size,Source,Delim,Item,After),
	read_as_list(After,Size,Source,Delim,Rest,Options).

read_item(Cur,Size,Source,Delim,Next,After)
	:-
	probe_delim(Cur,Size,Source,Delim,Len,After),
	sub_atom(Source,Cur,Len,_, Next).

read_item(Cur,Size,Source,Delim,Next,Size)
	:-
	sub_atom(Source,Cur,_,0, Next).

probe_delim(Cur,Size,Source,Delim,Len,After)
	:-
	sub_atom(Source,B0,1,_,Delim),
	B0 > Cur,
	After is B0 +1,
	Len is B0 -Cur.



endmod.
