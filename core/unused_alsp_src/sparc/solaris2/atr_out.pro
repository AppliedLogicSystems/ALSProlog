/*=====================================================================*
 |		atr_out.pro	
 |	Copyright (c) 1990-94 Applied Logic Systems, Inc.
 |
 |	-- concrete assembly language translator output (sparc/solaris2)
 |
 | Author:	Kevin A. Buettner
 | Creation:	4/12/90
 | Revision History:
 |	1/10/91	-- out of version for M68k/sun
 *=====================================================================*/

%
% insuffix/1 and outsuffix/1 determine the suffixes for input and output
% filenames.  This information is used by the driver to construct file names.
%

insuffix('.sprc').
outsuffix('.s').


module output.
    use labels.
    export output/4.
    export output_end/1.
    export output_begin/1.

    output_begin(_).	%% do nothing

    output_end(_).	%% do nothing

    output(error,LN,Tab,Tab) :- !, comment('Error on line',LN).
    output(statement(Label,Opcode,Operands),LN,Tab,Tab) :-
        output2(Label,Opcode,Operands,Tab),
	comment(' ',LN).

    output2(Label,Opcode,Operands,Tab) :-
	label(Label,Tab),
	opcode(Opcode),
	operands(Operands,Tab).
    
    /*
     * label/1		-- emit a label
     */
    
    label(empty(_),Tab) :-
	!,
	tab.
    label(llab(I),Tab) :-
	!,
	write('L'),write(I),write(':'),
	tab.
    label(Other,Tab) :-
	write(Other),
	write(':'),
	tab.
    
    /*
     * opcode/1		-- emit the opcode
     */
    
    opcode(empty) :- !.
    opcode(Opcode) :-
	translate_op(Opcode,OutOp),
	!,
	write(OutOp),
	tab.
    

    /*
     * operands/2	-- emit the operands
     */
    
    operands([],Tab) :- tab(4),!.
    operands([Op|Rest],Tab) :-
	operand(Op,Tab),
	more_operands(Rest,TL,Tab),
	tab(TL).
    
    more_operands([],3,Tab) :- !.
    more_operands([Op|Rest],TL,Tab) :-
	comma,
	tab,
	operand(Op,Tab),
	more_operands(Rest,PTL,Tab),
	TL is PTL-1.
    
    /*
     * operand/2	-- emit one operand
     */
    
    operand(mem(O),Tab) :- !, write('['), operand(O,Tab), write(']').
    operand(reg(R),Tab) :- !, write('%'), write(R).
    operand(freg(F),Tab) :- !, write('%'), write(F).
    operand(llab(I),Tab) :- !, expression(llab(I),Tab).
    operand(regaddr(R1,R2),Tab) :- !, 
			write('%'), write(R1), write('+%'), write(R2).
    operand(addr(R,-C),Tab) :- !,
			write('%'), write(R), write('-'), expression(C,Tab).
    operand(addr(R,C),Tab) :-  !,
			write('%'), write(R), write('+'), 
			expression(C,Tab).
    operand(exp(Exp),Tab) :- expression(Exp,Tab).
    

    expression(dot(I),Tab) :- !, write('.'), write(I).
    expression(llab(I),Tab) :- !, write('L'),write(I).
    expression(hi22(E),Tab) :- !, write('%hi('),o(E,Tab),write(')').
    expression(lo10(E),Tab) :- !, write('%lo('),o(E,Tab),write(')').
    expression(Number,_) :- number(Number), !, write(Number).
    expression(Atom,_) :- atom(Atom),!,write(Atom).
    expression(Other,Tab) :- write('('),o(Other,Tab),write(')').

    o(Exp,Tab) :-
	o(Exp,10,Tab).

    o(Exp,Lev,Tab) :-
	number(Exp),
	!,
	write(Exp).
    o(Exp,Lev,Tab) :- 
	atom(Exp),
	!,
	write(Exp).
    o(Exp,Lev,Tab) :- 
	functor(Exp,F,A), 
	is_op(F,A,OLev,Assoc,OFunc),
	!,
	o(Assoc,Lev,OLev,OFunc,Exp,Tab).
    o(Exp,Lev,Tab) :-
	write(illegal(Exp)).

    o(Assoc,Lev,OLev,OFunc,Exp,Tab) :-
	Lev < OLev,
	!,
	write('('),
	o(Assoc,10,OLev,OFunc,Exp,Tab),
	write(')').

    o(fy,_,OLev,OFunc,Exp,Tab) :-
	!,
	write(OFunc),
	arg(1,Exp,A1),
	o(A1,OLev,Tab).
    o(yfx,_,OLev,OFunc,Exp,Tab) :-
	!,
	arg(1,Exp,A1),
	o(A1,OLev,Tab),
	write(OFunc),
	POLev is OLev-1,
	arg(2,Exp,A2),
	o(A2,POLev,Tab).

    is_op(-,1,1,fy,-).
    is_op(*,2,2,yfx,*).
    is_op(/,2,2,yfx,/).
    is_op(+,2,3,yfx,+).
    is_op(-,2,3,yfx,-).



    tab(N) :- N =< 0, !.
    tab(N) :- tab, NN is N-1, tab(NN).

    tab :- put(0'\t).
    space :- put(0'  ).
    comma :- put(0',).

    comment(Comment,LN) :-
	commentChar(C),
	tab,
	put(C),
	space,
	write(Comment),
	space,
	write(LN),
	nl.



    %% sun specific stuff
    commentChar(0'!).

    translate_op(global,'.global').
    translate_op(text,'.seg "text"').
    translate_op(data,'.seg "data"').
    translate_op(word,'.word').
    translate_op(align,'.align').
    translate_op(skip,'.skip').
    translate_op(X,X).


endmod.	%% output
