/*=====================================================================*
 |		atr_out.pro
 |	Copyright (c) 1990-94 Applied Logic Systems, Inc.
 |
 |	-- concrete assembly language translator output (M88k)
 |
 | Author:	Kevin A. Buettner
 | Creation:	4/12/90
 | Revision History:
 |		5/8/91	kev		88k port
 *=====================================================================*/

%
% insuffix/1 and outsuffix/1 determine the suffixes for input and output
% filenames.  This information is used by the driver to construct file names.
%

insuffix('.88k').
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

    :- consult(atr_label).
    
    label(empty(_),Tab) :-
	!,
	tab.
    label(llab(I),Tab) :-
	!,
	local_label_prefix(LPrefix),
	write(LPrefix),write(I),write(':'),
	tab.
    label(Other,Tab) :-
	write(Other),
	write(':'),
	tab.
    
    /*
     * opcode/1		-- emit the opcode
     */
    
    opcode(empty) :- !.
    opcode(opcode(Op,Suffix)) :-
	translate_op(Op,OutOp),
	!,
	write(OutOp),
	suffix(Suffix),
	tab.
    
    /*
     * suffix/1		-- emit the suffix (if any)
     */
    
    suffix(none) :- !.
    suffix(Other) :-
	write('.'),
	write(Other).

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
    operand(Operand,Tab) :- operand(Operand).
    
    operand(reg(R)) :- !, write(R).
    operand(fcr(R)) :- !, write(R).
    operand(cr(R)) :- !, write(R).
    operand(scale(R1,R2)) :- !, write(R1),write([R2]).
    operand(wo(0,Offset)) :- !, write('<'), expression(Offset), write('>').
    operand(wo(Width,Offset)) :- 
			!,
			expression(Width), 
			write('<'),
			expression(Offset),
			write('>').
    operand(cond1(Cond)) :- write(Cond).
    operand(cond2(Cond)) :- write(Cond).
    operand(Other) :- expression(Other).

    expression(hi16(E)) :- !, write('hi16('),o(E),write(')').
    expression(lo16(E)) :- !, write('lo16('),o(E),write(')').
    expression(llab(I)) :- !, local_label_prefix(L),write(L),write(I).
    expression(E) :- o(E).

    /*
     * output an expression
     */

    o(Exp) :-
	o(Exp,10).

    o(Exp,Lev) :-
	number(Exp),
	!,
	write(Exp).
    o(Exp,Lev) :- 
	atom(Exp),
	!,
	write(Exp).
    o(Exp,Lev) :- 
	functor(Exp,F,A), 
	is_op(F,A,OLev,Assoc,OFunc),
	!,
	o(Assoc,Lev,OLev,OFunc,Exp).
    o(Exp,Lev) :-
	write(illegal(Exp)).

    o(Assoc,Lev,OLev,OFunc,Exp) :-
	Lev < OLev,
	!,
	write('('),
	o(Assoc,10,OLev,OFunc,Exp),
	write(')').

    o(fy,_,OLev,OFunc,Exp) :-
	!,
	write(OFunc),
	arg(1,Exp,A1),
	o(A1,OLev).
    o(yfx,_,OLev,OFunc,Exp) :-
	!,
	arg(1,Exp,A1),
	o(A1,OLev),
	write(OFunc),
	POLev is OLev-1,
	arg(2,Exp,A2),
	o(A2,POLev).

    is_op(-,1,1,fy,-).
    is_op(*,2,2,yfx,*).
    is_op(/,2,2,yfx,/).
    is_op(+,2,3,yfx,+).
    is_op(-,2,3,yfx,-).



    tab(N) :- N =< 0, !.
    tab(N) :- tab, NN is N-1, tab(NN).

    tab :- put(0'\t).
    space :- put(0' ).
    comma :- put(0',).

    commentChar(0';).

    comment(Comment,LN) :-
	commentChar(C),
	tab,
	put(C),
	space,
	write(Comment),
	space,
	write(LN),
	nl.



    translate_op(X,X).

endmod.	%% output
