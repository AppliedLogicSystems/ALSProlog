/*=====================================================================*
 |		atr_out.pro
 |	Copyright (c) 1990-94 Applied Logic Systems, Inc.
 |
 |	-- concrete assembly language translator output (vax/vms)
 |
 | Author:	Kevin A. Buettner
 | Creation:	4/12/90
 | Revision History:
 *=====================================================================*/

%%
%% insuffix/1 and outsuffix/1 determine the suffixes for input and output
%% filenames.  This information is used by the driver to construct file names.
%%

insuffix('.vax').
outsuffix('.mar').


module output.
    use labels.
    export output/4.
    export output_end/1.
    export output_begin/1.


    output_begin(ModuleName) :-
	write('	.TITLE	'), write(ModuleName),nl,
	write('	.ENABLE	GLOBAL, SUPPRESSION'), nl.

    output_end(_) :-
	write('	.END'), nl.

    output(error,LN,Tab,Tab) :- !, comment('Error on line',LN).
    output(statement(Label,Opcode,Operands),LN,Tab,Tab) :-
	output2(Label,Opcode,Operands,Tab),
	comment(' ',LN).
    
    output2(Label,Opcode,Operands,Tab) :-
	is_nop(Opcode,Operands),
	!,
	label(Label,Tab).
    output2(Label,Opcode,Operands,Tab) :-
	label(Label,Tab),
	opcode(Opcode),
	operands(Operands,Tab),
	opcode_trail(Opcode,Operands).
    
    is_nop(movl,[X,X]).

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
    
    translate_op(text,'.PSECT	ALSASM	NOWRT').
    translate_op(data,'.PSECT	ALSDATA WRT,RD,NOEXE').
    translate_op(long,'.LONG').
    translate_op(global,'.GLOBAL').
    translate_op(entry,'.ENTRY').
    translate_op(extern,'.PSECT').
    translate_op(X,X).

    /*
     * opcode_trail/1	-- emit stuff after operands for certain opcodes
     */
    
    /*
    opcode_trail(global,Rest) :-
	!,
	write(',usr,gbl,shr,long'),nl,
	tab,
	write('.GLOBAL	'), operands(Rest,_).
    */
    opcode_trail(extern,Rest) :-
	!,
	write('pic,usr,ovr,rel,gbl,shr,noexe,rd,wrt,novec,long'),nl,
	operands(Rest,_), write(':'),
	tab,
	write('.blkl').

    opcode_trail(_,_).
    
    /*
     * operands/2	-- emit the operands
     */
    
    operands([],Tab) :- tab(4), !.
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
    
    operand(Op,Tab) :- oper1(Op).

    oper1(index(B,Rx)) :-
	!,
	oper2(B),
	write('['),
	writeReg(Rx),
	write(']').
    oper1(Op) :- oper2(Op).

    oper2(deferred(B)) :-
	!,
	write('@'),
	oper3(B).
    oper2(Op) :- oper3(Op).

    oper3(reg(R)) :-
	!,
	writeReg(R).
    oper3(reg_deferred(R)) :-
	!,
	write('('),
	writeReg(R),
	write(')').
    oper3(autoincrement(R)) :-
	!,
	write('('),
	writeReg(R),
	write(')+').
    oper3(autodecrement(R)) :-
	!,
	write('-('),
	writeReg(R),
	write(')').
    oper3(disp(D,R,S)) :-
	!,
	disp_size(S),
	expression(D),
	write('('),
	writeReg(R),
	write(')').
    oper3(pcdisp(G,S)) :-
	!,
	disp_size(S),
	expression(G).
    oper3(imm(Const,Size)) :-
	!,
	const_size(Size),
	write('#'),
	expression(Const).
    oper3(general(G)) :-
	!,
	write('G^'),
	expression(G).
    oper3(absolute(Loc)) :-
	!,
	write('@#'),
	expression(Loc).
    oper3(llab(I)) :-
	!,
	expression(llab(I)).
    oper3(regmask(RegList)) :-
	!,
	write('^M<'),
	writeRegList(RegList),
	write('>').

    writeReg(r14) :- !, write('SP').
    writeReg(r13) :- !, write('FP').
    writeReg(r12) :- !, write('AP').
    writeReg(Reg) :- write(Reg).
    
    writeRegList([]).
    writeRegList([Reg]) :- 
	!, 
	writeReg(Reg).
    writeRegList([Reg|Rest]) :- 
	writeReg(Reg), 
	write(','), 
	writeRegList(Rest).


    expression(E) :- o(E,10).
    
    o(Exp,Lev) :-
	number(Exp),
	!,
	write(Exp).
    o(Exp,Lev) :- 
	atom(Exp),
	!,
	write(Exp).
    o(llab(I),Lev) :-
	!,
	write('L'),write(I).
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
	write('<'),
	o(Assoc,10,OLev,OFunc,Exp),
	write('>').

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


    disp_size(V) :- var(V), !.
    disp_size(byte) :- !, write('B^').
    disp_size(word) :- !, write('W^').
    disp_size(long) :- write('L^').

    const_size(V) :- var(V), !.
    const_size(short) :- !, write('S^').
    const_size(imm) :- write('I^').


    tab(N) :- N =< 0, !.
    tab(N) :- tab, NN is N-1, tab(NN).

    tab :- put(0'\t).
    space :- put(0' ).
    comma :- put(0',).

    comment(Comment,LN) :-
	tab,
	put(0';),
	space,
	write(Comment),
	space,
	write(LN),
	nl.
endmod.
