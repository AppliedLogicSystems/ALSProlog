/*=================================================================*
 |		atr_out.pro
 |	Copyright (c) 1990-94 Applied Logic Systems, Inc.
 |
 |	-- concrete assembly language translator output (680xx/sunos4)
 |
 | Author:	Kevin A. Buettner
 | Creation:	4/12/90
 | Revision History:
 |	Revised 5-14-93, Kev -- merged in Scott Medeiros' additions for
 |			   	floating point math
 *=================================================================*/

%
% insuffix/1 and outsuffix/1 determine the suffixes for input and output
% filenames.  This information is used by the driver to construct file names.
%

insuffix('.68k').
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

    output2(Label,opcode(Op,Suffix),Operands,Tab) :-
	is_nop(Op,Suffix,Operands),
	!,
	label(Label,Tab).
    output2(Label,Opcode,Operands,Tab) :-
	label(Label,Tab),
	opcode(Opcode),
	operands(Operands,Tab).
    
    is_nop(move,_,[X,X]).
    
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
	write('_'),
	write(Other),
	write(':'),
	tab.
    
    /*
     * opcode/1		-- emit the opcode
     */
    
    opcode(empty) :- !.
    opcode(opcode(Op,Suffix)) :-
	translate_op(Op,OutOp),
	translate_suffix(Op,Suffix,OutSuf),
	!,
	write(OutOp),
	suffix(OutSuf),
	tab.
    
    /*
     * suffix/1		-- emit the suffix (if any)
     */
    
    suffix(suppressed) :- !.
    suffix(unsized) :- !.
    suffix(Other) :-
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
    
    operand(dreg(Dn),Tab) :- !, write(Dn).
    operand(areg(An),Tab) :- !, write(An).
    operand(fpreg(FPn),Tab) :- !, write(FPn).
    operand(indir(An),Tab) :- !, write(An), write('@').
    operand(postincr(An),Tab) :- !, write(An), write('@+').
    operand(predecr(An),Tab) :- !, write(An), write('@-').
    operand(indir(E,An),Tab) :- 
	!,
	write(An),
	write('@('),
	expression(E,Tab),
	write(')').
    operand(indir(E,An,I),Tab) :-
	!,
	(An \== suppress_a -> write(An) ; true),
	index(E,I,Tab).
    operand(memindir(pre,Bd,BaseReg,Index,Od),Tab) :-
	!,
	(BaseReg \= suppress_a
	    ->
		write(BaseReg)
	    ;
		true),
	write('@'),
	((Bd == null_bd, Index == suppress_x)
	    ->
		write('(0)')
	    ;
		write('(')
	),
	(Bd \= null_bd
	    ->
		expression(Bd,Tab),
		(Index \= suppress_x -> write(',') ; true)
	    ;
		true
	),
	(Index \= suppress_x -> index(Index) ; true),
	((Bd == null_bd, Index == suppress_x) -> true ; write(')')),	
	(Od == null_od
	    ->
		write('@(0)')
	    ;
		write('@'),
		write('('),
		expression(Od,Tab),
		write(')')
	).
					
    operand(memindir(post,Bd,BaseReg,Index,Od),Tab) :-
	!,
	(BaseReg \= suppress_a
	    ->
		write(BaseReg)
	    ;
		true),
	write('@'),
	(Bd \= null_bd
	    ->
		write('('),
		expression(Bd,Tab),
		write(')')
	    ;
		write('(0)')
	),
	write('@'),
	((Od == null_od, Index == suppress_x)
	    ->
		write('(0)')
	    ;
		write('(')
	),	
	(Od == null_od
	    -> true
	    ;
		expression(Od,Tab),
		(Index == suppress_x -> true ; write(','))
	),
	(Index == suppress_x -> true
		;
		index(Index)),
	((Od == null_od, Index == suppress_x) -> true ; write(')')).	


    operand(pc_indir(E),Tab) :- !, write('pc@('), expression(E,Tab), write(')').
    operand(pc_indir(E,I),Tab) :- !, write('pc'), index(E,I,Tab).
    operand(abs_w(E),Tab) :- !, expression(E,Tab), write(':w').
    operand(abs(E),Tab) :- !, expression(E,Tab).
    operand(imm(E),Tab) :- !, put(0'#),expression(E,Tab).
    operand(llab(I),Tab) :- !, expression(llab(I),Tab).
    operand(bitfield(EA,Offset,Width),Tab) :-
	!,
	operand(EA,Tab),
	write('{'),
	bitfield_component(Offset,Tab),
	write(':'),
	bitfield_component(Width,Tab),
	write('}').

    bitfield_component(dreg(Dn),Tab) :-
	!,
	operand(dreg(Dn),Tab).
    bitfield_component(Num,Tab) :-
	number(Num),
	operand(imm(Num),Tab).


    index(E,index(Xn,Size,Scale),Tab) :-
	write('@('),
	expression(E,Tab),
	comma,
	write(Xn),
	write(':'),
	write(Size),
	write(':'),
	write(Scale),
	write(')').
    index(index(Xn,Size,Scale)) :-
	write(Xn),
	write(':'),
	write(Size),
	write(':'),
	write(Scale).

    
    expression(llab(I),Tab) :- !, write('L'),write(I).
    expression(Other,Tab) :- o(Other,Tab).

    /*
     * The following code actually needs to be fixed up somewhat since the
     * sun does not have any notion of operator precedence.  According to
     * page 19:
     *
     *		Expressions are evaluated left to right with no operator
     *		precedence.  Thus 
     *		1+2*3 
     *		evaluates to 9, not 7. Unary operators have precedence over
     *		binary operators since they are considered part of a term,
     *		and both terms of a binary operator must be evaluated before
     *		the binary operator can be applied.
     */

    o(Exp,Tab) :-
	o(Exp,10,Tab).

    o(Exp,Lev,Tab) :-
	float(Exp),
	round(Exp) =\= Exp,
	!,
	write('0r'),
	write(Exp).
    o(Exp,Lev,Tab) :-
	number(Exp),
	!,
	write(Exp).
    o(Exp,Lev,Tab) :- 
	atom(Exp),
	!,
	write('_'),
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
    space :- put(0' ).
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
    commentChar(0'|).

    translate_op(adda,add).
    translate_op(addi,add).
    translate_op(andi,and).
    translate_op(andi_ccr,and).
    translate_op(cmpi,cmp).
    translate_op(cmpa,cmp).
    translate_op(eori,eor).
    translate_op(move,mov).
    translate_op(movea,mov).
    translate_op(ori,or).
    translate_op(subi,sub).
    translate_op(suba,sub).
    translate_op(global,'.globl').
    translate_op(text,'.text').
    translate_op(data,'.data').
    translate_op(long,'.long').
    translate_op(even,'.even').
    translate_op(X,X).

    translate_suffix(Op,S,suppressed) :-
	suppress_suffix(Op),
	!.
    translate_suffix(Op,S,suppressed) :- var(S), !.
    translate_suffix(Op,pseudo,suppressed) :- !.
    translate_suffix(Op,b,s) :-
	btos(Op),
	!.
    translate_suffix(Op,'X',x) :- !.	% X is a suffix for fpcp instructions
    translate_suffix(Op,L,L).


    btos(bcc).
    btos(bcs).
    btos(beq).
    btos(bge).
    btos(bgt).
    btos(bhi).
    btos(ble).
    btos(bls).
    btos(blt).
    btos(bmi).
    btos(bne).
    btos(bpl).
    btos(bvc).
    btos(bvs).
    btos(bra).
    btos(bsr).

    suppress_suffix(lea).
    suppress_suffix(pea).
    suppress_suffix(moveq).
    suppress_suffix(bclr).
    suppress_suffix(exg).
    suppress_suffix(link).

    suppress_suffix(dbcc).
    suppress_suffix(dbcs).
    suppress_suffix(dbeq).
    suppress_suffix(dbf).
    suppress_suffix(dbge).
    suppress_suffix(dbgt).
    suppress_suffix(dbhi).
    suppress_suffix(dble).
    suppress_suffix(dbls).
    suppress_suffix(dblt).
    suppress_suffix(dbmi).
    suppress_suffix(dbne).
    suppress_suffix(dbpl).
    suppress_suffix(dbt).
    suppress_suffix(dbvc).
    suppress_suffix(dbvs).
    suppress_suffix(dbra).

    suppress_suffix(scc).
    suppress_suffix(scs).
    suppress_suffix(seq).
    suppress_suffix(sf).
    suppress_suffix(sge).
    suppress_suffix(sgt).
    suppress_suffix(shi).
    suppress_suffix(sle).
    suppress_suffix(sls).
    suppress_suffix(slt).
    suppress_suffix(smi).
    suppress_suffix(sne).
    suppress_suffix(spl).
    suppress_suffix(st).
    suppress_suffix(svc).
    suppress_suffix(svs).
    suppress_suffix(swap).
    suppress_suffix(tas).

    suppress_suffix(bfextu).

endmod.	%% output
