/*
 * atr_out.pro		-- output assembly statements for Motorola Delta
 *			   machines (680x0)
 *
 *	Copyright (c) 1990-93 Applied Logic Systems, Inc.
 *
 *
 * Author:	Kevin A. Buettner
 * Creation:	4/25/90
 * Revision History:
 *	Revised 5-14-93, Kev	-- merged in Scott Medeiros' additions for
 *				   floating point math
 *
 *
 * Note:  With any luck this should work on the HP machines as well.
 */

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

    output_begin(_).		%% do nothing

    output_end(Tab) :-
	expression_labels(LL,EL,Tab),
	oe(LL,EL).

    oe([],[]) :- !.
    oe([H1|T1],[H2|T2]) :-
	tab,
	write('set	L'),
	write(H1),
	comma,
	tab,
	write(H2),
	nl,
	oe(T1,T2).

    output(error, LN, Tab,Tab) :- !, comment('Error on line',LN).
    /*
    output(S,LN,T,T) :- write(S),comment(' ',LN).
    */
    output(statement(Label,Opcode,Operands),LN,InTab,OutTab) :-
	output2(Label,Opcode,Operands,InTab,OutTab),
	comment(' ',LN).

    output2(Label, opcode(Op,Suffix), Operands, Tab,Tab) :-
	is_nop(Op,Suffix,Operands),
	!,
	label(Label).
    output2(Label,Opcode,Operands, InTab, OutTab) :-
	label(Label),
	opcode(Opcode),
	operand_fix(Opcode,Operands,NewOperands),
	!,
	operands(NewOperands, InTab, OutTab).
    
    is_nop(move,_,[X,X]).

    /*
     * label/1		-- emit a label
     */
    
    label(empty(_)) :-
	!,
	tab.
    label(llab(I)) :-
	!,
	write('L'),write(I),write(':'),
	tab.
    label(Other) :-
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
	dot,
	write(Other).
    
    /*
     * operands/1	-- emit the operands
     */
    
    operands([],T,T) :- tab(4), !.
    operands([Op|Rest],InTab,OutTab) :-
	operand(Op,InTab,ITab),
	more_operands(Rest,TL,ITab,OutTab),
	tab(TL).
    
    more_operands([],3,T,T) :- !.
    more_operands([Op|Rest],TL,InTab,OutTab) :-
	comma,
	tab,
	operand(Op,InTab,ITab),
	more_operands(Rest,PTL,ITab,OutTab),
	TL is PTL-1.
    
    /*
     * operand/1	-- emit one operand
     */

    operand(dreg(Dn),T,T) :- !, reg(Dn).
    operand(areg(An),T,T) :- !, reg(An).
    operand(fpreg(FPn),T,T) :- !, reg(FPn).
    operand(indir(An),T,T) :- !, lparen, reg(An), rparen.
    operand(postincr(An),T,T) :- !, lparen, reg(An), rparen, put(0'+).
    operand(predecr(An),T,T) :- !, put(0'-), lparen, reg(An), rparen.
    operand(indir(E,An),InTab,OutTab) :- 
	!,
	expression(E,InTab,OutTab),
	lparen,
	reg(An),
	rparen.
    operand(indir(E,An,I),InTab,OutTab) :-
	!,
	expression(E,InTab,OutTab),
	index(An,I).
    operand(memindir(pre,Bd,BaseReg,Index,Od),InTab,OutTab) :-
	!,
	lparen,
	lbrac,
	% at least one operand must be non-null within [op1,op2,op3]
	(Bd \= null_bd ->
		expression(Bd,InTab,Tab1),
		(BaseReg \= suppress_a -> comma ;
					 (Index \= suppress_x -> comma ; true))
		;
		Tab1 = InTab),
	(BaseReg \= suppress_a ->
		reg(BaseReg),
		(Index \= suppress_x -> comma ; true)
		; true),
	(Index \= suppress_x -> index(Index) ; true),
	rbrac,
	(Od \= null_od -> comma, expression(Od,Tab1,OutTab) ; OutTab = Tab1),
	rparen.
    operand(memindir(post,Bd,BaseReg,Index,Od),InTab,OutTab) :-
	!,
	lparen,
	lbrac,
	% at least one operand must be non-null within [op1,op2]
	(Bd \= null_bd ->
		expression(Bd,InTab,Tab1),
		(BaseReg \= suppress_a -> comma ;
					  true)
		;
		Tab1 = InTab),
	(BaseReg \= suppress_a ->
		reg(BaseReg)
	 ; 	true),
		
	rbrac,
	% Both outer operands could be suppressed
	((Index \= suppress_x ; Od \= null_od) -> comma ; true),
	(Index \= suppress_x ->
		index(Index),
		(Od \= null_od ->
			comma ; true)
		;
		true),
	(Od \= null_od -> expression(Od,Tab1,OutTab) ; OutTab = Tab1),
	rparen.
    operand(pc_indir(E),InTab,OutTab) :-
	!,
	expression(E,InTab,OutTab),
	lparen,
	reg(pc),
	rparen.
    operand(pc_indir(E,I),InTab,OutTab) :-
	!,
	expression(E,InTab,OutTab),
	index(pc,I).
    operand(pc_memindir(pre,Bd,Index,Od),InTab,OutTab) :-
	!,
	lparen,
	lbrac,
	% none, one or both of bd and Xn could be null
	(Bd \= null_bd ->
		expression(Bd,InTab,Tab1),
		comma
		;
		Tab1 = InTab),
	reg(pc),
	(Index \= suppress_x ->
		comma,
		index(Index)
	;
		true),
		
	rbrac,
	(Od \= null_od -> comma, expression(Od,Tab1,OutTab) ; OutTab = Tab1),
	rparen.
    operand(pc_memindir(post,Bd,Index,Od),InTab,OutTab) :-
	!,
	lparen,
	lbrac,
	% bd may or may not be null
	(Bd \= null_bd ->
		expression(Bd,InTab,Tab1),
		comma
		;
		Tab1 = InTab),
	reg(pc),
	rbrac,

	% Both outer operands could be suppressed
	((Index \= suppress_x ; Od \= null_od) -> comma ; true),
	(Index \= suppress_x ->
		index(Index),
		(Od \= null_od ->
			comma ; true)
		;
		true),
	(Od \= null_od -> expression(Od,Tab1,OutTab) ; OutTab = Tab1),
	rparen.
    operand(abs_w(E),InTab,OutTab) :-
	!,
	expression(E,InTab,OutTab).
    operand(abs(E),InTab,OutTab) :-
	!,
	expression(E,InTab,OutTab).
    operand(imm(E),InTab,OutTab) :-
	!,
	put(0'&),
	expression(E,InTab,OutTab).
    operand(llab(I),InTab,OutTab) :-
	!,
	expression(llab(I),InTab,OutTab).
    operand(bitfield(EA,Offset,Width),InTab,OutTab) :-
	!,
	operand(EA,InTab,Tab1),
	lcurly,
	bitfield_component(Offset,Tab1,Tab2),
	colon,
	bitfield_component(Width,Tab2,OutTab),
	rcurly.


    bitfield_component(dreg(Dn),InTab,OutTab) :-
	!,
	operand(dreg(Dn),InTab,OutTab).
    bitfield_component(Num,InTab,OutTab) :-
	number(Num),
	operand(imm(Num),InTab,OutTab).

    
    index(Reg,index(Xn,Size,Scale)) :-
	lparen,
	(Reg \== suppress_a  ->
		reg(Reg),
		comma
	;
		true),
	reg(Xn),
	dot,
	write(Size),
	star,
	write(Scale),
	rparen.

    index(index(Xn,Size,Scale)) :-
	reg(Xn),
	dot,
	write(Size),
	star,
	write(Scale).

    expression(llab(I),T,T) :-
	!,
	write('L'),
	write(I).
    expression(A,T,T) :-
	atomic(A),
	!,
	write(A).
    expression(E,InTab,OutTab) :-
	expression_label(LID,E,InTab,OutTab),
	write('L'),
	write(LID).

    
    
    reg(suppress_a) :- !.
    reg(X) :- put(0'%),write(X).	%% stupid percent signs in front
					%% or register names

    
    tab(N) :- N =< 0, !.
    tab(N) :- tab, NN is N-1, tab(NN).

    tab :-	put(0'\t).
    space :-	put(0' ).
    comma :-	put(0',).
    dot :-	put(0'.).
    colon :-	put(0':).
    star :-	put(0'*).
    lparen :-	put(0'().
    rparen :-	put(0')).
    lbrac :- 	put(0'[).
    rbrac :- 	put(0']).
    lcurly :-	put(0'{).
    rcurly :-	put(0'}).

    comment(Comment,LN) :- 
	commentChar(C),
	tab,
	put(C),
	space,
	write(Comment),
	space,
	write(LN),
	nl.
    
    commentChar(0'#).

    %% translate_op/2 translates opcode names
    translate_op(movea,mov).
    translate_op(move,mov).
    translate_op(moveq,mov).
    translate_op(subq,sub).
    translate_op(suba,sub).
    translate_op(subi,sub).
    translate_op(addq,add).
    translate_op(adda,add).
    translate_op(addi,add).
    translate_op(andi,and).
    translate_op(cmpa,cmp).
    translate_op(cmpi,cmp).
    translate_op(cmpm,cmp).
    translate_op(eori,eor).
    translate_op(ori,or).
    % and now FPCP ops
    translate_op(fmove,fmov).
    translate_op(fbne,fbneq).
    % Everything else is itself
    translate_op(X,X).

    %% operand_fix/3 reverses the compare operands
    operand_fix(opcode(Op,S),IL,OL) :-
	ofix(Op,IL,OL),
	!.
    operand_fix(_,L,L) :- !.

    ofix(cmp,[X,Y],[Y,X]).
    ofix(cmpa,[X,Y],[Y,X]).
    ofix(cmpi,[X,Y],[Y,X]).
    ofix(fcmp,[X,Y],[Y,X]).


    translate_suffix(Op,S,suppressed) :-
	suppress_suffix(Op),
	!.
    translate_suffix(Op,S,suppressed) :- var(S), !.
    translate_suffix(Op,pseudo,suppressed) :- !.
    translate_suffix(Op,w,suppressed) :-
	btos(Op),
	!.
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
