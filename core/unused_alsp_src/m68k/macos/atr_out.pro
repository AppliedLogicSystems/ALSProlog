/*
 * atr_out.pro		-- outputs stuff for the Mac
 *	Copyright (c) 1990 Applied Logic Systems, Inc.
 *
 * Author:	Kevin A. Buettner
 * Creation:	4/12/90
 * Revision History:
 *	06/17/92 -- Ron DiNapoli:	  Began Macintosh Modifications
 *								  using Sun atr_out and Mot atr_out as guides.
 */

%
% insuffix/1 and outsuffix/1 determine the suffixes for input and output
% filenames.  This information is used by the driver to construct file names.
%

insuffix('.68k').
outsuffix('.a').    % MPW Assembler uses .a extension on assembly source files.


module output.
use labels.
export output/4.
export output_end/1.
export output_begin/1.

strip_ext(FileAtom, List, Stripped) 
	:-
	name(FileAtom, FileList),
	strip_extension(FileList,List,StrippedList),
	name(Stripped,StrippedList).

strip_extension([],Stripped,Stripped).
strip_extension([0'. | _], Stripped, Stripped).

strip_extension([ Char | Rest ], List, Stripped ) 
	:-
	append(List, [Char], NewList),!,
	strip_extension(Rest,NewList,Stripped).

output_begin(FilePath) 
	:-	% For MPW Asm, output necessary include files
	printf("\tcase on\n"),
	printf("\tmachine mc68020\n"),
	printf("\tinclude 'SysEqu.a'\n"),
	printf("\tinclude 'Traps.a'\n"),
%%		printf("\tinclude 'SANEMacs.a'\n"),
	pathPlusFile(Path, File, FilePath),
	strip_ext(File,[],StrippedFile),
	printf("%tHEADER\tproc\n",[StrippedFile]).
%%		printf("     include '%t.imports'\n\n",[StrippedFile]).

output_end(File) 
	:-			% For MPW Asm, output necessary terminator for
	printf("\tendproc\n"),	%  the 'proc' statement at start of file
	printf("\tend\n").

notAnum(Expr) 
	:-
	integer(Expr),!, fail.
notAnum(Expr).

%
%	adjustMOVEMoperands/2 is used to convert the decimal oeprand into
%	a list of registers.  For some reason, MPW Assembler doesn't allow
%	one to specify a hex number instead of a list of registers.
%

adjustMOVEMoperands(Operands,NewOperands) 
	:-
	fixmovem(Operands,[],NewOperands).

fixmovem([],NewOperands,NewOperands).

fixmovem([imm(Expr) | Rest], SoFar, NewOperands) 
	:-
	number(Expr),
	make_movem_reglist(Expr,RegListAtom),
	append(SoFar,[abs(RegListAtom)],NewSoFar),!,
	fixmovem(Rest,NewSoFar,NewOperands).

fixmovem([ One | Rest], SoFar, NewOperands) 
	:-
	append(SoFar,[One],NewSoFar),!,
	fixmovem(Rest,NewSoFar,NewOperands).

make_movem_reglist(0x630, 'd5/d6/a2/a3').
make_movem_reglist(0x690, 'd5/d6/a0/a3').
make_movem_reglist(0xfefe,'a1-a7/d1-d7').
make_movem_reglist(0x3efe,'a1-a5/d1-d7').
make_movem_reglist(0xfc60,'d5/d6/a2-a7').
make_movem_reglist(0x3ffe,'a0-a5/d1-d7').
make_movem_reglist(0x7ffe,'a0-a6/d1-d7').
make_movem_reglist(0x2001,'d0/a5').
make_movem_reglist(0x8004,'d0/a5').
make_movem_reglist(0x077f,'d5-d7/a1-a7').
make_movem_reglist(0xfee0,'d5-d7/a1-a7').
make_movem_reglist(Num,Num).

%
%	The second predicate of output/4 is added specifically for generating
%	MPW Asm files.  The abstract "global" declaration must be translated
%	into an "export fn:code" declaration inthe MPW asm file.  There is no
%	problem translating "global" to "export", but we must special case the
%	output/4 predicate to tack on a ":code" when a "global" opcode is 
%	encountered
%

output(error,LN,Tab,Tab) 
	:- !, 
	comment('Error on line',LN).
output(statement(Label,opcode(externdata,S),Operands),LN,Tab,Tab) 
	:- !,
	output2(Label,opcode(externdata,S),Operands,Tab),
	printf(":data"),
	comment(' ',LN).
output(statement(Label,opcode(externcode,S),Operands),LN,Tab,Tab) 
	:- !,
	output2(Label,opcode(externdata,S),Operands,Tab),
	printf(":code"),
	comment(' ',LN).
output(statement(Label,opcode(global,S),Operands),LN,Tab,Tab) 
	:- !,
	output2(Label,opcode(global,S),Operands,Tab),
	printf(":code"),
	comment(' ',LN).
output(statement(Label,opcode(globaldata,S),Operands),LN,Tab,Tab) 
	:- !,
	output2(Label,opcode(globaldata,S),Operands,Tab),
	printf(":data"),
	comment(' ',LN).
output(statement(Label,opcode(trap,S),[imm(TrapMacro)]),LN,Tab,Tab) 
	:- !,
	label(Label,Tab),
	write(TrapMacro),
	tab(5),
	comment(' ',LN).
output(statement(Label,opcode(ftst,S),Operands),LN,Tab,Tab) 
	:- !,
	output2(Label,opcode(ftest,S),Operands,Tab),
	comment(' ',LN).
output(statement(Label,opcode(movea,S),[imm(Addr),Dest]),LN,Tab,Tab) 
	:- !,
	output2(Label,opcode(movea,S),[abs(Addr),Dest],Tab),
	comment(' ',LN).
output(statement(Label,opcode(movem,S),Operands),LN,Tab,Tab) 
	:- !,
	adjustMOVEMoperands(Operands,NewOperands),
	output2(Label,opcode(movem,S),NewOperands,Tab),
	comment(' ',LN).		
output(statement(Label,opcode(move,S),[imm(Expr), Dest]),LN,Tab,Tab) 
	:-
	notAnum(Expr),
	!,
	output2(Label,opcode(move,S),[abs(Expr),Dest],Tab),
	comment('  ',LN). 
output(statement(Label,Opcode,Operands),LN,Tab,Tab) 
	:-
    output2(Label,Opcode,Operands,Tab),
	comment(' ',LN).
	
output2(Label,opcode(Op,Suffix),Operands,Tab) 
	:-
	is_nop(Op,Suffix,Operands),
	!,
	label(Label,Tab).
output2(Label,Opcode,Operands,Tab) 
	:-
	label(Label,Tab),
	opcode(Opcode),
	operands(Operands,Tab).

is_nop(move,_,[X,X]).

/*
 * label/1		-- emit a label
 */

label(empty(_),Tab) 
	:- !,
	tab.
label(llab(I),Tab) 
	:- !,
	write('L'),write(I),write(':'),
	tab.
label(Other,Tab) 
	:-
%	write('_'),				% MPW Asm does not require leading _
	write(Other),
	write(':'),
	tab.

/*
 * opcode/1		-- emit the opcode
 */

opcode(empty) 
	:- !.
opcode(opcode(Op,Suffix)) 
	:-
	translate_op(Op,OutOp),
	translate_suffix(Op,Suffix,OutSuf),
	!,
	write(OutOp),
	suffix(OutSuf),
	tab.

/*
 * suffix/1		-- emit the suffix (if any)
 */

suffix(suppressed) 
	:- !.
suffix(unsized) 
	:- !.
suffix(Other) 
	:-
	write('.'),				% Mac asm uses original Motorola syntax
	write(Other).

/*
 * operands/2	-- emit the operands
 */

operands([],Tab) 
	:- 
	tab(4),!.
operands([Op|Rest],Tab) 
	:-
	operand(Op,Tab),
	more_operands(Rest,TL,Tab),
	tab(TL).

more_operands([],3,Tab) 
	:- !.
more_operands([Op|Rest],TL,Tab) 
	:-
	comma,
	tab,
	operand(Op,Tab),
	more_operands(Rest,PTL,Tab),
	TL is PTL-1.


%
%	Only convert very large integers that will not fit into a Prolog 
%	integer...
%
convert2hex_if_necessary(Expr,Expr) 
	:-
	integer(Expr).

convert2hex_if_necessary(Expr,NewExpr) 
	:-
	number(Expr),
	user:largeDec2Hex(Expr,NewExpr).

convert2hex_if_necessary(Expr,Expr).

/*
 * operand/2	-- emit one operand
 */

operand(dreg(Dn),Tab) 
	:- !, 
	write(Dn).
operand(areg(An),Tab) 
	:- !, 
	write(An).
operand(fpreg(FPn),Tab) 
	:- !, 
	write(FPn).
operand(indir(An),Tab) 
	:- !, 
	write('('), write(An), write(')').
operand(postincr(An),Tab) 
	:- !, 
	write('('), write(An), write(')+').
operand(predecr(An),Tab) 
	:- !, 
	write('-('), write(An), write(')').
operand(indir(E,An),Tab) 
	:- !,
	expression(E,Tab),
	write('('),
	write(An),
	write(')').
operand(indir(E,An,I),Tab) 
	:- !,
	write('('),
	expression(E,Tab),
%		index(An,I,Tab).	
	write(','),
	(An \= suppress_a
		->
			write(An),
			write(',')
		;
			true),
	index(I),
	write(')').

operand(memindir(pre,Bd,BaseReg,Index,Od),Tab) 
	:- !,
	write('(['),
	((Bd == null_bd)
	    ->
	true
	    ;
	expression(Bd,Tab),
	write(',')
	),
	(BaseReg \= suppress_a
	    ->
	write(BaseReg),
	write(',')
	    ;
	true),
	(Index \= suppress_x -> index(Index) ; true),
	write(']'),
	(Od == null_od
	    ->
	true
	    ;
	write(','),
	expression(Od,Tab)
	),
	write(')').
	
operand(memindir(post,Bd,BaseReg,Index,Od),Tab) 
	:- !,
	write('(['),
	((Bd == null_bd)
	    ->
	true
	    ;
	expression(Bd,Tab),
	write(',')
	),
	(BaseReg \= suppress_a
	    ->
	write(BaseReg)
	    ;
	true),
	write(']'),
	(Index \= suppress_x -> index(Index) ; true),
	(Od == null_od
	    ->
	true
	    ;
	write(','),
	expression(Od,Tab)
	),
	write(')').

operand(pc_indir(E),Tab) 
	:- !, 
	expression(E,Tab), write('(pc)').
operand(pc_indir(E,I),Tab) 
	:- !, 
	write('pc'), index(E,I,Tab).
operand(abs_w(E),Tab) 
	:- 
	expression(E,Tab), write('.w').
operand(abs(E),Tab) 
	:-!, 
	expression(E,Tab).

operand(imm(E),Tab) 
	:- !, 
	put(0'#), expression(E,Tab)
/*
operand(imm(E),Tab) 
	:- 
	!, 
	put(0'#),
	convert2hex_if_necessary(E,NewE),
	expression(NewE,Tab).
*/
operand(llab(I),Tab) 
	:- !, 
	expression(llab(I),Tab).
operand(bitfield(EA,Offset,Width),Tab) 
	:- !,
	operand(EA,Tab),
	write('{'),
	bitfield_component(Offset,Tab),
	write(':'),
	bitfield_component(Width,Tab),
	write('}').

index(E,index(Xn,Size,Scale),Tab) 
	:-
	write('('),
	expression(E,Tab),
	comma,
	write(Xn),
	write('.'),
	write(Size),
	write('*'),
	write(Scale),
	write(')').
index(index(Xn,Size,Scale)) 
	:-
	write(Xn),
	write('.'),
	write(Size),
	write('*'),
	write(Scale).

expression(llab(I),Tab) 
	:- !, 
	write('L'),write(I).
expression(Other,Tab) 
	:- 
	o(Other,Tab).

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

o(Exp,Tab) 
	:-
	o(Exp,10,Tab).

o(Exp,Lev,Tab) 
	:-
	float(Exp),
	round(Exp) =\= Exp,
	!,
	write('"'),
	write(Exp),
	write('"').

o(Exp,Lev,Tab) 
	:-
	number(Exp),
	!,
	write(Exp).
o(Exp,Lev,Tab) 
	:- 
	atom(Exp),
	!,
	%	write('_'),		% No leading underbar needed for MPW Asm
	write(Exp).
o(Exp,Lev,Tab) 
	:- 
	functor(Exp,F,A), 
	is_op(F,A,OLev,Assoc,OFunc),
	!,
	o(Assoc,Lev,OLev,OFunc,Exp,Tab).
o(Exp,Lev,Tab) 
	:-
	write(illegal(Exp)).

o(Assoc,Lev,OLev,OFunc,Exp,Tab) 
	:-
	Lev < OLev,
	!,
	write('('),
	o(Assoc,10,OLev,OFunc,Exp,Tab),
	write(')').

o(fy,_,OLev,OFunc,Exp,Tab) 
	:- !,
	write(OFunc),
	arg(1,Exp,A1),
	o(A1,OLev,Tab).
o(yfx,_,OLev,OFunc,Exp,Tab) 
	:- !,
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



tab(N) 
	:- 
	N =< 0, !.
tab(N) 
	:- 
	tab, NN is N-1, tab(NN).

tab 
	:- 
	put(0'\t).
space 
	:- 
	put(0' ).
comma 
	:- 
	put(0',).

comment(Comment,LN) 
	:-
	commentChar(C),
	tab,
	put(C),
	space,
	write(Comment),
	space,
	write(LN),
	nl.

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%% Mac specific stuff
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
commentChar(0';).

translate_op(andi_ccr,and).
translate_op(global,export).
/*
translate_op(text,'').
translate_op(data,'').
translate_op(long,'dc.l').
*/
translate_op(globaldata,export).
translate_op(text,code).
%%    translate_op(data,data).
translate_op(even,'align').
translate_op(long,'dc.l').
	translate_op(externdata,import).
translate_op(X,X).

translate_suffix(Op,S,suppressed) 
	:-
	suppress_suffix(Op),
	!.
translate_suffix(Op,S,suppressed) 
	:- 
	var(S), !.
translate_suffix(Op,pseudo,suppressed) 
	:- !.
translate_suffix(Op,b,s) 
	:-
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

endmod.	%% output


dec2hex(Dec,Hex) 
	:-
	Nybble1 is (Dec >> 28) /\ 0x0000000F,
	Nybble2 is (Dec >> 24) /\ 0x0000000F,
	Nybble3 is (Dec >> 20) /\ 0x0000000F,
	Nybble4 is (Dec >> 16) /\ 0x0000000F,
	Nybble5 is (Dec >> 12) /\ 0x0000000F,
	Nybble6 is (Dec >>  8) /\ 0x0000000F,
	Nybble7 is (Dec >>  4) /\ 0x0000000F,
	Nybble8 is Dec /\ 0x0000000F,
	constructHexNum(Nybble1,Nybble2,Nybble3,Nybble4,Nybble5,Nybble6,
				Nybble7,Nybble8,Hex).

largeDec2Hex(Dec,Hex) 
	:-
	N1 is Dec,
	Nybble1 is N1 // 0x10000000,
	N2 is (N1 - (0x10000000 * Nybble1)),
	Nybble2 is N2 // 0x01000000,
	N3 is (N2 - (0x01000000 * Nybble2)),
	Nybble3 is N3 // 0x00100000,
	N4 is (N3 - (0x00100000 * Nybble3)),
	Nybble4 is N4 // 0x00010000,
	N5 is (N4 - (0x00010000 * Nybble4)),
	Nybble5 is N5 // 0x00001000,
	N6 is (N5 - (0x00001000 * Nybble5)),
	Nybble6 is N6 // 0x00000100,
	N7 is (N6 - (0x00000100 * Nybble6)),
	Nybble7 is N7 // 0x00000010,
	Nybble8 is (N7 - (0x00000010 * Nybble7)),
	constructHexNum(Nybble1,Nybble2,Nybble3,Nybble4,Nybble5,
				Nybble6,Nybble7,Nybble8,Hex).

constructHexNum(N1,N2,N3,N4,N5,N6,N7,N8,HexAtom) 
	:-
	getHexDigit(N1,Digit1),
	getHexDigit(N2,Digit2), append(Digit1,Digit2,List2),
	getHexDigit(N3,Digit3), append(List2,Digit3,List3),
	getHexDigit(N4,Digit4), append(List3,Digit4,List4),
	getHexDigit(N5,Digit5), append(List4,Digit5,List5),
	getHexDigit(N6,Digit6), append(List5,Digit6,List6),
	getHexDigit(N7,Digit7), append(List6,Digit7,List7),
	getHexDigit(N8,Digit8), append(List7,Digit8,NumList),
	stripLeading0(NumList,AlmostHexList),
	addHexIdentifier(AlmostHexList,HexList),
	name(HexAtom,HexList).


getHexDigit(0,[0'0]).
getHexDigit(1,[0'1]).
getHexDigit(2,[0'2]).
getHexDigit(3,[0'3]).
getHexDigit(4,[0'4]).
getHexDigit(5,[0'5]).
getHexDigit(6,[0'6]).
getHexDigit(7,[0'7]).
getHexDigit(8,[0'8]).
getHexDigit(9,[0'9]).
getHexDigit(10,[0'a]).
getHexDigit(11,[0'b]).
getHexDigit(12,[0'c]).
getHexDigit(13,[0'd]).
getHexDigit(14,[0'e]).
getHexDigit(15,[0'f]).

getHexDigit(Digit,_) 
	:-
	printf("ERROR: Unable to recognize digit %t\n",[Digit]).


stripLeading0([0'0 | Rest], Stripped) 
	:- !, 
	stripLeading0(Rest,Stripped).
stripLeading0([],"0").
stripLeading0(Stripped,Stripped).

addHexIdentifier(NumList,HexList) 
	:-
	append("$",NumList,HexList).
