/*===================================================================*
 |		atr_args.pro
 |	Copyright (c) 1991-94 Applied Logic Systems, Inc
 |
 |	-- Parse an abstract assembly language argument (M88000)
 |
 | Author: Kevin A. Buettner
 | Created: 4/10/90	(68k version)
 | Revision History:
 |	Revised: 6/30/90, kev	-- VAX version
 |	Revised: 1/10/91, kev	-- Sparc version
 |	Revised: 5/7/91,  kev	-- M88k version
 |
 | Module Name:	args
 | Uses Modules:
 |		expressions		(aexp.pro)
 |
 | Exported Procedures:
 |	aarg(Arg,InList,OutList)
 |		-- get Arg from InList with remaining
 |		   tail OutList.
 |
 |	M88K addressing constructs
 |
 |	R		-- register
 |			== reg(R)
 |	FCR		-- floating point control register
 |			== fcr(R)
 |	CR		-- control register
 |			== cr(R)
 |	Exp		-- expression (used for labels and constants)
 |			== exp(Exp)
 |	[R]		-- Scaled register (for loads and stores)
 |			== scale(R)
 |	W<O>		-- width/offset pair for bit field instrs
 |			== wo(W,O)
 |	Cond		-- lt, gt, le, etc used with bb1 and bb0 from cmp
 |			== cond1(Cond)
 |	Cond		-- lt0, gt0, etc used with bcnd
 |			== cond2(Cond)
 *===================================================================*/

module args.
    use	expressions.
    export aarg/3.

aarg(Label) -->
		[number(Num),ident(BorF)],
		{bORf(BorF,Num,Label)},
		!.

bORf(b,Num,blab(Num)).
bORf(f,Num,flab(Num)).

aarg(RC) -->	[ident(ID)],
		{regorcond_ident(ID,RC1)},
		!,
		checkscale(RC1,RC).

checkscale(In,Out) -->
		[lbrac,ident(R),rbrac],
		!,
		{checkscale2(R,In,Out)}.
checkscale(R,R) --> [].

checkscale2(R2,reg(R1),scale(R1,R2)) :- register(R2), !.
checkscale2(_,_,error('Error in scale expression')).


aarg(wo(0,Offset)) -->
		[langle],
		expression_additive(Offset),
		[rangle].
aarg(hi16(E)) -->
		hi16,
		expression(E),
		!.
aarg(lo16(E)) -->
		lo16,
		expression(E),
		!.

hi16 --> [ident(hi16)].
hi16 --> [fident(hi16)].
lo16 --> [ident(lo16)].
lo16 --> [fident(lo16)].


aarg(Exp) -->	expression_additive(E1),
		check_angle(E1,Exp),
		!.
aarg(error('Invalid argument')) --> [].

check_angle(Width,wo(Width,Offset)) -->
		[langle],
		expression_additive(Offset),
		[rangle],
		!.
check_angle(E,E) --> [].

regorcond_ident(ID,reg(ID))	:- register(ID).
regorcond_ident(ID,cr(ID))	:- control_register(ID).
regorcond_ident(ID,fcr(ID))	:- fcontrol_register(ID).
regorcond_ident(ID,cond1(ID))	:- cond1(ID).
regorcond_ident(ID,cond2(ID))	:- cond2(ID).

register(r0).	register(r1).	register(r2).	register(r3).
register(r4).	register(r5).	register(r6).	register(r7).
register(r8).	register(r9).	register(r10).	register(r11).
register(r12).	register(r13).	register(r14).	register(r15).
register(r16).	register(r17).	register(r18).	register(r19).
register(r20).	register(r21).	register(r22).	register(r23).
register(r24).	register(r25).	register(r26).	register(r27).
register(r28).	register(r29).	register(r30).	register(r31).

control_register(cr0).		control_register(cr1).	
control_register(cr2).		control_register(cr3).
control_register(cr4).		control_register(cr5).
control_register(cr6).		control_register(cr7).
control_register(cr8).		control_register(cr9).
control_register(cr10).		control_register(cr11).
control_register(cr12).		control_register(cr13).
control_register(cr14).		control_register(cr15).
control_register(cr16).		control_register(cr17).
control_register(cr18).		control_register(cr19).
control_register(cr20).

fcontrol_register(fcr0).	fcontrol_register(fcr1).	
fcontrol_register(fcr2).	fcontrol_register(fcr3).
fcontrol_register(fcr4).	fcontrol_register(fcr5).
fcontrol_register(fcr6).	fcontrol_register(fcr7).
fcontrol_register(fcr8).	fcontrol_register(fcr62).
fcontrol_register(fcr63).

cond1(hs).
cond1(lo).
cond1(ls).
cond1(hi).
cond1(ge).
cond1(lt).
cond1(le).
cond1(gt).
cond1(ne).
cond1(eq).

cond2(eq0).
cond2(ne0).
cond2(gt0).
cond2(lt0).
cond2(ge0).
cond2(le0).


/*
 * Labels in the 88k implementation may have an @ in them.  We hack the
 * tokenizer here to accomplish this
 */

:- tokenizer:retract(isSpecial(0'@,at)), tokenizer:assertz(isAlpha(0'@)).

endmod.
