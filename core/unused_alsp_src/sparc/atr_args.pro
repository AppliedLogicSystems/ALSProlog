/*=======================================================================*
 | 		atr_args.pro
 |	Copyright (c) 1990-94 Applied Logic Systems, Inc
 |
 |	-- Parse an abstract assembly language argument (SPARC)
 |
 | Author: Kevin A. Buettner
 | Created: 4/10/90	(68k version)
 | Revision History:
 |	Revised: 6/30/90, kev	-- VAX version
 |	Revised: 1/10/90, kev	-- Sparc version
 |
 |
 | Module Name:	args
 | Uses Modules:
 |		expressions		(aexp.pro)
 |
 | Exported Procedures:
 |		aarg(Arg,InList,OutList)
 |					-- get Arg from InList with remaining
 |					   tail OutList.
 |
 |	SPARC addressing constructs
 |
 |	R		-- register
 |			== reg(R)
 |	FR		-- floating point register
 |			== freg(R)
 |	R1+R2		-- address formed by adding two registers together
 |			== regaddr(R1,R2)
 |	R1+C13		-- address formed by adding a register to a thirteen
 |			   bit immediate
 |			== addr(R1,C13)
 |	R1-C13		-- address formed by subtracting signed thirteen
 |			   bit constant from value in register
 |	Exp		-- expression (used for labels and constants)
 |			== exp(Exp)
 |
 |	A memory address may be syntactically formed by enclosing any
 |	of the above components in square brackets.  This form will be
 |	represented as mem(component).
 |	
 |
 *=======================================================================*/

module args.
    use	expressions.
    export aarg/3.

aarg(Label) -->
		[number(Num),ident(BorF)],
		{bORf(BorF,Num,Label)},
		!.

bORf(b,Num,blab(Num)).
bORf(f,Num,flab(Num)).

aarg(mem(Arg)) -->	[lbrac],
			simple_arg(Arg),
			[rbrac],
			!.
aarg(freg(FReg)) -->	[ident(FReg)],
			{freg(FReg)},
			!.
aarg(Arg) -->		simple_arg(Arg).
aarg(error('Invalid argument')) --> [].

simple_arg(Arg) -->	address(Arg), !.
simple_arg(Arg) -->	exp_or_const(Arg), !.


register(Reg) -->	[ident(Reg)],
			{reg(Reg)},
			!.

reg(r0).	reg(g0).	/* globals */
reg(r1).	reg(g1).
reg(r2).	reg(g2).
reg(r3).	reg(g3).
reg(r4).	reg(g4).
reg(r5).	reg(g5).
reg(r6).	reg(g6).
reg(r7).	reg(g7).
reg(r8).	reg(o0).	/* outs */
reg(r9).	reg(o1).
reg(r10).	reg(o2).
reg(r11).	reg(o3).
reg(r12).	reg(o4).
reg(r13).	reg(o5).
reg(r14).	reg(o6).
reg(r15).	reg(o7).
reg(r16).	reg(l0).	/* locals */
reg(r17).	reg(l1).
reg(r18).	reg(l2).
reg(r19).	reg(l3).
reg(r20).	reg(l4).
reg(r21).	reg(l5).
reg(r22).	reg(l6).
reg(r23).	reg(l7).
reg(r24).	reg(i0).	/* ins */
reg(r25).	reg(i1).
reg(r26).	reg(i2).
reg(r27).	reg(i3).
reg(r28).	reg(i4).
reg(r29).	reg(i5).
reg(r30).	reg(i6).
reg(r31).	reg(i7).


/* Floating point registers */

freg(f0).	freg(f8).	freg(f16).	freg(f24).
freg(f1).	freg(f9).	freg(f17).	freg(f25).
freg(f2).	freg(f10).	freg(f18).	freg(f26).
freg(f3).	freg(f11).	freg(f19).	freg(f27).
freg(f4).	freg(f12).	freg(f20).	freg(f28).
freg(f5).	freg(f13).	freg(f21).	freg(f29).
freg(f6).	freg(f14).	freg(f22).	freg(f30).
freg(f7).	freg(f15).	freg(f23).	freg(f31).



address(Addr) -->	register(Reg),
			addr2(Addr,Reg),
			!.


addr2(Addr,Reg) -->	[plus],
			addr3(Addr,Reg),
			!.

addr2(addr(Reg,-C13),Reg) -->	
			[minus],
			const13(C13),
			!.

addr2(reg(Reg),Reg) -->	!.

addr3(regaddr(Reg1,Reg2),Reg1) -->
			register(Reg2),
			!.

addr3(addr(Reg,C13),Reg) -->
			const13(C13),
			!.

const13(C13) -->	exp_or_const(exp(C13)).



exp_or_const(exp(dot(Id))) -->
			[dot,ident(Id)],
			!.
exp_or_const(exp(lo10(Exp))) -->
			lo,
			expression(Exp),
			!.
exp_or_const(exp(hi22(Exp))) -->
			hi,
			expression(Exp),
			!.
exp_or_const(exp(Exp)) -->	
			expression(Exp),
			!.

lo --> [ident(lo)].
lo --> [fident(lo)].
hi --> [ident(hi)].
hi --> [fident(hi)].

endmod.
