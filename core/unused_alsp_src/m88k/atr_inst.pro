/*====================================================================*
 |		atr_inst.pro
 |	Copyright (c) 1991-94 Applied Logic Systems, Inc.
 |
 |	-- 88k abstract assembly instruction database
 |
 | Author: Kevin A. Buettner
 | Creation: 5/6/91
 |
 | Module Name:	instructions
 | Exported Procedures:
 |	instruction(Opcode,Args,OutOpcode,InTab,OutTab)
 |		-- succeeds if Opcode and Args are of appropiate type
 |		   giving the OutOpCode.  InTab and OutTab are the
 |		   input and output database arguments.
 |	opcode(Opcode,InList,OutList)
 |		-- Parses InList leaving OutList and giving Opcode as
 |		   the return value.  This is a dcg rule.
 *====================================================================*/

module instructions.

use labels.

export instruction/5.
export opcode/3.

/*
 * An opcode is considered to be an identifier possible followed by a dot
 * followed by another identifier.
 */

opcode(Opcode) --> [ident(ID)], checkdot(Opcode,ID), !.
opcode(error('Identifier expected as opcode')) --> [].

checkdot(Opcode,ID) --> [dot], !, checksuffix(Opcode,ID).
checkdot(opcode(ID,none),ID) --> [].

checksuffix(opcode(ID,Suffix),ID) -->
		[ident(Suffix)],
		{legal_suffix(Suffix)},
		!.

checksuffix(error('Illegal suffix on instruction'),ID) --> [].

legal_suffix(c).
legal_suffix(u).
legal_suffix(b).
legal_suffix(bu).
legal_suffix(h).
legal_suffix(hu).
legal_suffix(d).
legal_suffix(n).
legal_suffix(ss).
legal_suffix(sd).
legal_suffix(ds).
legal_suffix(dd).
legal_suffix(ci).
legal_suffix(co).
legal_suffix(cio).
legal_suffix(S) :- f3_suffix(S).


/*
 * An instruction is valid if it can be found in the instr database below
 * with the appropriate arguments.
 */

instruction(opcode(Op,Suffix),Args,opcode(Op,Suffix),Tab,Tab) :-
    instr(Op,Suffix,Args),
    !.


/*`
 * Pseudo-ops
 */

instr(text,none,[]).
instr(data,none,[]).
instr(align,none,[_]).
instr(global,none,[_]).
instr(word,none,_).
instr(double,none,_).

/*
 * 88k instructions
 */

instr(add,none,[reg(RD),reg(RS1),S2]).
instr(add,Suffix,[reg(RD),reg(RS1),reg(RS2)]) :-
				member(Suffix,[ci,co,cio]).
instr(addu,none,[reg(RD),reg(RS1),S2]).
instr(addu,Suffix,[reg(RD),reg(RS1),reg(RS2)]) :-
				member(Suffix,[ci,co,cio]).
instr(and,c,[reg(D),reg(S1),reg(S2)]).
instr(and,Suffix,[reg(D),reg(S1),S2]) :-	
				member(Suffix,[none,u]).
instr(bb0,Suffix,[D,reg(S1),S2]) :-
				branch_suffix(Suffix).
instr(bb1,Suffix,[D,reg(S1),S2]) :-
				branch_suffix(Suffix).
instr(bcnd,Suffix,[D,reg(S1),S2]) :-
				branch_suffix(Suffix).
instr(br,Suffix,[D26]) :-	branch_suffix(Suffix).
instr(bsr,Suffix,[D26]) :-	branch_suffix(Suffix).
instr(clr,none,[reg(D),reg(S1),S2]).
instr(cmp,none,[reg(D),reg(S1),S2]).
instr(div,none,[reg(D),reg(S1),S2]).
instr(divu,none,[reg(D),reg(S1),S2]).
instr(ext,none,[reg(D),reg(S1),S2]).
instr(extu,none,[reg(D),reg(S1),S2]).
instr(fadd,Suffix,[reg(D),reg(S1),reg(S2)]) :-
				f3_suffix(Suffix).
instr(fcmp,Suffix,[reg(D),reg(S1),reg(S2)]) :-
				f3_suffix(Suffix).
instr(fdiv,Suffix,[reg(D),reg(S1),reg(S2)]) :-
				f3_suffix(Suffix).
instr(ff0,none,[reg(D),reg(S2)]).
instr(ff1,none,[reg(D),reg(S2)]).
instr(fldcr,none,[reg(D),fcr(S)]).
instr(flt,Suffix,[reg(D),reg(S2)]) :-
				member(Suffix,[ss,ds]).
instr(fmul,Suffix,[reg(D),reg(S1),reg(S2)]) :-
				f3_suffix(Suffix).
instr(fstcr,none,[fcr(S),reg(D)]).
instr(fsub,Suffix,[reg(D),reg(S1),reg(S2)]) :-
				f3_suffix(Suffix).
instr(fxcr,none,[reg(D),reg(S1),fcr(S2)]).
instr(int,Suffix,[reg(D),reg(S2)]) :-
				member(Suffix,[ss,sd]).
instr(jmp,Suffix,[reg(S2)]) :-	branch_suffix(Suffix).
instr(jsr,Suffix,[reg(S2)]) :-	branch_suffix(Suffix).
instr(ld,Suffix,[reg(D),reg(S1),S2]) :- 
				mem_suffix(Suffix).
instr(lda,Suffix,[reg(D),reg(S1),S2]) :-
				mem_suffix(Suffix).
instr(ld,Suffix,[reg(D),scale(S1,S2)]) :- 
				mem_suffix(Suffix).
instr(lda,Suffix,[reg(D),scale(S1,S2)]) :-
				mem_suffix(Suffix).
instr(ldcr,none,[reg(D),cr(S)]).
instr(mak,none,[reg(D),reg(S1),S2]).
instr(mask,none,[reg(D),reg(S1),S2]).
instr(mul,none,[reg(D),reg(S1),S2]).
instr(nint,Suffix,[reg(D),reg(S2)]) :- member(Suffix,[ss,sd]).
instr(or,c,[reg(D),reg(S1),reg(S2)]).
instr(or,Suffix,[reg(D),reg(S1),S2]) :-	
				member(Suffix,[none,u]).
instr(rot,none,[reg(D),reg(S1),S2]).
instr(rte,none,[]).
instr(set,none,[reg(D),reg(S1),S2]).
instr(st,Suffix,[reg(D),reg(S1),S2]) :-
				mem_suffix(Suffix).
instr(st,Suffix,[reg(D),scale(S1,S2)]) :-
				mem_suffix(Suffix).
instr(stcr,none,[reg(D),cr(S)]).
instr(sub,none,[reg(RD),reg(RS1),S2]).
instr(sub,Suffix,[reg(RD),reg(RS1),reg(RS2)]) :-
				member(Suffix,[ci,co,cio]).
instr(subu,none,[reg(RD),reg(RS1),S2]).
instr(subu,Suffix,[reg(RD),reg(RS1),reg(RS2)]) :-
				member(Suffix,[ci,co,cio]).
instr(tb0,none,[D,reg(S1),S2]).
instr(tb1,none,[D,reg(S1),S2]).
instr(tbnd,none,[reg(S1),reg(S2)]).
instr(tcnd,none,[D,reg(S1),S2]).
instr(trnc,Suffix,[reg(D),reg(S2)]) :-
				member(Suffix,[ss,sd]).
instr(xcr,none,[reg(D),reg(S1),cr(S2)]).
instr(xmem,Suffix,[reg(D),reg(S1),S2]) :- member(Suffix,[none,bu]).
instr(xor,c,[reg(D),reg(S1),reg(S2)]).
instr(xor,Suffix,[reg(D),reg(S1),S2]) :-	
				member(Suffix,[none,u]).


branch_suffix(n).
branch_suffix(none).

f3_suffix(sss).
f3_suffix(ssd).
f3_suffix(sds).
f3_suffix(sdd).
f3_suffix(dss).
f3_suffix(dsd).
f3_suffix(dds).
f3_suffix(ddd).


mem_suffix(b).
mem_suffix(bu).
mem_suffix(h).
mem_suffix(hu).
mem_suffix(none).
mem_suffix(d).

endmod.
