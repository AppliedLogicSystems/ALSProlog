/*=======================================================================*
 |		atr_inst.pro	
 |	Copyright (c) 1991-4 Applied Logic Systems, Inc.
 |
 |	-- SPARC abstract assembly language instruction database
 |
 | Author: Kevin Buettner
 | Creation: 1/10/91
 |
 | Module Name:	instructions
 | Exported Procedures:
 |	instruction(Opcode,Args,OutOpcode,InTab,OutTab)
 |		-- succeeds if Opcode and Args are of approprate type
 |		   giving the new OutOpCode.  InTab and OutTab are the
 |		   input and output database arguments.
 |	opcode(Opcode,InList,OutList)
 |		-- Parses InList leaving OutList and giving Opcode as
 |		   the return value.  This is a dcg rule.
 *=======================================================================*/

module instructions.

use labels.

export instruction/5.
export opcode/3.

intern([]) :- !.
intern([H|L]) :- functor(_,H,0), intern(L).
:- intern(['bn,a', 'bne,a', 'be,a', 'bg,a', 'ble,a', 'bge,a', 'bl,a',
	'bgu,a', 'bleu,a', 'bcc,a', 'bcs,a', 'bpos,a', 'bneg,a', 'bvc,a',
	'bvs,a', 'ba,a', 'b,a', 'blu,a', 'bgeu,a', 'bz,a', 'bnz,a',
	'fba,a', 'fbn,a', 'fbu,a', 'fbg,a', 'fbug,a', 'fbl,a', 'fbul,a', 
	'fblg,a', 'fbne,a', 'fbe,a', 'fbue,a', 'fbge,a', 'fbuge,a', 'fble,a',
	'fbule,a', 'fbo,a']).


/*
 * An opcode is considered to be an identifier.
 */

opcode(Opcode) --> [ident(Id)], check_anulled(Opcode,Id).
opcode(error('Identifier expected as opcode')) --> [].

check_anulled(Opcode,Id) --> [comma,ident(a)], !, {makeanulled(Id,Opcode),!}.
check_anulled(Id,Id) --> [].

makeanulled(bn,	'bn,a').
makeanulled(bne,'bne,a').
makeanulled(bnz,'bnz,a').	%% synonym for bne
makeanulled(be,	'be,a').
makeanulled(bz,	'bz,a').	%% synonym for be
makeanulled(bg,	'bg,a').
makeanulled(ble,'ble,a').
makeanulled(bge,'bge,a').
makeanulled(bl,	'bl,a').
makeanulled(bgu,'bgu,a').
makeanulled(bleu,'bleu,a').
makeanulled(bcc,'bcc,a').
makeanulled(bgeu,'bgeu,a').	%% synonym for bcc
makeanulled(bcs,'bcs,a').
makeanulled(blu,'blu,a').	%% synonym for bcs
makeanulled(bpos,'bpos,a').
makeanulled(bneg,'bneg,a').
makeanulled(bvc,'bvc,a').
makeanulled(bvs,'bvs,a').
makeanulled(ba,	'ba,a').
makeanulled(b,	'b,a').		%% synonym for ba
makeanulled(fba,'fba,a').
makeanulled(fbn,'fbn,a').
makeanulled(fbu,'fbu,a').
makeanulled(fbg,'fbg,a').
makeanulled(fbug,'fbug,a').
makeanulled(fbl,'fbl,a').
makeanulled(fbul,'fbul,a').
makeanulled(fblg,'fblg,a').
makeanulled(fbne,'fbne,a').
makeanulled(fbe,'fbe,a').
makeanulled(fbue,'fbue,a').
makeanulled(fbge,'fbge,a').
makeanulled(fbuge,'fbuge,a').
makeanulled(fble,'fble,a').
makeanulled(fbule,'fbule,a').
makeanulled(fbo,'fbo,a').
makeanulled(_,	error('Only branch instructions may be anulled')).


/*
 * An instruction is valid if it can be found in the instr database below
 * with the appropriate arguments.
 */

instruction(Op,Args,Op,Tab,Tab) :-
    instr(Op,Args),
    !.

%% Pseudo-ops
instr(text,[]).
instr(data,[]).
instr(global,[A]).
instr(word,[A]).
instr(align,[A]).
instr(skip,[A]).

%% Synthetic instructions
%%	These are supported by the sun assembler.  If we ever port to another
%%	machine with a different assembler, we can synthesize these ourselves
%%	in atr_out.pro.
%%
instr(set,[Val,Reg]).
instr(mov,[A1,A2]).
instr(cmp,[A1,A2]).
instr(jmp,[Address]).
instr(ret,[]).
instr(retl,[]).
instr(restore,[]).

/*
 * B.1 Load Integer Instructions
 */

/* LDSB		-- Load Signed Byte */
instr(ldsb,[Address,reg(D)]).

/* LDSBA	-- Load Signed Byte from Alternate space */
instr(ldsba,[Address,reg(D)]).

/* LDSH		-- Load Signed Halfword */
instr(ldsh,[Address,reg(D)]).

/* LDSHA	-- Load Signed Halfword from Alternate space */
instr(ldsha,[Address,reg(D)]).

/* LDUB		-- Load Unsigned Byte */
instr(ldub,[Address,reg(D)]).

/* LDUBA	-- Load Unsigned Byte from Alternate space */
instr(lduba,[Address,reg(D)]).

/* LDUH		-- Load Unsigned Halfword */
instr(lduh,[Address,reg(D)]).

/* LDUHA	-- Load Unsigned Halfword from Alternate space */
instr(lduha,[Address,reg(D)]).

/* LD		-- Load Word */
instr(ld,[Address,reg(D)]).

/* LDA		-- Load Word from Alternate space */
instr(lda,[Address,reg(D)]).

/* LDD		-- Load Doubleword */
instr(ldd,[Address,reg(D)]).

/* LDDA		-- Load Doubleword from Alternate space */
instr(ldda,[Address,reg(D)]).

/*
 * B.2 Load Floating-point Instructions
 *
 */

/* LDF		-- Load Floating-point register */
instr(ld,[Address,freg(FReg)]).
instr(ldd,[Address,freg(FReg)]).


/*
 * B.3 Load Coprocesor Instructions
 *
 * We will probably never need these so I'm skipping them.
 */

/*
 * B.4 Store Integer Instructions
 */

/* STB		-- Store Byte */
instr(stb,[reg(D),Address]).

/* STBA		-- Store Byte into Alternate space */
instr(stba,[reg(D),Address]).

/* STH		-- Store Halfword */
instr(sth,[reg(D),Address]).

/* STHA		-- Store Halfword into Alternate space */
instr(stha,[reg(D),Address]).

/* ST		-- Store Word */
instr(st,[reg(D),Address]).

/* STA		-- Store Word into Alternate space */
instr(sta,[reg(D),Address]).

/* STD		-- Store Doubleword */
instr(std,[reg(D),Address]).

/* STDA		-- Store Doubleword into Alternate space */
instr(stda,[reg(D),Address]).


/*
 * B.5 Store Floating-point Instructions
 *
 */

instr(st,[freg(F),Address]).
instr(std,[freg(F),Address]).


/*
 * B.6 Store Coprocessor Instructions
 *
 * We're skipping these too.
 */

/*
 * B.7 Atomic Load-Store Unsigned Byte Instructions
 */

/* LDSTUB	-- Atomic Load-Store Unsigned Byte */
instr(ldstub,[Address,D]).

/* LDSTUBA	-- Atomic Load-Store Unsigned Byte into Alternate space */
instr(ldstuba,[Address,D]).


/*
 * B.8		-- Swap r Register with Memory
 */

/* SWAP		-- SWAP r register with memory */
instr(swap,[Address,D]).

/* SWAPA	-- SWAP r register with Alternate space memory */
instr(swapa,[Address,D]).


/*
 * B.9		Add Instructions
 */

/* ADD		-- Add */
instr(add,[S1,S2,D]).

/* ADDcc	-- Add and modify icc */
instr(addcc,[S1,S2,D]).

/* ADDX		-- Add with Carry */
instr(addx,[S1,S2,D]).

/* ADDXcc	-- Add with Carry and modify icc */
instr(addxcc,[S1,S2,D]).


/*
 * B.10		Tagged Add Instructions
 *
 * Although we will probably never use these, I will put them in anyway.
 */

/* TADDcc	-- Tagged Add and modify icc */
instr(taddcc,[S1,S2,D]).

/* TADDccTV	-- Tagged Add, modify icc and Trap on Overflow */
instr(taddcctv,[S1,S2,D]).


/*
 * B.11		Subtract Instructions
 */

/* SUB		-- Subtract */
instr(sub,[S1,S2,D]).

/* SUBcc	-- Subtract and modify icc */
instr(subcc,[S1,S2,D]).

/* SUBX		-- Subtract with Carry */
instr(subx,[S1,S2,D]).

/* SUBXcc	-- Subtract with Carry and modify icc */
instr(subxcc,[S1,S2,D]).


/*
 * B.12		Tagged Subtract Instructions
 */

/* TSUBcc	-- Tagged Subtract and modify icc */
instr(tsubcc,[S1,S2,D]).

/* TSUBccTV	-- Tagged Subtract, modify icc and Trap on Overflow */
instr(tsubcctv,[S1,S2,D]).


/*
 * B.13		Multiply Step Instruction
 */

/* MULScc	-- Multiply Step and modify icc */
instr(mulscc,[S1,S2,D]).


/*
 * B.14		Logical Instructions
 */

/* AND		-- And */
instr(and,[S1,S2,D]).

/* ANDcc	-- And and modify icc */
instr(andcc,[S1,S2,D]).

/* ANDN		-- And Not */
instr(andn,[S1,S2,D]).

/* ANDNcc	-- And Not and modify icc */
instr(andncc,[S1,S2,D]).

/* OR		-- Inclusive Or */
instr(or,[S1,S2,D]).

/* ORcc		-- Inclusive Or and modify icc */
instr(orcc,[S1,S2,D]).

/* ORN		-- Inclusive Or Not */
instr(orn,[S1,S2,D]).

/* ORNcc	-- Inclusive Or Not and modify icc */
instr(orncc,[S1,S2,D]).

/* XOR		-- Exclusive Or */
instr(xor,[S1,S2,D]).

/* XORcc	-- Exclusive Or and modify icc */
instr(xorcc,[S1,S2,D]).

/* XNOR		-- Exclusive Nor */
instr(xnor,[S1,S2,D]).

/* XNORcc	-- Exclusive Nor and modify icc */
instr(xnorcc,[S1,S2,D]).


/*
 * B.15		Shift Instructions
 */

/* SLL		-- Shift Left Logical */
instr(sll,[S1,S2,D]).

/* SRL		-- Shift Right Logical */
instr(srl,[S1,S2,D]).

/* SRA		-- Shift Right Arithmetic */
instr(sra,[S1,S2,D]).


/*
 * B.16		SETHI Instruction
 */

/* SETHI	-- Set High */
instr(sethi,[C22,D]).


/*
 * B.17		SAVE and RESTORE instructions
 */

/* SAVE		-- Save caller's window */
instr(save,[S1,S2,D]).

/* RESTORE	-- Restore caller's window */
instr(restore,[S1,S2,D]).


/*
 * B.18		Branch on Integer Condition Instructions
 *
 *	The _a instructions represent the "annulled" version.
 */

/* BN		-- Branch Never */
instr(bn,[Disp22]).
instr('bn,a',[Disp22]).

/* BE		-- Branch on Equal */
instr(be,[Disp22]).
instr('be,a',[Disp22]).
instr(bz,[Disp22]).
instr('bz,a',[Disp22]).

/* BLE		-- Branch on Less or Equal */
instr(ble,[Disp22]).
instr('ble,a',[Disp22]).

/* BL		-- Branch on Less */
instr(bl,[Disp22]).
instr('bl,a',[Disp22]).

/* BLEU		-- Branch on Less or Equal Unsigned */
instr(bleu,[Disp22]).
instr('bleu,a',[Disp22]).

/* BCS		-- Branch on Carry Set (Less than, unsigned) */
instr(bcs,[Disp22]).
instr('bcs,a',[Disp22]).
instr(blu,[Disp22]).
instr('blu,a',[Disp22]).

/* BNEG		-- Branch on Negative */
instr(bneg,[Disp22]).
instr('bneg,a',[Disp22]).

/* BVS		-- Branch on Overflow Set */
instr(bvs,[Disp22]).
instr('bvs,a',[Disp22]).

/* BA		-- Branch Always */
instr(ba,[Disp22]).
instr('ba,a',[Disp22]).
instr(b,[Disp22]).
instr('b,a',[Disp22]).

/* BNE		-- Branch on Not Equal */
instr(bne,[Disp22]).
instr('bne,a',[Disp22]).
instr(bnz,[Disp22]).
instr('bnz,a',[Disp22]).

/* BG		-- Branch on Greater */
instr(bg,[Disp22]).
instr('bg,a',[Disp22]).

/* BGE		-- Branch on Greater or Equal */
instr(bge,[Disp22]).
instr('bge,a',[Disp22]).

/* BGU		-- Branch on Greater Unsigned */
instr(bgu,[Disp22]).
instr('bgu,a',[Disp22]).

/* BCC		-- Branch on Carry Clear (Greater than or Equal, Unsigned) */
instr(bcc,[Disp22]).
instr('bcc,a',[Disp22]).
instr(bgeu,[Disp22]).
instr('bgeu,a',[Disp22]).

/* BPOS		-- Branch on Positive */
instr(bpos,[Disp22]).
instr('bpos,a',[Disp22]).

/* BVC		-- Branch of Overflow Clear */
instr(bvc,[Disp22]).
instr('bvc,a',[Disp22]).


/*
 * B.19		Floating-point Branch on Condition Instructions
 *
 */

instr(fba,[Disp22]).
instr('fba,a',[Disp22]).
instr(fbn,[Disp22]).
instr('fbn,a',[Disp22]).
instr(fbu,[Disp22]).
instr('fbu,a',[Disp22]).
instr(fbg,[Disp22]).
instr('fbg,a',[Disp22]).
instr(fbug,[Disp22]).
instr('fbug,a',[Disp22]).
instr(fbl,[Disp22]).
instr('fbl,a',[Disp22]).
instr(fbul,[Disp22]).
instr('fbul,a',[Disp22]).
instr(fblg,[Disp22]).
instr('fblg,a',[Disp22]).
instr(fbne,[Disp22]).
instr('fbne,a',[Disp22]).
instr(fbe,[Disp22]).
instr('fbe,a',[Disp22]).
instr(fbue,[Disp22]).
instr('fbue,a',[Disp22]).
instr(fbge,[Disp22]).
instr('fbge,a',[Disp22]).
instr(fbuge,[Disp22]).
instr('fbuge,a',[Disp22]).
instr(fble,[Disp22]).
instr('fble,a',[Disp22]).
instr(fbule,[Disp22]).
instr('fbule,a',[Disp22]).
instr(fbo,[Disp22]).
instr('fbo,a',[Disp22]).


/*
 * B.20		Coprocessor Branch on Condition Instructions
 *
 * We are skipping these.
 */


/*
 * B.21		CALL instruction
 */

/* CALL		-- Call */
instr(call,[DISP]).


/*
 * B.22		Jump and Link Instruction
 */

/* JMPL		-- Jump and Link */
instr(jmpl,[Address,D]).


/*
 * B.23		Return from Trap Instruction
 *
 * I'm putting this in even though I doubt that it will ever get used.
 */

/* RETT		-- Return from Trap */
instr(rett,[Address]).


/*
 * B.24		Trap on Integer Condition Instruction
 */

/* TN		-- Trap Never */
instr(tn,[Address]).

/* TE		-- Trap on Equal */
instr(te,[Address]).

/* TLE		-- Trap on Less or Equal */
instr(tle,[Address]).

/* TL		-- Trap on Less */
instr(tl,[Address]).

/* TLEU		-- Trap on Less or Equal Unsigned */
instr(tleu,[Address]).

/* TCS		-- Trap on Carry Set (Less than, unsigned) */
instr(tcs,[Address]).

/* TNEG		-- Trap on Negative */
instr(tneg,[Address]).

/* TVS		-- Trap on Overflow Set */
instr(tvs,[Address]).

/* TA		-- Trap Always */
instr(ta,[Address]).

/* TNE		-- Trap on Not Equal */
instr(tne,[Address]).

/* TG		-- Trap on Greater */
instr(tg,[Address]).

/* TGE		-- Trap on Greater or Equal */
instr(tge,[Address]).

/* TGU		-- Trap on Greater Unsigned */
instr(tgu,[Address]).

/* TCC		-- Trap on Carry Clear (Greater than or Equal, Unsigned) */
instr(tcc,[Address]).

/* TPOS		-- Trap on Positive */
instr(tpos,[Address]).

/* TVC		-- Trap of Overflow Clear */
instr(tvc,[Address]).


/*
 * B.25		Read State Register Instructions
 */

/* RDY		-- Read Y register */
instr(rdy,[D]).

/* RDPSR	-- Read Processor State Register */
instr(rdpsr,[D]).

/* RDWIM	-- Read Window Invalid Mask register */
instr(rdwim,[D]).

/* RDTBR	-- Read Trap Base Register */
instr(rdtbr,[D]).


/*
 * B.26		Write State Register Instructions
 */

/* WRY		-- Write Y Register */
instr(wry,[S1,S2]).

/* WRPSR	-- Write Processor State Register */
instr(wrpsr,[S1,S2]).

/* WRWIM	-- Write Window Invalid Mask */
instr(wrwim,[S1,S2]).

/* WRTBR	-- Write Trap Base Register */
instr(wrtbr,[S1,S2]).


/*
 * B.27		Unimplemented Instruction
 */

/* UNIMP	-- Unimplemented */
instr(unimp,[C22]).


/*
 * B.28		Instruction Cache Flush Instruction
 */

/* IFLUSH	-- Instruction Cache Flush */
instr(flush,[Address]).


/*
 * B.29		Floating-point Operate (FPop) Instructions
 *
 */

instr(fitos,[freg(F1),freg(F2)]).	/* convert integer to single */
instr(fitod,[freg(F1),freg(F2)]).	/* convert integer to double */
instr(fitox,[freg(F1),freg(F2)]).	/* convert integer to extended */
instr(fstoi,[freg(F1),freg(F2)]).	/* convert single to integer */
instr(fdtoi,[freg(F1),freg(F2)]).	/* convert double to integer */
instr(fxtoi,[freg(F1),freg(F2)]).	/* convert extended to integer */
instr(fstod,[freg(F1),freg(F2)]).	/* convert single to double */
instr(fstox,[freg(F1),freg(F2)]).	/* convert single to extended */
instr(fdtos,[freg(F1),freg(F2)]).	/* convert double to single */
instr(fdtox,[freg(F1),freg(F2)]).	/* convert double to extended */
instr(fxtos,[freg(F1),freg(F2)]).	/* convert extended to single */
instr(fxtod,[freg(F1),freg(F2)]).	/* convert extended to double */
instr(fmovs,[freg(F1),freg(F2)]).	/* move */
instr(fnegs,[freg(F1),freg(F2)]).	/* negate */
instr(fabss,[freg(F1),freg(F2)]).	/* absolute value */
instr(fsqrts,[freg(F1),freg(F2)]).	/* square root single */
instr(fsqrtd,[freg(F1),freg(F2)]).	/* square root double */
instr(fsqrtx,[freg(F1),freg(F2)]).	/* square root extended */
instr(fadds,[freg(F1),freg(F2),freg(F3)]).	/* add single */
instr(faddd,[freg(F1),freg(F2),freg(F3)]).	/* add double */
instr(faddx,[freg(F1),freg(F2),freg(F3)]).	/* add extended */
instr(fsubs,[freg(F1),freg(F2),freg(F3)]).	/* subtract single */
instr(fsubd,[freg(F1),freg(F2),freg(F3)]).	/* subtract double */
instr(fsubx,[freg(F1),freg(F2),freg(F3)]).	/* subtract extended */
instr(fmuls,[freg(F1),freg(F2),freg(F3)]).	/* multiply single */
instr(fmuld,[freg(F1),freg(F2),freg(F3)]).	/* multiply double */
instr(fmulx,[freg(F1),freg(F2),freg(F3)]).	/* multiply extended */
instr(fdivs,[freg(F1),freg(F2),freg(F3)]).	/* divide single */
instr(fdivd,[freg(F1),freg(F2),freg(F3)]).	/* divide double */
instr(fdivx,[freg(F1),freg(F2),freg(F3)]).	/* divide extended */
instr(fcmps,[freg(F1),freg(F2)]).	/* compare single */
instr(fcmpd,[freg(F1),freg(F2)]).	/* compare double */
instr(fcmpx,[freg(F1),freg(F2)]).	/* compare extended */
instr(fcmpes,[freg(F1),freg(F2)]).	/* compare single and exception if */
					/* 	unordered */
instr(fcmped,[freg(F1),freg(F2)]).	/* compare double and exception if */
					/* 	unordered */
instr(fcmpex,[freg(F1),freg(F2)]).	/* compare extended and exception if */
					/* 	unordered */


/*
 * B.30		Coprocessor Operate Instructions
 *
 * We probably won't need these so I won't put them in.
 */

endmod.
