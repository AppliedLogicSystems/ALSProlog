/*
 * machinst.h		-- Code Generation macros for SPARC
 *	Copyright (c) 1990-1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin Buettner
 * Creation: 3/7/90
 * Revision History:
 */

#include "wintcode.h"
#include "icodegen.h"

extern	void	move_const	PARAMS(( long, long ));
extern	int	ic_get_operand	PARAMS(( long, long, long ));
extern	void	ic_put_operand	PARAMS(( long, long, long ));

/* Do not call these as functions... */
extern	void	wm_unify	PARAMS(( void ));
extern	void	wm_docut	PARAMS(( void ));
extern	void	wm_g_uia	PARAMS(( void ));
extern	void	wm_p_uia	PARAMS(( void ));
extern	void	wm_g_sym	PARAMS(( void ));
extern	void	wm_g_dbl	PARAMS(( void ));
extern	void	wm_u_lval	PARAMS(( void ));
extern	void	wm_p_unsafe	PARAMS(( void ));
extern	void	wm_u_sym	PARAMS(( void ));
extern	void	wm_u_int	PARAMS(( void ));
extern	void	wm_g_int	PARAMS(( void ));

/*
 * ic_put
 *
 *	This macro is responsible for writing the instruction in the code
 *	stream.  Note that the increment is done after the assignment, thus
 *	permitting the value of ic_ptr to be used in the expression on the
 *	right hand side of the assignment operator.
 *
 *	This was a problem in previous implementations in that the C compiler
 *	would do different things on different machines.  It is not clear
 *	whether the value taken for ic_ptr on the right hand side of the 
 *	assignment is before or after the increment of ic_ptr.  It actually
 *	did different things with different compilers.
 */

#define ic_put(dat) (*((long *) ic_ptr) = ((long) (dat))),ic_ptr++

/*
 * Labels:
 *
 *	A label may occur before it appears in a branch instruction of after.
 *	If it appears before, it is easy to compute the displacement when the
 *	instruction is emitted.  If it appears after, we must patch the
 *	displacement field at the time the label is defined.  To this end,
 *	we will distinguish between forward branches and backward branches by
 *	using different label declarations.
 *
 *	A label which occurs before its use in an instruction is declared with
 *
 *		BLABDCL(label)
 *
 *	When it is used in a branch instruction, it would be used as follows
 *
 *			BNE(BLAB(label))
 *
 *	It is permissible to have more than one branch back to a label since
 *	the label variable is fixed and referes to the position to branch back
 *	to.  A forward label variable on the other hand refers to the position
 *	to fix up.  Therefore, multiple forward branches to the same location
 *	should use different label variables.  A forward label is used in a
 *	branch instruction as follows:
 *
 *			BNE(FLAB(label))
 *
 *	FLAB actually records the location to fix up in the label variable.
 *	The displacement is patched with a forward label declaration which
 *	will appear as follows:
 *
 *		FLABDCL(label)
 *
 *	FLAB and FLABDCL may be only used with instructions which require
 *	22 bit displacements.  It will not work with the CALL instruction
 *	which uses a 30 bit displacement.
 *
 *
 */

#define BLABDCL(label) label = ic_ptr;
#define BLAB(label) (((Code *)(label))-ic_ptr)
#define FLAB(label) ((label=ic_ptr),0)
#define FLABDCL(label) *label |= ((ic_ptr-(label)) & 0x3fffff);

#define is22BitDispl(d) ((d) <= 0x1ffff && (d) >= -0x20000)

typedef Code * LABEL;

/*
 * Immediates:
 *
 *	Immediate values (sign extended 13 bit constants) are indicated by
 *	using the following construction:
 *
 *		imm(0x03)
 *
 *	In this case, the constant 3 is appropriately sign extended and the
 *	i bit will be set in the instruction field indicating that it is an
 *	immediate operand instead of a register.
 */

#define imm(val) (0x2000 | (0x1fff  & (val)))
#define immUpper 0x0fff
#define immLower 0xfffff000
#define hi22(val) (((int) (val)) >> 10)
#define lo10(val) (((int) (val)) & 0x03ff)


/*
 * Registers:
 *
 */

#define ZERO	0
#define UArg1	1
#define UArg2	2
#define tmp1	3
#define tmp2	4

#define H	8
#define HB	9
#define B	10
#define TR	11
#define SPB	12
#define E	13
#define cSP	14
#define RET	15
#define HeapBase 16
#define StkBot	17
#define OldE	18
#define CP	19
#define Fail	20
#define Safety	21
#define SP	22
#define S	23
#define A1	24
#define A2	25
#define A3	26
#define T1	27
#define T2	28
#define T3	29
#define cFP	30
#define cRET	31

/*
 * Shorthand Pseudo Instructions
 */

#define MOVE(src,dst)	ADD(ZERO,src,dst)
#define CMP(s1,s2)	SUBcc(s1,s2,ZERO)
#define NOP		SETHI(0,ZERO)


/*
 * B.1 Load Integer Instructions
 */

/* LDSB		-- Load Signed Byte */
#define LDSB(s1,s2,d) ic_put(iLDSB(s1,s2,d));

/* LDSBA	-- Load Signed Byte from Alternate space */
#define LDSBA(s1,s2,asi,d) ic_put(iLDSBA(s1,s2,asi,d));

/* LDSH		-- Load Signed Halfword */
#define LDSH(s1,s2,d) ic_put(iLDSH(s1,s2,d));

/* LDSHA	-- Load Signed Halfword from Alternate space */
#define LDSHA(s1,s2,asi,d) ic_put(iLDSHA(s1,s2,asi,d));

/* LDUB		-- Load Unsigned Byte */
#define LDUB(s1,s2,d) ic_put(iLDUB(s1,s2,d));

/* LDUBA	-- Load Unsigned Byte from Alternate space */
#define LDUBA(s1,s2,asi,d) ic_put(iLDUBA(s1,s2,asi,d));

/* LDUH		-- Load Unsigned Halfword */
#define LDUH(s1,s2,d) ic_put(iLDUH(s1,s2,d));

/* LDUHA	-- Load Unsigned Halfword from Alternate space */
#define LDUHA(s1,s2,asi,d) ic_put(iLDUHA(s1,s2,asi,d));

/* LD		-- Load Word */
#define LD(s1,s2,d) ic_put(iLD(s1,s2,d));

/* LDA		-- Load Word from Alternate space */
#define LDA(s1,s2,asi,d) ic_put(iLDA(s1,s2,asi,d));

/* LDD		-- Load Doubleword */
#define LDD(s1,s2,d) ic_put(iLDD(s1,s2,d));

/* LDDA		-- Load Doubleword from Alternate space */
#define LDDA(s1,s2,asi,d) ic_put(iLDDA(s1,s2,asi,d));

/*
 * B.2 Load Floating-point Instructions
 *
 * We're skipping these for right now as I don't anticipate needing them
 * in the near future (if at all).
 */

/*
 * B.3 Load Coprocesor Instructions
 *
 * We will probably never need these so I'm skipping them too.
 */

/*
 * B.4 Store Integer Instructions
 */

/* STB		-- Store Byte */
#define STB(d,s1,s2) ic_put(iSTB(d,s1,s2));

/* STBA		-- Store Byte into Alternate space */
#define STBA(d,s1,s2,asi) ic_put(iSTBA(d,s1,s2,asi));

/* STH		-- Store Halfword */
#define STH(d,s1,s2) ic_put(iSTH(d,s1,s2));

/* STHA		-- Store Halfword into Alternate space */
#define STHA(d,s1,s2,asi) ic_put(iSTHA(d,s1,s2,asi));

/* ST		-- Store Word */
#define ST(d,s1,s2) ic_put(iST(d,s1,s2));
#define ST_RELOC(d,s1,s2,rtype,rval) { \
  RELOC_INFO(rtype,ic_ptr,rval); \
  ST(d,s1,s2); }

/* STA		-- Store Word into Alternate space */
#define STA(d,s1,s2,asi) ic_put(iSTA(d,s1,s2,asi));

/* STD		-- Store Doubleword */
#define STD(d,s1,s2) ic_put(iSTD(d,s1,s2));

/* STDA		-- Store Doubleword into Alternate space */
#define STDA(d,s1,s2,asi) ic_put(iSTDA(d,s1,s2,asi));


/*
 * B.5 Store Floating-point Instructions
 *
 * We're skipping these too.
 */


/*
 * B.6 Store Coprocessor Instructions
 *
 * ...and these.
 */

/*
 * B.7 Atomic Load-Store Unsigned Byte Instructions
 */

/* LDSTUB	-- Atomic Load-Store Unsigned Byte */
#define LDSTUB(s1,s2,d) ic_put(iLDSTUB(s1,s2,d));

/* LDSTUBA	-- Atomic Load-Store Unsigned Byte into Alternate space */
#define LDSTUBA(s1,s2,d,asi) ic_put(iLDSTUBA(s1,s2,d,asi));


/*
 * B.8		-- Swap r Register with Memory
 */

/* SWAP		-- SWAP r register with memory */
#define SWAP(s1,s2,d) ic_put(iSWAP(s1,s2,d));

/* SWAPA	-- SWAP r register with Alternate space memory */
#define SWAPA(s1,s2,d,asi) ic_put(iSWAPA(s1,s2,d,asi));


/*
 * B.9		Add Instructions
 */

/* ADD		-- Add */
#define ADD(s1,s2,d) ic_put(iADD(s1,s2,d));
#define ADD_RELOC(s1,s2,d,rtype,rval) { \
   RELOC_INFO(rtype,ic_ptr,rval); \
   ADD(s1,s2,d); }

/* ADDcc	-- Add and modify icc */
#define ADDcc(s1,s2,d) ic_put(iADDcc(s1,s2,d));

/* ADDX		-- Add with Carry */
#define ADDX(s1,s2,d) ic_put(iADDX(s1,s2,d));

/* ADDXcc	-- Add with Carry and modify icc */
#define ADDXcc(s1,s2,d) ic_put(iADDXcc(s1,s2,d));


/*
 * B.10		Tagged Add Instructions
 *
 * Although we will probably never use these, I will put them in anyway.
 */

/* TADDcc	-- Tagged Add and modify icc */
#define TADDcc(s1,s2,d) ic_put(iTADDcc(s1,s2,d));

/* TADDccTV	-- Tagged Add, modify icc and Trap on Overflow */
#define TADDccTV(s1,s2,d) ic_put(iTADDccTV(s1,s2,d));


/*
 * B.11		Subtract Instructions
 */

/* SUB		-- Subtract */
#define SUB(s1,s2,d) ic_put(iSUB(s1,s2,d));

/* SUBcc	-- Subtract and modify icc */
#define SUBcc(s1,s2,d) ic_put(iSUBcc(s1,s2,d));

/* SUBX		-- Subtract with Carry */
#define SUBX(s1,s2,d) ic_put(iSUBX(s1,s2,d));

/* SUBXcc	-- Subtract with Carry and modify icc */
#define SUBXcc(s1,s2,d) ic_put(iSUBXcc(s1,s2,d));


/*
 * B.12		Tagged Subtract Instructions
 */

/* TSUBcc	-- Tagged Subtract and modify icc */
#define TSUBcc(s1,s2,d) ic_put(iTSUBcc(s1,s2,d));

/* TSUBccTV	-- Tagged Subtract, modify icc and Trap on Overflow */
#define TSUBccTV(s1,s2,d) ic_put(iTSUBccTV(s1,s2,d));


/*
 * B.13		Multiply Step Instruction
 */

/* MULScc	-- Multiply Step and modify icc */
#define MULScc(s1,s2,d) ic_put(iMULScc(s1,s2,d));


/*
 * B.14		Logical Instructions
 */

/* AND		-- And */
#define AND(s1,s2,d) ic_put(iAND(s1,s2,d));

/* ANDcc	-- And and modify icc */
#define ANDcc(s1,s2,d) ic_put(iANDcc(s1,s2,d));

/* ANDN		-- And Not */
#define ANDN(s1,s2,d) ic_put(iANDN(s1,s2,d));

/* ANDNcc	-- And Not and modify icc */
#define ANDNcc(s1,s2,d) ic_put(iANDNcc(s1,s2,d));

/* OR		-- Inclusive Or */
#define OR(s1,s2,d) ic_put(iOR(s1,s2,d));

/* ORcc		-- Inclusive Or and modify icc */
#define ORcc(s1,s2,d) ic_put(iORcc(s1,s2,d));

/* ORN		-- Inclusive Or Not */
#define ORN(s1,s2,d) ic_put(iORN(s1,s2,d));

/* ORNcc	-- Inclusive Or Not and modify icc */
#define ORNcc(s1,s2,d) ic_put(iORNcc(s1,s2,d));

/* XOR		-- Exclusive Or */
#define XOR(s1,s2,d) ic_put(iXOR(s1,s2,d));

/* XORcc	-- Exclusive Or and modify icc */
#define XORcc(s1,s2,d) ic_put(iXORcc(s1,s2,d));

/* XNOR		-- Exclusive Nor */
#define XNOR(s1,s2,d) ic_put(iXNOR(s1,s2,d));

/* XNORcc	-- Exclusive Nor and modify icc */
#define XNORcc(s1,s2,d) ic_put(iXNORcc(s1,s2,d));


/*
 * B.15		Shift Instructions
 */

/* SLL		-- Shift Left Logical */
#define SLL(s1,s2,d) ic_put(iSLL(s1,s2,d));

/* SRL		-- Shift Right Logical */
#define SRL(s1,s2,d) ic_put(iSRL(s1,s2,d));

/* SRA		-- Shift Right Arithmetic */
#define SRA(s1,s2,d) ic_put(iSRA(s1,s2,d));


/*
 * B.16		SETHI Instruction
 */

/* SETHI	-- Set High */
#define SETHI(c22,d) ic_put(iSETHI(c22,d));
#define SETHI_RELOC(c22,d,rtype,rval) { \
    RELOC_INFO(rtype,ic_ptr,rval); SETHI(c22,d); }

/*
 * B.17		SAVE and RESTORE instructions
 */

/* SAVE		-- Save caller's window */
#define SAVE(s1,s2,d) ic_put(iSAVE(s1,s2,d));

/* RESTORE	-- Restore caller's window */
#define RESTORE(s1,s2,d) ic_put(iRESTORE(s1,s2,d));


/*
 * B.18		Branch on Integer Condition Instructions
 *
 *	The _a instructions represent the "annulled" version.
 */

/* BN		-- Branch Never */
#define BN(disp22) ic_put(iBN(disp22));
#define BN_a(disp22) ic_put(iBN_a(disp22));

/* BE		-- Branch on Equal */
#define BE(disp22) ic_put(iBE(disp22));
#define BE_a(disp22) ic_put(iBE_a(disp22));

/* BLE		-- Branch on Less or Equal */
#define BLE(disp22) ic_put(iBLE(disp22));
#define BLE_a(disp22) ic_put(iBLE_a(disp22));

/* BL		-- Branch on Less */
#define BL(disp22) ic_put(iBL(disp22));
#define BL_a(disp22) ic_put(iBL_a(disp22));

/* BLEU		-- Branch on Less or Equal Unsigned */
#define BLEU(disp22) ic_put(iBLEU(disp22));
#define BLEU_a(disp22) ic_put(iBLEU_a(disp22));

/* BCS		-- Branch on Carry Set (Less than, unsigned) */
#define BCS(disp22) ic_put(iBCS(disp22));
#define BCS_a(disp22) ic_put(iBCS_a(disp22));

/* BNEG		-- Branch on Negative */
#define BNEG(disp22) ic_put(iBNEG(disp22));
#define BNEG_a(disp22) ic_put(iBNEG_a(disp22));

/* BVS		-- Branch on Overflow Set */
#define BVS(disp22) ic_put(iBVS(disp22));
#define BVS_a(disp22) ic_put(iBVS_a(disp22));

/* BA		-- Branch Always */
#define BA(disp22) ic_put(iBA(disp22));
#define BA_RELOC(disp22,rtype,rval) { \
	  RELOC_INFO(rtype,ic_ptr,rval); \
	  BA(disp22); }
#define BA_a(disp22) ic_put(iBA_a(disp22));

/* BNE		-- Branch on Not Equal */
#define BNE(disp22) ic_put(iBNE(disp22));
#define BNE_a(disp22) ic_put(iBNE_a(disp22));

/* BG		-- Branch on Greater */
#define BG(disp22) ic_put(iBG(disp22));
#define BG_a(disp22) ic_put(iBG_a(disp22));

/* BGE		-- Branch on Greater or Equal */
#define BGE(disp22) ic_put(iBGE(disp22));
#define BGE_a(disp22) ic_put(iBGE_a(disp22));

/* BGU		-- Branch on Greater Unsigned */
#define BGU(disp22) ic_put(iBGU(disp22));
#define BGU_a(disp22) ic_put(iBGU_a(disp22));

/* BCC		-- Branch on Carry Clear (Greater than or Equal, Unsigned) */
#define BCC(disp22) ic_put(iBCC(disp22));
#define BCC_a(disp22) ic_put(iBCC_a(disp22));

/* BPOS		-- Branch on Positive */
#define BPOS(disp22) ic_put(iBPOS(disp22));
#define BPOS_a(disp22) ic_put(iBPOS_a(disp22));

/* BVC		-- Branch of Overflow Clear */
#define BVC(disp22) ic_put(iBVC(disp22));
#define BVC_a(disp22) ic_put(iBVC_a(disp22));


/*
 * B.19		Floating-point Branch on Condition Instructions
 *
 * We are skipping these.
 */


/*
 * B.20		Coprocessor Branch on Condition Instructions
 *
 * We are skipping these too.
 */


/*
 * B.21		CALL instruction
 */

/* CALL		-- Call */
#define CALL(disp) ic_put(iCALL(disp));
#define CALL_RELOC(disp,rtype,rval) { \
	RELOC_INFO(rtype, ic_ptr, rval);  \
	CALL(disp); }

/*
 * B.22		Jump and Link Instruction
 */

/* JMPL		-- Jump and Link */
#define JMPL(s1,s2,d) ic_put(iJMPL(s1,s2,d));


/*
 * B.23		Return from Trap Instruction
 *
 * I'm putting this in even though I doubt that it will ever get used.
 */

/* RETT		-- Return from Trap */
#define RETT(s1,s2) ic_put(iRETT(s1,s2));


/*
 * B.24		Trap on Integer Condition Instruction
 */

/* TN		-- Trap Never */
#define TN(s1,s2) ic_put(iTN(s1,s2));

/* TE		-- Trap on Equal */
#define TE(s1,s2) ic_put(iTE(s1,s2));

/* TLE		-- Trap on Less or Equal */
#define TLE(s1,s2) ic_put(iTLE(s1,s2));

/* TL		-- Trap on Less */
#define TL(s1,s2) ic_put(iTL(s1,s2));

/* TLEU		-- Trap on Less or Equal Unsigned */
#define TLEU(s1,s2) ic_put(iTLEU(s1,s2));

/* TCS		-- Trap on Carry Set (Less than, unsigned) */
#define TCS(s1,s2) ic_put(iTCS(s1,s2));

/* TNEG		-- Trap on Negative */
#define TNEG(s1,s2) ic_put(iTNEG(s1,s2));

/* TVS		-- Trap on Overflow Set */
#define TVS(s1,s2) ic_put(iTVS(s1,s2));

/* TA		-- Trap Always */
#define TA(s1,s2) ic_put(iTA(s1,s2));

/* TNE		-- Trap on Not Equal */
#define TNE(s1,s2) ic_put(iTNE(s1,s2));

/* TG		-- Trap on Greater */
#define TG(s1,s2) ic_put(iTG(s1,s2));

/* TGE		-- Trap on Greater or Equal */
#define TGE(s1,s2) ic_put(iTGE(s1,s2));

/* TGU		-- Trap on Greater Unsigned */
#define TGU(s1,s2) ic_put(iTGU(s1,s2));

/* TCC		-- Trap on Carry Clear (Greater than or Equal, Unsigned) */
#define TCC(s1,s2) ic_put(iTCC(s1,s2));

/* TPOS		-- Trap on Positive */
#define TPOS(s1,s2) ic_put(iTPOS(s1,s2));

/* TVC		-- Trap of Overflow Clear */
#define TVC(s1,s2) ic_put(iTVC(s1,s2));


/*
 * B.25		Read State Register Instructions
 */

/* RDY		-- Read Y register */
#define RDY(d) ic_put(iRDY(d));

/* RDPSR	-- Read Processor State Register */
#define RDPSR(d) ic_put(iRDPSR(d));

/* RDWIM	-- Read Window Invalid Mask register */
#define RDWIM(d) ic_put(iRDWIM(d));

/* RDTBR	-- Read Trap Base Register */
#define RDTBR(d) ic_put(iRDTBR(d));


/*
 * B.26		Write State Register Instructions
 */

/* WRY		-- Write Y Register */
#define WRY(s1,s2) ic_put(iWRY(s1,s2));

/* WRPSR	-- Write Processor State Register */
#define WRPSR(s1,s2) ic_put(iWRPSR(s1,s2));

/* WRWIM	-- Write Window Invalid Mask */
#define WRWIM(s1,s2) ic_put(iWRWIM(s1,s2));

/* WRTBR	-- Write Trap Base Register */
#define WRTBR(s1,s2) ic_put(iWRTBR(s1,s2));


/*
 * B.27		Unimplemented Instruction
 */

/* UNIMP	-- Unimplemented */
#define UNIMP(c22) ic_put(iUNIMP(c22));


/*
 * B.28		Instruction Cache Flush Instruction
 */

/* IFLUSH	-- Instruction Cache Flush */
#define IFLUSH(s1,s2) ic_put(iIFLUSH(s1,s2));


/*
 * B.29		Floating-point Operate (FPop) Instructions
 *
 * We probably won't need these so I won't put them in.
 */


/*
 * B.30		Coprocessor Operate Instructions
 *
 * I'm not putting these in either.
 */
