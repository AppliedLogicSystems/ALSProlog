/*
 * machinst.h			-- include file for 88000 code generator
 *	Copyright (c) 1987 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation Date: 3/22/87
 * Revision History:
 *	May, 1988	Kev	-- 88K port
 *	Feb 6, 1990	Kev	-- SP reg change for bcs
 * 	July, 1991	SM	-- added label stuff for domath.88k stuff
 */

#include "wintcode.h"
#include "icodegen.h"

/*
 * The number of bytes needed for the garbage collector information. This
 * must be a multiple of four.
 */

#define GC_INFO_SIZE 12

/*
 * The stack bias
 */

#define EBIAS 32768


/*
 * Certain bit assignments
 */

#define BREF 31
#define BSYM 30
#define BNUM 29
#define BUIA 28
#define BLIST 27
#define BSTRC 26
#define BSIGN 25

/*
 * Masks for creating structured types
 */

#define LIST_MASK 0x8800
#define STRC_MASK 0x8400
#define INT_MASK  0xa000
#define SYM_MASK  0xc000


/* 
 * Register Assignments
 */

#define ZERO_REG 0
#define RET_REG 1
#define A1_REG 2
#define A2_REG 3
#define A3_REG 4
#define S_REG 5
#define T1_REG 6
#define T2_REG 7
#define T3_REG 8
#define T4_REG 9
#define UARG1_REG 10
#define UARG2_REG 11
#define TMP1_REG 12
#define TMP2_REG 13
#define OldE_REG 14
#define CP_REG	15
#define TR_REG	16
#define H_REG	17
#define FAIL_REG 18
#define SPB_REG	19
#define HB_REG	20
#define B_REG	21
#define OV_REG	22
#define SAFETY_REG 22
#define SP_REG	23
#define HEAPBASE_REG 24
#define STKBOT_REG 25
#define E_REG	30


#define ic_put(data) *ic_ptr++ = (data)

/* Labels as used by icode1 */

#define LABEL(lab) lab = ic_ptr
#define COMP_DISP(d,lab) (d)=(lab)-ic_ptr
#define PATCHDISP(d) *(d) |= ic_ptr-(d)

/*
 * Labels - for icmath.c  (backwards and forwards):
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
 *	FLABDCL actually records the location to fix up in the label variable.
 *	The displacement is patched with a forward label declaration which
 *	will appear as follows:
 *
 *		FLABDCL(label)
 *
 *	FLAB and FLABDCL may be only used with instructions which require
 *	26 bit displacements.  
 *
 *
 */

#define BLABDCL(label) label = ic_ptr;
#define BLAB(label) (((Code *)(label))-ic_ptr+1)  /* Why do I need the +1??  Umm, it works... */
#define FLAB(label) ((label=ic_ptr),0)
#define FLABDCL(label) *label |= ((ic_ptr-(label)) & 0x3ffffff);

typedef Code * LAB;

extern long *ic_ptr;
extern long *ic_macropatch1;
extern long *ic_failaddr;


#include "imm.h"

extern	long	ic_get_operand	PARAMS(( long, long, long ));
extern	void	ic_put_operand	PARAMS(( long, long, long ));
extern	void	move_const	PARAMS(( unsigned long, long ));

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

/* Shorthand Pseudo Instructions */

#define MOVE(src,dst)	addu(dst,src,ZERO_REG);

/*
 * See codegen.h for definitions of ADD, ADDCI, etc.
 */

#define add(rd,rs1,rs2)		ic_put(ADD(rd,rs1,rs2))
#define addci(rd,rs1,rs2)	ic_put(ADDCI(rd,rs1,rs2))
#define addco(rd,rs1,rs2)	ic_put(ADDCO(rd,rs1,rs2))
#define addcio(rd,rs1,rs2)	ic_put(ADDCIO(rd,rs1,rs2))
#define addi(rd,rs1,lit16)	ic_put(ADDI(rd,rs1,lit16))
#define addu(rd,rs1,rs2)	ic_put(ADDU(rd,rs1,rs2))
#define adduci(rd,rs1,rs2)	ic_put(ADDUCI(rd,rs1,rs2))
#define adduco(rd,rs1,rs2)	ic_put(ADDUCO(rd,rs1,rs2))
#define addcio(rd,rs1,rs2)	ic_put(ADDCIO(rd,rs1,rs2))
#define addui(rd,rs1,lit16)	ic_put(ADDUI(rd,rs1,lit16))
#define and(rd,rs1,rs2)		ic_put(AND(rd,rs1,rs2))
#define andc(rd,rs1,rs2)	ic_put(ANDC(rd,rs1,rs2))
#define andi(rd,rs1,lit16)	ic_put(ANDI(rd,rs1,lit16))
#define andiu(rd,rs1,lit16)	ic_put(ANDIU(rd,rs1,lit16))
#define bb0(b5,rs1,off16)	ic_put(BB0(b5,rs1,off16))
#define bb0n(b5,rs1,off16)	ic_put(BB0N(b5,rs1,off16))
#define bb1(b5,rs1,off16)	ic_put(BB1(b5,rs1,off16))
#define bb1n(b5,rs1,off16)	ic_put(BB1N(b5,rs1,off16))
#define bcnd(m5,rs1,off16)	ic_put(BCND(m5,rs1,off16))
#define bcndn(m5,rs1,off16)	ic_put(BCNDN(m5,rs1,off16))
#define br(off26)		ic_put(BR(off26))
#define brn(off26)		ic_put(BRN(off26))
#define bsr(off26)		ic_put(BSR(off26))
#define bsrn(off26)		ic_put(BSRN(off26))
#define clri(rd,rs1,w5,o5)	ic_put(CLRI(rd,rs1,w5,o5))
#define clr(rd,rs1,rs2)		ic_put(CLR(rd,rs1,rs2))
#define cmp(rd,rs1,rs2)		ic_put(CMP(rd,rs1,rs2))
#define cmpi(rd,rs1,lit16)	ic_put(CMPI(rd,rs1,lit16))
#define div(rd,rs1,rs2)		ic_put(DIV(rd,rs1,rs2))
#define divi(rd,rs1,lit16)	ic_put(DIVI(rd,rs1,lit16))
#define divu(rd,rs1,rs2)	ic_put(DIVU(rd,rs1,rs2))
#define divui(rd,rs1,lit16)	ic_put(DIVUI(rd,rs1,lit16))
#define exti(rd,rs1,w5,o5)	ic_put(EXTI(rd,rs1,w5,o5))
#define ext(rd,rs1,rs2)		ic_put(EXT(rd,rs1,rs2))
#define extui(rd,rs1,w5,o5)	ic_put(EXTUI(rd,rs1,w5,o5))
#define extu(rd,rs1,rs2)	ic_put(EXTU(rd,rs1,rs2))
#define ff0(rd,rs2)		ic_put(FF0(rd,rs2))
#define ff1(rd,rs2)		ic_put(FF1(rd,rs2))
#define jmp(rs2)		ic_put(JMP(rs2))
#define jmpn(rs2)		ic_put(JMPN(rs2))
#define jsr(rs2)		ic_put(JSR(rs2))
#define jsrn(rs2)		ic_put(JSRN(rs2))

/*
#ifdef silicon_filter
#define ldi(rd,rs1,lit16)	ic_put(LDI(rd,rs1,lit16)); tb1(0,0,511)
#else
#define ldi(rd,rs1,lit16)	ic_put(LDI(rd,rs1,lit16));
#endif
*/
#define ldi(rd,rs1,lit16)	ic_put(LDI(rd,rs1,lit16));

#define ldhui(rd,rs1,lit16)	ic_put(LDHUI(rd,rs1,lit16))
#define ld(rd,rs1,rs2)		ic_put(LD(rd,rs1,rs2))
#define lds(rd,rs1,rs2)		ic_put(LDS(rd,rs1,rs2))
#define ldai(rd,rs1,lit16)	ic_put(LDAI(rd,rs1,lit16))
#define lda(rd,rs1,rs2)		ic_put(LDA(rd,rs1,rs2))
#define ldas(rd,rs1,rs2)	ic_put(LDAS(rd,rs1,rs2))
#define	ldcr(rd,crs)		ic_put(LDCR(rd,crs))
#define maki(rd,rs1,w5,o5)	ic_put(MAKI(rd,rs1,w5,o5))
#define mak(rd,rs1,rs2)		ic_put(MAK(rd,rs1,rs2))
#define mask(rd,rs1,lit16)	ic_put(MASK(rd,rs1,lit16))
#define masku(rd,rs1,lit16)	ic_put(MASKU(rd,rs1,lit16))
#define mul(rd,rs1,rs2)		ic_put(MUL(rd,rs1,rs2))
#define muli(rd,rs1,lit16)	ic_put(MULI(rd,rs1,lit16))
#define or(rd,rs1,rs2)		ic_put(OR(rd,rs1,rs2))
#define orc(rd,rs1,rs2)		ic_put(ORC(rd,rs1,rs2))
#define ori(rd,rs1,rs2)		ic_put(ORI(rd,rs1,rs2))
#define oru(rd,rs1,rs2)		ic_put(ORU(rd,rs1,rs2))
#define roti(rd,rs1,o5)		ic_put(ROTI(rd,rs1,o5))
#define rot(rd,rs1,rs2)		ic_put(ROT(rd,rs1,rs2))
#define rte			ic_put(RTE)
#define seti(rd,rs1,w5,o5)	ic_put(SETI(rd,rs1,w5,o5))
#define set(rd,rs1,rs2)		ic_put(SET(rd,rs1,rs2))
#define sti(rs,rs1,lit16)	ic_put(STI(rs,rs1,lit16))
#define st(rs,rs1,rs2)		ic_put(ST(rs,rs1,rs2))
#define sts(rs,rs1,rs2)		ic_put(STS(rs,rs1,rs2))
#define stcr(rs,crd)		ic_put(STCR(rs,crd))
#define sub(rd,rs1,rs2)		ic_put(SUB(rd,rs1,rs2))
#define subci(rd,rs1,rs2)	ic_put(SUBCI(rd,rs1,rs2))
#define subco(rd,rs1,rs2)	ic_put(SUBCO(rd,rs1,rs2))
#define subcio(rd,rs1,rs2)	ic_put(SUBCIO(rd,rs1,rs2))
#define subi(rd,rs1,lit16)	ic_put(SUBI(rd,rs1,lit16))
#define subu(rd,rs1,rs2)	ic_put(SUBU(rd,rs1,rs2))
#define subci(rd,rs1,rs2)	ic_put(SUBCI(rd,rs1,rs2))
#define subuco(rd,rs1,rs2)	ic_put(SUBUCO(rd,rs1,rs2))
#define subucio(rd,rs1,rs2)	ic_put(SUBUCIO(rd,rs1,rs2))
#define subui(rd,rs1,lit16)	ic_put(SUBUI(rd,rs1,lit16))
#define tb0(b5,rs1,vec9)	ic_put(TB0(b5,rs1,vec9))
#define tb1(b5,rs1,vec9)	ic_put(TB1(b5,rs1,vec9))
#define tbndi(rs1,lit16)	ic_put(TBNDI(rs1,lit16))
#define tbnd(rs1,rs2)		ic_put(TBND(rs1,rs2))
#define tcnd(m5,rs1,vec9)	ic_put(TCND(m5,rs1,vec9))
#define xcr(rd,rs,cr)		ic_put(XCR(rd,rs,cr))
#define xmemi(rd,rs1,lit16)	ic_put(XMEMI(rd,rs1,lit16))
#define xmem(rd,rs1,rs2)	ic_put(XMEM(rd,rs1,rs2))
#define xmems(rd,rs1,rs2)	ic_put(XMEMS(rd,rs1,rs2))
#define xor(rd,rs1,rs2)		ic_put(XOR(rd,rs1,rs2))
#define xori(rd,rs1,lit16)	ic_put(XORI(rd,rs1,lit16))
#define xoriu(rd,rs1,lit16)	ic_put(XORIU(rd,rs1,lit16))
#define xorc(rd,rs1,rs2)	ic_put(XORC(rd,rs1,rs2))
