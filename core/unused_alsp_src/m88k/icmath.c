/*
 * icmath.c		-- stuff to emit math instructions
 *	Copyright (c) 1991 Applied Logic Systems, Inc.
 *	Copyright (c) 1991 Motorola Inc.
 *
 * Author:	Kevin Buettner, Motorola port by Scott Medeiros
 * Creation:	5/17/91
 * Revision History:
 *	Revised: 5/31/91,	SM		-- Motorola 88k port
 *		 8/19/91	SM		-- cleaned up and documented some
 *	Revised: mm/dd/yy	Who		-- Why and What
 */

#include "defs.h"

#include <stdio.h>

#include "wintcode.h"
#include "icode.h"
#include "icodegen.h"
#include "compile.h"
#include "machinst.h"
#include "codegen.h"
#include "rinfo.h"

#define MDISP(lab) (((char *) (lab)) - (char *) mth_base) 
#define MBASE S_REG

extern	void	mth_base	PARAMS(( void ));
extern	void	dcmp_base	PARAMS(( void ));
extern long mth_bot;
extern long mth_stk;

static Code *mth_stackadj;
static LAB mth_initlab1;
static LAB mth_initlab2;
static int stacksize;		/* number of elements in arithmetic stack */
static int stackmax;

#define ICODE(macro,str,func,obp) void func PARAMS(( long, long, long, long ));
#include "icodedef.h"


void
ic_mth_init1(isonlygoal,regmask,y,z)
    long isonlygoal;
    long regmask;
    long y,z;
{
    long offset=0;

    mth_stackadj = ic_ptr;

    /* the next instruction is overwritten by ic_mth_fin, afterwards, 
       when it is known how big the arithmetic stack needs to be.  */

    addu(OldE_REG,H_REG,ZERO_REG);
    andi(OldE_REG,OldE_REG,0xfff8);	

    /* the and doubleword aligns the arithmetic stack -- this assumes that 
       the stack has been allocated one word more than necessary, otherwise 
       doubleword aligning would make the stack one element too small.  
       This depends on the constant chosen in ic_mth_fin...  */

    oru(TMP1_REG,ZERO_REG,hi16(&mth_bot));
    RELOC_INFO(RELOC_GVAR_HI16,(((unsigned long)ic_ptr)-2),symidx_mth_bot)

    sti(OldE_REG,TMP1_REG,lo16(&mth_bot));
    RELOC_INFO(RELOC_GVAR_LO16,(((unsigned long)ic_ptr)-2),symidx_mth_bot)

    /*** move_const((int) mth_base , MBASE); ***/
    oru(MBASE,ZERO_REG,hi16((int)mth_base));
    RELOC_INFO(RELOC_GVAR_HI16,(((unsigned long)ic_ptr)-2),symidx_mth_base)
    ori(MBASE,MBASE,lo16((int)mth_base));
    RELOC_INFO(RELOC_GVAR_LO16,(((unsigned long)ic_ptr)-2),symidx_mth_base)

    bsrn(2);			/* jump 2 instructions ahead */
    cmp(TMP2_REG,SAFETY_REG,ZERO_REG);	/* in the delay slot */

    /* The above code sequence bsrn(2) cmp(tmp2,Safety,Zero) is used to 
       load up RET and do some useful work in the delay slot.  Note that 
       the above might also be better(?) encoded on the 88k
       as bsr(1); bcndn(gt0,SAFETY_REG,FLAB(mth_initlab1)) ...
       One fewer instruction, but waste the cycle in the delay slot.  */

    LABEL(mth_initlab1);
    bb1n(GT,TMP2_REG,0);
    /*** bb1n(GT,TMP2_REG,FLAB(mth_initlab1)); ***/

    addui(CP_REG,RET_REG,0x18);
    sti(H_REG,TMP1_REG,lo16(&mth_stk));
    RELOC_INFO(RELOC_GVAR_LO16,(((unsigned long)ic_ptr)-2),symidx_mth_stk)

    addui(H_REG,OldE_REG,4);
    LABEL(mth_initlab2);
    brn(0);
    /*** brn(FLAB(mth_initlab2)); ***/

    addui(MBASE,MBASE,  /* (char *) (dcmp_base) - (char *) mth_base */ MDISP(dcmp_base));

    /* Historical Note: MDISP used to add back the BIAS on each access via S, 
       having set up S biased (in the move_const emitted above).
       Since we don't need negative offsets - we can lay out the entry points 
       any way we wish, as indeed we did it (lo-hi : mth_base dbl_base dcmp_base)
       we always get +ve offsets, hence biasing on S is not necessary at all. */

    if (regmask) {
	long reg;
	for (reg=1; reg<32; reg++)
	    if (regmask & (1<<reg)) {
		offset -= 4;
		sti(reg,SP_REG,EBIAS+offset);
	    }
    }

    /* Can't addui a negative literal on the 88k, so subu instead. */
    subui(SP_REG,SP_REG,-(offset-16));  
    MOVE(UARG1_REG,A1_REG)
    MOVE(UARG2_REG,A2_REG)
    jsrn(TMP1_REG);
    MOVE(E_REG,OldE_REG)

}


extern	void	mth_aftercall	PARAMS((void));

void
ic_mth_init2(regmask,sadj,y,z)
    long regmask;
    long sadj;
    long y,z;
{
    Code delay_instr;
	long d;

    subui(SP_REG,E_REG,8);
    if (regmask) {
	long reg;
	long offset=0;
	for (reg=1; reg<32; reg++)
	    if (regmask & (1<<reg)) {
		offset -= 4;
		ldi(reg,SP_REG,EBIAS+offset)
	    }
    }

    delay_instr = *--ic_ptr;

    COMP_DISP(d,((Code *)mth_aftercall));
    bsrn(d);
	RELOC_INFO(RELOC_GVAR_OFF26,(ic_ptr-1),symidx_mth_aftercall)
    /*** bsrn(BLAB(mth_aftercall)); ***/

    ic_put(delay_instr);

    PATCHDISP(mth_initlab1);
    /*** FLABDCL(mth_initlab1) ***/
    PATCHDISP(mth_initlab2);
    /*** FLABDCL(mth_initlab2) ***/
    
    stacksize = 0;
    stackmax = 0;
}

void
ic_mth_fin(x,y,z,w)
    long x,y,z,w;
{
    /*
      How big need the arithmetic stack area be?  Well, as deep as 
      the computation gets (stackmax) +
      whatever overhead is needed for stuff like callouts and storing 
      double results.
      40 bytes = 10 words on the stack frame, is the same # used on the Sparc,
      and seems to be sufficient.
      By my calculations (??), the 10 words can be accounted for as follows:
  	    7 words for callout:
		    2 fences
		    CP, S, RET, RET2, Top Offset
	2 words to store a double at the end of a computation
	1 word to allow for doubleword alignment of the arithmetic stack
    */

    *mth_stackadj = ADDUI(OldE_REG,H_REG,40+stackmax*4);
}


extern	void	mth_eq		PARAMS(( void ));

void
ic_mth_eq(x,y,z,w)
    long x,y,z,w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_eq));
    jsr(TMP1_REG);
    stacksize -= 2;
}


extern	void	mth_lt		PARAMS(( void ));

void
ic_mth_lt(x,y,z,w)
    long x,y,z,w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_lt));
    jsr(TMP1_REG);
    stacksize -= 2;
}


extern	void	mth_gt		PARAMS(( void ));

void
ic_mth_gt(x,y,z,w)
    long x,y,z,w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_gt));
    jsr(TMP1_REG);
    stacksize -= 2;
}


extern	void	mth_le		PARAMS(( void ));

void
ic_mth_le(x,y,z,w)
    long x,y,z,w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_le));
    jsr(TMP1_REG);
    stacksize -= 2;
}


extern	void	mth_ge		PARAMS(( void ));

void
ic_mth_ge(x,y,z,w)
    long x,y,z,w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_ge));
    jsr(TMP1_REG);
    stacksize -= 2;
}


extern	void	mth_ne		PARAMS(( void ));

void
ic_mth_ne(x,y,z,w)
    long x,y,z,w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_ne));
    jsr(TMP1_REG);
    stacksize -= 2;
}


extern	void	mth_putnum		PARAMS(( void ));

void
ic_mth_getval(base,disp,z,w)
    long base, disp;
    long z,w;
{
    int reg;
    Code delay_instr;
	long d;


    addui(TMP1_REG,MBASE,MDISP(mth_putnum));
    jsr(TMP1_REG);

#ifdef notdef
    ADD(OldE_REG,imm(8),OldE_REG)		/* pop stack in delay slot */
#endif

    if (UARG2_REG != (reg = ic_get_operand(base,disp,UARG2_REG))) {
	MOVE(reg, UARG2_REG)
    }

    delay_instr = *--ic_ptr;

    COMP_DISP(d,((Code *)wm_unify));
    bsrn(d);
	RELOC_INFO(RELOC_GVAR_OFF26,(ic_ptr-1),symidx_wm_unify)
    /*** bsrn(BLAB(wm_unify)); ***/

    ic_put(delay_instr);

    stacksize -= 1;
}


extern	void	mth_getnum		PARAMS(( void ));
extern	void	mth_getnum0		PARAMS(( void ));

void
ic_mth_getnum(base,disp,z,w)
    long base,disp;
    long z,w;
{
    int reg;
    Code delay_instr;
    if (stacksize == 0) {
	if (UARG1_REG != (reg = ic_get_operand(base,disp,UARG1_REG))) {
	    MOVE(reg, UARG1_REG)
	}

	delay_instr = *--ic_ptr;

	addui(TMP1_REG,MBASE,MDISP(mth_getnum0));
     	jsrn(TMP1_REG);
    }
    else {
	if (UARG2_REG != (reg = ic_get_operand(base,disp,UARG2_REG))) {
	    MOVE(reg, UARG2_REG)
	}

	delay_instr = *--ic_ptr;

	addui(TMP1_REG,MBASE,MDISP(mth_getnum));
	jsrn(TMP1_REG);
    }

    ic_put(delay_instr);

    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


void
ic_mth_putnum(base,disp,z,w)
    long base, disp;
    long z,w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_putnum));
    jsr(TMP1_REG);

#ifdef notdef
    ADD(OldE_REG,imm(8),OldE_REG)		/* pop stack in delay slot */
#endif
    ic_put_operand(base,disp,UARG1_REG);

    stacksize -= 1;
}


extern	void	mth_push		PARAMS(( void ));
extern	void	mth_pushdbl0		PARAMS(( void ));


void
ic_mth_pushdbl(d0,d1,z,w)
    long d0;
    long d1;
    long z, w;
{
	long d;

    if (stacksize != 0) {
	/* Old TOS is about to be replaced, so push it onto the in-memory stack */
	addui(TMP1_REG,MBASE,MDISP(mth_push));
	jsrn(TMP1_REG);
	subui(OldE_REG,OldE_REG,8);		/* perform part of push in delay slot */
    }

    COMP_DISP(d,((Code *)mth_pushdbl0));
    bsr(d);
	RELOC_INFO(RELOC_GVAR_OFF26,(ic_ptr-1),symidx_mth_pushdbl0)
    /*** bsr(BLAB(mth_pushdbl0)); ***/

    ic_put(d0);				/* put down the pieces of the double */
    ic_put(d1);
    stacksize += 1;			/* stack is now bigger */
    if (stackmax < stacksize)
	stackmax = stacksize;		/* adjust maximum stack size */
}


extern	void	mth_pushint		PARAMS(( void ));
extern	void	mth_pushint0		PARAMS(( void ));

void
ic_mth_pushint(val,y,z,w)
    long val;
    long y, z, w;
{
    Code delay_instr;
    if (stacksize == 0) {
	move_const((unsigned long)val,UARG1_REG);
	delay_instr = *--ic_ptr;
	addui(TMP1_REG,MBASE,MDISP(mth_pushint0));
	jsrn(TMP1_REG);
    }
    else {
	move_const((unsigned long)val,UARG2_REG);
	delay_instr = *--ic_ptr;
	addui(TMP1_REG,MBASE,MDISP(mth_pushint));
	jsrn(TMP1_REG);
    }


    ic_put(delay_instr);
    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;

}


extern	void	mth_add		PARAMS(( void ));
void
ic_mth_add(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_add));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);		/* pop stack in delay slot */

    stacksize -= 1;
}


extern	void	mth_addi		PARAMS(( void ));
void
ic_mth_addi(i,x,y,z)
    long i;
    long x, y, z;
{
    Code delay_instr;
    move_const((unsigned long)i,UARG2_REG);
    delay_instr = *--ic_ptr;
    addui(TMP1_REG,MBASE,MDISP(mth_addi));
    jsrn(TMP1_REG);
    ic_put(delay_instr);
}


extern	void	mth_sub		PARAMS(( void ));
void
ic_mth_sub(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_sub));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);		/* pop stack in delay slot */
    stacksize -= 1;
}


extern	void	mth_subi		PARAMS(( void ));
void
ic_mth_subi(i,x,y,z)
    long i;
    long x, y, z;
{
    Code delay_instr;
    move_const((unsigned long)i,UARG2_REG);
    delay_instr = *--ic_ptr;
    addui(TMP1_REG,MBASE,MDISP(mth_subi));
    jsrn(TMP1_REG);
    ic_put(delay_instr);
}


extern	void	mth_mul		PARAMS(( void ));
void
ic_mth_mul(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_mul));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);		/* pop stack in delay slot */
    stacksize -= 1;
}


extern	void	mth_div		PARAMS(( void ));
void
ic_mth_div(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_div));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);		/* pop stack in delay slot */
    stacksize -= 1;
}


extern	void	mth_fdiv		PARAMS(( void ));
void
ic_mth_fdiv(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_fdiv));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);		/* pop stack in delay slot */
    stacksize -= 1;
}


extern	void	mth_mod		PARAMS(( void ));
void
ic_mth_mod(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_mod));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);		/* pop stack in delay slot */
    stacksize -= 1;
}


extern	void	mth_neg		PARAMS(( void ));
void
ic_mth_neg(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_neg));
    jsrn(TMP1_REG);
    subu(UARG2_REG,ZERO_REG,UARG1_REG);

}

extern	void	mth_band		PARAMS(( void ));
void
ic_mth_band(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_band));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);		/* pop stack in delay slot */

    stacksize -= 1;
}


extern	void	mth_bor		PARAMS(( void ));
void
ic_mth_bor(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_bor));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);

    stacksize -= 1;
}


extern	void	mth_bxor		PARAMS(( void ));
void
ic_mth_bxor(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_bxor));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);

    stacksize -= 1;
}


extern	void	mth_not		PARAMS(( void ));
void
ic_mth_not(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_not));
    jsr(TMP1_REG);
}


extern	void	mth_lshft		PARAMS(( void ));
void
ic_mth_lshft(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_lshft));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);

    stacksize -= 1;
}


extern	void	mth_rshft		PARAMS(( void ));
void
ic_mth_rshft(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_rshft));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);

    stacksize -= 1;
}


extern	void	mth_power		PARAMS(( void ));
void
ic_mth_power(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_power));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);

    stacksize -= 1;
}


extern	void	mth_abs		PARAMS(( void ));
void
ic_mth_abs(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_abs));
    jsr(TMP1_REG);

}

extern	void	mth_sin		PARAMS(( void ));
void
ic_mth_sin(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_sin));
    jsr(TMP1_REG);
}


extern	void	mth_cos		PARAMS(( void ));
void
ic_mth_cos(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_cos));
    jsr(TMP1_REG);
}


extern	void	mth_tan		PARAMS(( void ));
void
ic_mth_tan(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_tan));
    jsr(TMP1_REG);
}


extern	void	mth_asin		PARAMS(( void ));
void
ic_mth_asin(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_asin));
    jsr(TMP1_REG);
}


extern	void	mth_acos		PARAMS(( void ));
void
ic_mth_acos(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_acos));
    jsr(TMP1_REG);
}


extern	void	mth_atan		PARAMS(( void ));
void
ic_mth_atan(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_atan));
    jsr(TMP1_REG);
}


extern	void	mth_sqrt		PARAMS(( void ));
void
ic_mth_sqrt(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_sqrt));
    jsr(TMP1_REG);
}


extern	void	mth_exp		PARAMS(( void ));
void
ic_mth_exp(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_exp));
    jsr(TMP1_REG);
}


extern	void	mth_exp10		PARAMS(( void ));
void
ic_mth_exp10(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_exp10));
    jsr(TMP1_REG);
}


extern	void	mth_log		PARAMS(( void ));
void
ic_mth_log(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_log));
    jsr(TMP1_REG);
}


extern	void	mth_log10		PARAMS(( void ));
void
ic_mth_log10(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_log10));
    jsr(TMP1_REG);
}


extern	void	mth_floor		PARAMS(( void ));
void
ic_mth_floor(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_floor));
    jsr(TMP1_REG);
}


extern	void	mth_round		PARAMS(( void ));
void
ic_mth_round(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_round));
    jsr(TMP1_REG);
}


extern	void	mth_trunc		PARAMS(( void ));
void
ic_mth_trunc(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_trunc));
    jsr(TMP1_REG);
}


extern	void	mth_heapused		PARAMS(( void ));
void
ic_mth_heapused(x,y,z,w)
    long x, y, z, w;
{
    if (stacksize != 0) {
	/* Old TOS is about to be replaced, so push it onto the in-memory stack */
	addui(TMP1_REG,MBASE,MDISP(mth_push));
	jsrn(TMP1_REG);
	subui(OldE_REG,OldE_REG,8);	/* perform part of push in delay slot */
    }
    addui(TMP1_REG,MBASE,MDISP(mth_heapused));
    jsr(TMP1_REG);
    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


extern	void	mth_cputime		PARAMS(( void ));
void
ic_mth_cputime(x,y,z,w)
    long x, y, z, w;
{
    if (stacksize != 0) {
	addui(TMP1_REG,MBASE,MDISP(mth_push));
	jsrn(TMP1_REG);
	subui(OldE_REG,OldE_REG,8);	/* perform part of push in delay slot */
    }
    addui(TMP1_REG,MBASE,MDISP(mth_cputime));
    jsr(TMP1_REG);
    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


extern	void	mth_realtime		PARAMS(( void ));
void
ic_mth_realtime(x,y,z,w)
    long x, y, z, w;
{
    if (stacksize != 0) {
	addui(TMP1_REG,MBASE,MDISP(mth_push));
	jsrn(TMP1_REG);
	subui(OldE_REG,OldE_REG,8);	/* perform part of push in delay slot */
    }
    addui(TMP1_REG,MBASE,MDISP(mth_realtime));
    jsr(TMP1_REG);

    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


extern	void	mth_random		PARAMS(( void ));
void
ic_mth_random(x,y,z,w)
    long x, y, z, w;
{
    if (stacksize != 0) {
	addui(TMP1_REG,MBASE,MDISP(mth_push));
	jsrn(TMP1_REG);
	subui(OldE_REG,OldE_REG,8);	/* perform part of push in delay slot */
    }
    addui(TMP1_REG,MBASE,MDISP(mth_random));
    jsr(TMP1_REG);
    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}

extern	void	mth_callout_init		PARAMS(( void ));
void
ic_mth_callout_init(x,y,z,w)
    long x, y, z, w;
{
    if (stacksize != 0) {
	addui(TMP1_REG,MBASE,MDISP(mth_push));
	jsrn(TMP1_REG);
	subui(OldE_REG,OldE_REG,8);
    }

    addui(TMP1_REG,MBASE,MDISP(mth_callout_init));
    jsr(TMP1_REG);

    stacksize+=1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


extern	void	mth_callout		PARAMS(( void ));
void
ic_mth_callout(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_callout));
    ldi(UARG1_REG,SP_REG,EBIAS);
    jsrn(TMP1_REG);
    addui(SP_REG,SP_REG,4);
    /* previous line added by Kev (5-20-93) to pop the stack */
}


/*
 * The following were added by Kev, 5-18-93
 */

extern	void	mth_sinh		PARAMS(( void ));
void
ic_mth_sinh(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_sinh));
    jsr(TMP1_REG);
}


extern	void	mth_cosh		PARAMS(( void ));
void
ic_mth_cosh(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_cosh));
    jsr(TMP1_REG);
}


extern	void	mth_tanh		PARAMS(( void ));
void
ic_mth_tanh(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_tanh));
    jsr(TMP1_REG);
}


extern	void	mth_ceil		PARAMS(( void ));
void
ic_mth_ceil(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_ceil));
    jsr(TMP1_REG);
}


extern	void	mth_erf		PARAMS(( void ));
void
ic_mth_erf(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_erf));
    jsr(TMP1_REG);
}


extern	void	mth_erfc		PARAMS(( void ));
void
ic_mth_erfc(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_erfc));
    jsr(TMP1_REG);
}

extern	void	mth_gamma		PARAMS(( void ));
void
ic_mth_gamma(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_gamma));
    jsr(TMP1_REG);
}


extern	void	mth_j0		PARAMS(( void ));
void
ic_mth_j0(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_j0));
    jsr(TMP1_REG);
}

extern	void	mth_j1		PARAMS(( void ));
void
ic_mth_j1(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_j1));
    jsr(TMP1_REG);
}

extern	void	mth_y0		PARAMS(( void ));
void
ic_mth_y0(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_y0));
    jsr(TMP1_REG);
}

extern	void	mth_y1		PARAMS(( void ));
void
ic_mth_y1(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_y1));
    jsr(TMP1_REG);
}

extern	void	mth_atan2		PARAMS(( void ));
void
ic_mth_atan2(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_atan2));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);

    stacksize -= 1;
}

extern	void	mth_fmod		PARAMS(( void ));
void
ic_mth_fmod(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_fmod));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);

    stacksize -= 1;
}

extern	void	mth_hypot		PARAMS(( void ));
void
ic_mth_hypot(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_hypot));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);

    stacksize -= 1;
}

extern	void	mth_jn		PARAMS(( void ));
void
ic_mth_jn(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_jn));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);

    stacksize -= 1;
}

extern	void	mth_yn		PARAMS(( void ));
void
ic_mth_yn(x,y,z,w)
    long x, y, z, w;
{
    addui(TMP1_REG,MBASE,MDISP(mth_yn));
    jsrn(TMP1_REG);
    addui(OldE_REG,OldE_REG,8);

    stacksize -= 1;
}
