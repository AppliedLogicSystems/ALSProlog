/*
 * icmath.c		-- stuff to emit math instructions
 *	Copyright (c) 1991-1993 Applied Logic Systems, Inc.
 *
 * Author:	Kevin A. Buettner
 * Creation:	2/25/91
 * Revision History:
 *	Revised: mm/dd/yy	Who	-- Why and What
 */

#include "defs.h"

#include <stdio.h>

#include "icom.h"
#include "icode.h"
#include "compile.h"
#include "wintcode.h"
#include "machinst.h"
#include "codegen.h"
#include "rinfo.h"

#define MDISP(lab) imm(((char *) (lab)) - (char *) mth_base)
extern	void	mth_base	PARAMS(( void ));
extern	void	dcmp_base	PARAMS(( void ));
extern long mth_bot;
extern long mth_stk;

static Code *mth_stackadj;
static LABEL mth_initlab1;
static LABEL mth_initlab2;
static int stacksize;		/* number of elements in arithmetic stack */
static int stackmax;

#define ICODE(macro,str,func,obp) void func PARAMS(( long, long, long, long ));
#include "icodedef.h"


void
ic_mth_init1(isonlygoal,regmask,y,z)
    long isonlygoal;
    long regmask;
    long y, z;
{
    int offset=0;
    mth_stackadj = ic_ptr;
    ADD(H,imm(0),OldE)
    ANDN(OldE,imm(7),OldE)

    SETHI_RELOC(hi22(&mth_bot),tmp1,
				RELOC_GVAR_HI22,symidx_mth_bot);
    ST_RELOC(OldE,tmp1,imm(lo10(&mth_bot)),
			 RELOC_GVAR_LO10,symidx_mth_bot);

    /*** move_const((int) mth_base, S); ***/
    SETHI_RELOC(hi22((int)mth_base),S,
				RELOC_GVAR_HI22,symidx_mth_base);
    ADD_RELOC(S,imm(lo10((int)mth_base)),S,
			  RELOC_GVAR_LO10,symidx_mth_base);

    CALL(2)			/* jump 2 instructions ahead */
    CMP(Safety,ZERO)
    BG(FLAB(mth_initlab1))
    ADD(RET,imm(32),CP)

    ST_RELOC(H,tmp1,imm(lo10(&mth_stk)),
			 RELOC_GVAR_LO10,symidx_mth_stk);

    ADD(OldE,imm(4),H)
    BA(FLAB(mth_initlab2))
    ADD(S,MDISP(dcmp_base),S)

    if (regmask) {
	int reg;
	for (reg=1; reg<32; reg++)
	    if (regmask & (1<<reg)) {
		offset -= 4;
		ST(reg,SP,imm(offset))
	    }
    }
    ADD(SP,imm(offset-16),SP)
    MOVE(UArg1,A1)
    MOVE(UArg2,A2)
    JMPL(tmp1,ZERO,RET)
    MOVE(E,OldE)

}

#ifdef PACKAGE

coff_init_mth_init1()
{
  INSERT_SYM(symidx_mth_base, 		"_mth_base");
  INSERT_SYM(symidx_mth_stk, 			"_mth_stk");
  INSERT_SYM(symidx_mth_bot, 			"_mth_bot");
}

#endif /* PACKAGE */

extern	void	mth_aftercall	PARAMS(( void ));

void
ic_mth_init2(regmask,sadj,y,z)
    long regmask;
    long sadj;
    long y, z;
{
    Code delay_instr;
    SUB(E,imm(8),SP)
    if (regmask) {
	int reg;
	int offset=0;
	for (reg=1; reg<32; reg++)
	    if (regmask & (1<<reg)) {
		offset -= 4;
		LD(SP,imm(offset),reg)
	    }
    }
    delay_instr = *--ic_ptr;
    CALL_RELOC(BLAB(mth_aftercall),
			   RELOC_GVAR_WDISP30,symidx_mth_aftercall);
    ic_put(delay_instr);

    FLABDCL(mth_initlab1)
    FLABDCL(mth_initlab2)
    
    stacksize = 0;
    stackmax = 0;
}

#ifdef PACKAGE

coff_init_mth_init2()
{
  INSERT_SYM(symidx_mth_aftercall, 	"_mth_aftercall");
}

#endif /* PACKAGE */

void
ic_mth_fin(x,y,z,w)
    long x,y,z,w;
{
    *mth_stackadj = iADD(H,imm(40+stackmax*4),OldE);
}


extern	void	mth_eq	PARAMS(( void ));

void
ic_mth_eq(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_eq),RET)
    NOP

    stacksize -= 2;
}


extern	void	mth_lt	PARAMS(( void ));

void
ic_mth_lt(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_lt),RET)
    NOP
    stacksize -= 2;
}


extern	void	mth_gt	PARAMS(( void ));

void
ic_mth_gt(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_gt),RET)
    NOP
    stacksize -= 2;
}


extern	void	mth_le	PARAMS(( void ));

void
ic_mth_le(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_le),RET)
    NOP
    stacksize -= 2;
}


extern	void	mth_ge	PARAMS(( void ));

void
ic_mth_ge(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_ge),RET)
    NOP
    stacksize -= 2;
}


extern	void	mth_ne	PARAMS(( void ));

void
ic_mth_ne(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_ne),RET)
    NOP
    stacksize -= 2;
}


extern	void	mth_putnum	PARAMS(( void ));

void
ic_mth_getval(base,disp,z,w)
    long base, disp;
    long z, w;
{
    int reg;
    Code delay_instr;



    JMPL(S,MDISP(mth_putnum),RET)
    NOP

#ifdef notdef
    ADD(OldE,imm(8),OldE)		/* pop stack in delay slot */
#endif

    if (UArg2 != (reg = ic_get_operand(base,disp,UArg2))) {
	MOVE(reg, UArg2)
    }
    delay_instr = *--ic_ptr;
    CALL(BLAB(wm_unify))
    RELOC_INFO(RELOC_GVAR_WDISP30,(ic_ptr-1),symidx_wm_unify)
    ic_put(delay_instr);

    stacksize -= 1;
}


extern	void	mth_getnum	PARAMS(( void ));
extern	void	mth_getnum0	PARAMS(( void ));

void
ic_mth_getnum(base,disp,z,w)
    long base, disp;
    long z, w;
{
    int reg;
    Code delay_instr;
    if (stacksize == 0) {
	if (UArg1 != (reg = ic_get_operand(base,disp,UArg1))) {
	    MOVE(reg, UArg1)
	}

	delay_instr = *--ic_ptr;

	JMPL(S,MDISP(mth_getnum0),RET)
    }
    else {
	if (UArg2 != (reg = ic_get_operand(base,disp,UArg2))) {
	    MOVE(reg, UArg2)
	}

	delay_instr = *--ic_ptr;

	JMPL(S,MDISP(mth_getnum),RET)
    }

    ic_put(delay_instr);

    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


void
ic_mth_putnum(base,disp,z,w)
    long base, disp;
    long z, w;
{
    JMPL(S,MDISP(mth_putnum),RET)
    NOP

#ifdef notdef
    ADD(OldE,imm(8),OldE)		/* pop stack in delay slot */
#endif
    ic_put_operand(base,disp,UArg1);

    stacksize -= 1;
}


extern	void	mth_push	PARAMS(( void ));
extern	void	mth_pushdbl0	PARAMS(( void ));

void
ic_mth_pushdbl(d0,d1,z,w)
    long d0;
    long d1;
    long z, w;
{
    if (stacksize != 0) {
	JMPL(S,MDISP(mth_push),RET)
	SUB(OldE,imm(8),OldE)		/* perform part of push in delay slot */
    }
    CALL_RELOC(BLAB(mth_pushdbl0),
			   RELOC_GVAR_WDISP30,symidx_mth_pushdbl0);
    ADD(RET,imm(8),RET)			/* skip to start of doubles in delay */
					/* slot		*/
    ic_put(d0);				/* put down the pieces of the double */
    ic_put(d1);
    stacksize += 1;			/* stack is now bigger */
    if (stackmax < stacksize)
	stackmax = stacksize;		/* adjust maximum stack size */
}

#ifdef PACKAGE

coff_init_mth_pushdbl()
{
  INSERT_SYM(symidx_mth_pushdbl0, 	"_mth_pushdbl0");
}

#endif /* PACKAGE */

extern	void	mth_pushint	PARAMS(( void ));
extern	void	mth_pushint0	PARAMS(( void ));

void
ic_mth_pushint(val,y,z,w)
    long val;
    long y, z, w;
{
    Code delay_instr;
    if (stacksize == 0) {
	move_const(val,UArg1);
	delay_instr = *--ic_ptr;
	JMPL(S,MDISP(mth_pushint0),RET)
    }
    else {
	move_const(val,UArg2);
	delay_instr = *--ic_ptr;
	JMPL(S,MDISP(mth_pushint),RET)
    }

    ic_put(delay_instr);
    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;

}


extern	void	mth_add	PARAMS(( void ));
void
ic_mth_add(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_add),RET)
    ADD(OldE,imm(8),OldE)		/* pop stack in delay slot */

    stacksize -= 1;
}


extern	void	mth_addi	PARAMS(( void ));
void
ic_mth_addi(i,x,y,z)
    long i;
    long x, y, z;
{
    Code delay_instr;
    move_const(i,UArg2);
    delay_instr = *--ic_ptr;
    JMPL(S,MDISP(mth_addi),RET)
    ic_put(delay_instr);
}


extern	void	mth_sub	PARAMS(( void ));
void
ic_mth_sub(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_sub),RET)
    ADD(OldE,imm(8),OldE)		/* pop stack in delay slot */

    stacksize -= 1;
}


extern	void	mth_subi	PARAMS(( void ));
void
ic_mth_subi(i,x,y,z)
    long i;
    long x, y, z;
{
    Code delay_instr;
    move_const(i,UArg2);
    delay_instr = *--ic_ptr;
    JMPL(S,MDISP(mth_subi),RET)
    ic_put(delay_instr);
}


extern	void	mth_mul	PARAMS(( void ));
void
ic_mth_mul(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_mul),RET)
    ADD(OldE,imm(8),OldE)		/* pop stack in delay slot */

    stacksize -= 1;
}


extern	void	mth_div	PARAMS(( void ));
void
ic_mth_div(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_div),RET)
    ADD(OldE,imm(8),OldE)		/* pop stack in delay slot */

    stacksize -= 1;
}


extern	void	mth_fdiv	PARAMS(( void ));
void
ic_mth_fdiv(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_fdiv),RET)
    ADD(OldE,imm(8),OldE)		/* pop stack in delay slot */

    stacksize -= 1;
}


extern	void	mth_mod	PARAMS(( void ));
void
ic_mth_mod(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_mod),RET)
    ADD(OldE,imm(8),OldE)		/* pop stack in delay slot */

    stacksize -= 1;
}


extern	void	mth_neg	PARAMS(( void ));
void
ic_mth_neg(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_neg),RET)
    SUBcc(ZERO,UArg1,UArg2)		/* do part of int case in delay slot */
}


extern	void	mth_band	PARAMS(( void ));
void
ic_mth_band(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_band),RET)
    ADD(OldE,imm(8),OldE)		/* pop stack in delay slot */

    stacksize -= 1;
}


extern	void	mth_bor	PARAMS(( void ));
void
ic_mth_bor(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_bor),RET)
    ADD(OldE,imm(8),OldE)

    stacksize -= 1;
}


extern	void	mth_bxor	PARAMS(( void ));
void
ic_mth_bxor(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_bxor),RET)
    ADD(OldE,imm(8),OldE)

    stacksize -= 1;
}


extern	void	mth_not	PARAMS(( void ));
void
ic_mth_not(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_not),RET)
    NOP
}


extern	void	mth_lshft	PARAMS(( void ));
void
ic_mth_lshft(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_lshft),RET)
    ADD(OldE,imm(8),OldE)

    stacksize -= 1;
}


extern	void	mth_rshft	PARAMS(( void ));
void
ic_mth_rshft(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_rshft),RET)
    ADD(OldE,imm(8),OldE)

    stacksize -= 1;
}


extern	void	mth_power	PARAMS(( void ));
void
ic_mth_power(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_power),RET)
    ADD(OldE,imm(8),OldE)

    stacksize -= 1;
}


extern	void	mth_abs	PARAMS(( void ));
void
ic_mth_abs(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_abs),RET)
    CMP(UArg1,ZERO)			/* handle part of integer case in */
					/* delay slot */
}


extern	void	mth_sin	PARAMS(( void ));
void
ic_mth_sin(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_sin),RET)
    NOP
}


extern	void	mth_cos	PARAMS(( void ));
void
ic_mth_cos(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_cos),RET)
    NOP
}


extern	void	mth_tan	PARAMS(( void ));
void
ic_mth_tan(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_tan),RET)
    NOP
}


extern	void	mth_asin	PARAMS(( void ));
void
ic_mth_asin(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_asin),RET)
    NOP
}


extern	void	mth_acos	PARAMS(( void ));
void
ic_mth_acos(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_acos),RET)
    NOP
}


extern	void	mth_atan	PARAMS(( void ));
void
ic_mth_atan(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_atan),RET)
    NOP
}


extern	void	mth_sqrt	PARAMS(( void ));
void
ic_mth_sqrt(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_sqrt),RET)
    NOP
}


extern	void	mth_exp	PARAMS(( void ));
void
ic_mth_exp(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_exp),RET)
    NOP
}


extern	void	mth_exp10	PARAMS(( void ));
void
ic_mth_exp10(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_exp10),RET)
    NOP
}


extern	void	mth_log	PARAMS(( void ));
void
ic_mth_log(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_log),RET)
    NOP
}


extern	void	mth_log10	PARAMS(( void ));
void
ic_mth_log10(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_log10),RET)
    NOP
}


extern	void	mth_floor	PARAMS(( void ));
void
ic_mth_floor(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_floor),RET)
    NOP
}


extern	void	mth_round	PARAMS(( void ));
void
ic_mth_round(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_round),RET)
    NOP
}


extern	void	mth_trunc	PARAMS(( void ));
void
ic_mth_trunc(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_trunc),RET)
    NOP
}


extern	void	mth_heapused	PARAMS(( void ));
void
ic_mth_heapused(x,y,z,w)
    long x, y, z, w;
{
    if (stacksize != 0) {
	JMPL(S,MDISP(mth_push),RET)
	SUB(OldE,imm(8),OldE)		/* perform part of push in delay slot */
    }
    JMPL(S,MDISP(mth_heapused),RET)
    NOP
    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


extern	void	mth_cputime	PARAMS(( void ));
void
ic_mth_cputime(x,y,z,w)
    long x, y, z, w;
{
    if (stacksize != 0) {
	JMPL(S,MDISP(mth_push),RET)
	SUB(OldE,imm(8),OldE)		/* perform part of push in delay slot */
    }
    JMPL(S,MDISP(mth_cputime),RET)
    NOP
    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


extern	void	mth_realtime	PARAMS(( void ));
void
ic_mth_realtime(x,y,z,w)
    long x, y, z, w;
{
    if (stacksize != 0) {
	JMPL(S,MDISP(mth_push),RET)
	SUB(OldE,imm(8),OldE)		/* perform part of push in delay slot */
    }
    JMPL(S,MDISP(mth_realtime),RET)
    NOP
    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


extern	void	mth_random	PARAMS(( void ));
void
ic_mth_random(x,y,z,w)
    long x, y, z, w;
{
    if (stacksize != 0) {
	JMPL(S,MDISP(mth_push),RET)
	SUB(OldE,imm(8),OldE)		/* perform part of push in delay slot */
    }
    JMPL(S,MDISP(mth_random),RET)
    NOP
    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


extern	void	mth_callout_init	PARAMS(( void ));
void
ic_mth_callout_init(x,y,z,w)
    long x, y, z, w;
{
    if (stacksize != 0) {
	JMPL(S,MDISP(mth_push),RET)
	SUB(OldE,imm(8),OldE)
    }

    JMPL(S,MDISP(mth_callout_init),RET)
    NOP

    stacksize+=1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


extern	void	mth_callout	PARAMS(( void ));
void
ic_mth_callout(x,y,z,w)
    long x, y, z, w;
{
    LD(SP,imm(0),UArg1)
    JMPL(S,MDISP(mth_callout),RET)
    ADD(SP,imm(4),SP)
    /* previous line added by Kev (5-20-93) so stack is popped */
}

/*
 * The following was added by Kev on 5-18-93
 */

extern	void	mth_atan2	PARAMS(( void ));
void
ic_mth_atan2(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_atan2),RET)
    ADD(OldE,imm(8),OldE)

    stacksize -= 1;
}

extern	void	mth_fmod	PARAMS(( void ));
void
ic_mth_fmod(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_fmod),RET)
    ADD(OldE,imm(8),OldE)

    stacksize -= 1;
}

extern	void	mth_hypot	PARAMS(( void ));
void
ic_mth_hypot(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_hypot),RET)
    ADD(OldE,imm(8),OldE)

    stacksize -= 1;
}

extern	void	mth_jn	PARAMS(( void ));
void
ic_mth_jn(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_jn),RET)
    ADD(OldE,imm(8),OldE)

    stacksize -= 1;
}

extern	void	mth_yn	PARAMS(( void ));
void
ic_mth_yn(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_yn),RET)
    ADD(OldE,imm(8),OldE)

    stacksize -= 1;
}

extern	void	mth_ceil	PARAMS(( void ));
void
ic_mth_ceil(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_ceil),RET)
    NOP
}

extern	void	mth_cosh	PARAMS(( void ));
void
ic_mth_cosh(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_cosh),RET)
    NOP
}

extern	void	mth_erf	PARAMS(( void ));
void
ic_mth_erf(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_erf),RET)
    NOP
}

extern	void	mth_erfc	PARAMS(( void ));
void
ic_mth_erfc(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_erfc),RET)
    NOP
}

extern	void	mth_gamma	PARAMS(( void ));
void
ic_mth_gamma(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_gamma),RET)
    NOP
}

extern	void	mth_j0	PARAMS(( void ));
void
ic_mth_j0(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_j0),RET)
    NOP
}

extern	void	mth_j1	PARAMS(( void ));
void
ic_mth_j1(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_j1),RET)
    NOP
}

extern	void	mth_sinh	PARAMS(( void ));
void
ic_mth_sinh(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_sinh),RET)
    NOP
}

extern	void	mth_tanh	PARAMS(( void ));
void
ic_mth_tanh(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_tanh),RET)
    NOP
}

extern	void	mth_y0	PARAMS(( void ));
void
ic_mth_y0(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_y0),RET)
    NOP
}

extern	void	mth_y1	PARAMS(( void ));
void
ic_mth_y1(x,y,z,w)
    long x, y, z, w;
{
    JMPL(S,MDISP(mth_y1),RET)
    NOP
}

#ifdef PACKAGE

void
coff_init_math()
{
  coff_init_mth_init1();
  coff_init_mth_init2();
  coff_init_mth_pushdbl();
}

#endif /* PACKAGE */
