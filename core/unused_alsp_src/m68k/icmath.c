/*===================================================================*
 |			icmath.c
 |	Copyright (c) 1992 Applied Logic Systems, Inc.
 |	Copyright (c) 1992 Motorola Inc.
 |
 |			-- stuff to emit math instructions
 |
 | Author:	Kevin Buettner, 
 | Creation:	2/18/92
 | Revision History:
 | 		Motorola 68k port by Scott Medeiros
 | 11/30/94,	C. Houpt	-- Parametrized math stack pointer
 |					register so different platforms can use
 |					different registers.  The MacOS use the B
 |					register instead of the Fail register.
 *===================================================================*/

#include "defs.h"
#include "icom.h"
#include "icode.h"
#include "compile.h"
#include "wintcode.h"
#include "machinst.h"
#include "codegen.h"
#include "icmath.h"

#include <stdio.h>

/*-------------------------------------------------------------------*
 | These are the global symbols we need access to.  Thus, RELOC_INFO
 | is emitted around uses of these symbols (for packaging).
 *-------------------------------------------------------------------*/
extern long int_table;
extern long dcmp_table;
extern long mth_bot;
extern long mth_stk;
extern long mth_is_addr;

static Code *mth_stackadj;
static LAB mth_initlab1;
static LAB mth_initlab2;
static int stacksize;		/* number of elements in arithmetic stack */
static int stackmax;

#define ICODE(macro,str,func,obp) extern void func PARAMS(( long, long, long, long ));
#include "icodedef.h"

/*-----------------------------------------------------------------* 
 | MStack is the register that contains th math stack pointer.
 | On most machines the Fail register is temporarily used for 
 | the math stack pointer,
 | but on the Macintosh, the B register is used.
 *-----------------------------------------------------------------*/
#ifdef MacOS
#define MStack	B
#else
#define MStack	Fail
#endif

void ic_mth_init1(long isonlygoal, long regmask, long y, long z);
void
ic_mth_init1(isonlygoal,regmask,y,z)
    long isonlygoal;
    long regmask;
    long y, z;
{

		/*------------------------------------------------------* 
		 | First need to save off MStack, so that it can be 
		 | used as a MathSP 
		 *------------------------------------------------------*/
    MOVE(MStack,ADIRECT,0,SP,PREDECR,0);	/* move.l	MStack,	-(SP) */

		/*------------------------------------------------------------* 
		 | The next 2 instructions initialize the Math SP.  The 
		 | second instruction is overwritten by ic_mth_fin, afterwards, 
		 | when it is known how big the arithmetic stack needs to be.
		 *------------------------------------------------------------*/
    MOVE(H,ADIRECT,0,D0,DDIRECT,0);		/* move.l	H,     	d0 */
    ADDIL(0,D0,DDIRECT,0);				/* overwrite extension word after instr word */
    mth_stackadj = ic_ptr - 1;

    ANDIW(0xfff8,D0,DDIRECT,0);				/* 68k only allows this op on d-regs */
    MOVE(D0,DDIRECT,0,MStack,ADIRECT,0);	/* movea.l	d0,	MStack */

		/*---------------------------------------------------------------------* 
		 | The and doubleword aligns the arithmetic stack -- this assumes 
		 | that the stack has been allocated one (long)word more than 
		 | necessary, otherwise doubleword aligning would make the stack one
   		 | element too small.  This depends on the constant chosen in ic_mth_fin...
		 *---------------------------------------------------------------------*/
#ifdef notdef
    MOVI((int) &mth_bot,A0,ADIRECT,0);		/* movea.l	#mth_bot, a0    */
#endif
#ifdef PACKAGE
RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_mth_bot)
#endif
#ifdef notdef
    MOVE(D0,DDIRECT,0,A0,INDIRECT,0);		/* move.l	d0,	  (a0)  */
#endif

    MOVtoAbs((int) &mth_bot, D0, DDIRECT, 0);	/* move.l d0, mth_bot */

    MOVI(&int_table,S,ADIRECT,0);	/* movea.l	int_table, S	*/
#ifdef PACKAGE
RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_int_table)
#endif

/* Note: 68k PC points at instruction currently executing */
    LEA_PCREL(A0);
    ic_put(13*2);		/* get addr just past bra mth_initlab2 - 12 words away */
    MOVE(A0,ADIRECT,0,SP,PREDECR,0);		/* move.l	a0, -(SP) = 1 word */

/* The Prolog arg/env stack:

   +---------------------------+
   |  .                        |
      .
   	Old Stuff (env of 
      .            clause)
   |  .                        |
   +___________________________+
   |	Fail (or B on MacOS)   |
   +---------------------------+
   |	&Callout Code          |  <------ SP
   -----------------------------

   Note that before a callout is done, the two top stack elements are accessed; at this time,
   the arg/env stack is also used to callout...

***CAVEAT- the C stack and the Prolog arg/env stack are one and the same.
	   Right now, it looks like we can get away with storing "global" values off SP as 
	   above; we must always "know" what's been pushed over it so we can access these
	   values before doing a callout.  In any event, if we later find this method does
	   not work well, we can always expand the math stack area, and store "global" values
	   in a separate stack area (just above mth_bot)
*/
#ifdef notdef
    CMPI(0,OV,DDIRECT,0);		/* cmpi.l	#0,	OV = 3 words */
#endif
    TST(OV,DDIRECT,0);		/* tst.l	OV = 1 word */
    BGT(FLAB(mth_initlab1));		/* 1 word */
/* Note that the two instructions below should be replaced with a single
   	move.l H, mth_stk
*/
#ifdef notdef
    LEA(0,ABSADDR,&mth_stk,A0);		/* lea.l	_mth_stk, a0   = 3 words */
    MOVE(H,ADIRECT,0,A0,INDIRECT,0); /* move.l H, (a0) = 1 word */
#endif
    MOVtoAbs((int) &mth_stk, H,ADIRECT,0);	/* move.l  H, mth_stk = 3 words */
#ifdef PACKAGE

RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_mth_stk)
#endif
    MOVE(MStack,ADIRECT,0,H,ADIRECT,0);	/* 1 word */
    ADDQ(4,H,ADIRECT,0);	      		/* H := MStack + 4 = 1 word */
    MOVI(&dcmp_table,S,ADIRECT,0);	/* movea.l	dcmp_table, S	= 3 words */    
    BRA(FLAB(mth_initlab2));		/* 1 word */

    /* don't need to save off model regs - 68k doesn't use machine registers
       to hold An regs, and temps already saved before initial is/2 call
    */

    /* 68k implementation requires that the stack frame be filled in BEFORE
       procedure entry. Just fill in arguments, and do a jsr.  The jsr pushes
       the return address on the stack, and then entry code does a link to get
       E onto the stack.  Note that the exit code will branch past the 2 words
       of gcinfo after the jsr, tho' it ain't strictly necessary, cuz gcinfo
       is a harmless mov.w instruction.
     */

    /* Assume on entry: d1 = & LHS is/2  (A2)
       			d2 = & RHS is/2  (A1)
			a0 = & where to callout  (is/2 entry code, or relop entry code)
    */
    MOVE(D2,DDIRECT,0,SP,PREDECR,0);		/* move.l d2, -(SP)  = 1 word */
    MOVE(D1,DDIRECT,0,SP,PREDECR,0);		/* move.l d1, -(SP)  = 1 word */
#ifdef notdef
    LEA(0,ABSADDR,mth_is_addr,A0);			/* prepare to call mth_is_addr (3) */
#endif

    JSRI(A0,INDIRECT,0);				/* and call is/2 = 1 word */
#ifdef PACKAGE
RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_mth_is_addr)
#endif

}

/* 2 words gcinfo inbetween code segments emitted */

extern void * mth_aftercall(void);

void
ic_mth_init2(regmask,sadj,y,z)
    long regmask;
    long sadj;
    long y, z;
{

    MOVE(E,ADIRECT,0,SP,ADIRECT,0);	/* 1 word */
    SUBQ(8,SP,ADIRECT,0);		/* SP := E - 8 = 1 word */
#ifdef notdef
    BSRL(BLAB(mth_aftercall));		/* 3 words, cuz mth_aftercall is a long ways below
					   but make sure it's ALWAYS gonna be 3 by using BSRL */
#endif
    JMP((int) &mth_aftercall);
#ifdef PACKAGE
RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_mth_aftercall)
#endif

    FLABDCL(mth_initlab1)
    FLABDCL(mth_initlab2)
    
    stacksize = 0;
    stackmax = 0;
}

void
ic_mth_fin(x, y, z, w)
    long x, y, z, w;
{
/*
  How big need the arithmetic stack area be?  Well, as deep as the computation gets (stackmax) +
  whatever overhead is needed for stuff like callouts and storing double results.
  28 bytes = 7 (long)words on the stack frame, is the same # used on the Sparc, and seems to be 
  sufficient.
  By my calculations (??), the 7 words can be accounted for as follows:
  	7 words for callout:
		2 fences
		S, Top Offset
	2 words to store a double at the end of a computation
	1 word to allow for doubleword alignment of the arithmetic stack
*/

/* In the 68k implementation, mth_stackadj points to the just after the addi instruction word,
   where the (word) immediate data should be placed.
*/

    *mth_stackadj = 28+stackmax*4;
    /* For the 68k implementation, restore the Fail register to its value on entry 
       
       Fail
       &CalloutCode      <----- SP
    */

    MOVE(SP,DISPL,4,MStack,ADIRECT,0);	/* move.l  4(sp), MStack */
}

void
ic_mth_eq(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,EQ_INDEX*4);
    stacksize -= 2;
}

void
ic_mth_lt(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,LT_INDEX*4);
    stacksize -= 2;
}

void
ic_mth_gt(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,GT_INDEX*4);
    stacksize -= 2;
}

void
ic_mth_le(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,LE_INDEX*4);
    stacksize -= 2;
}

void
ic_mth_ge(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,GE_INDEX*4);
    stacksize -= 2;
}

void
ic_mth_ne(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,NE_INDEX*4);
    stacksize -= 2;
}

void
ic_mth_getval(base,disp,z,w)
    long base, disp;
    long z, w;
{
    int reg, mode;

    JSRI(S,MEMINDIRECT,PUTNUM_INDEX*4);

    COMPUTE_MODE(base,disp,reg,mode);
    MOVE(reg,mode,disp,A0,ADIRECT,0);

/* 68k convention: a0, d0 contain input arguments to wm_unify 
   By convention, putnum leaves its result in d1, so move it to d0.
*/

    MOVE(D1,DDIRECT,0,D0,DDIRECT,0);	/* move.l d1, d0 */

/* 68k: before calling the unifier, restore Fail.
   Notice that inside domath.68k, move.l 8(sp), Fail,  this is because there is a return
   address sitting on the stack in those failure situations.
*/

    MOVE(SP,DISPL,4,MStack,ADIRECT,0);	/* move.l 4(sp), MStack */
 
#ifdef notdef
    BSR(BLAB(wm_unify));
#endif
    JSR(wm_unify);
#ifdef PACKAGE
RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_wm_unify)
#endif

    stacksize -= 1;
}

void
ic_mth_getnum(base,disp,z,w)
    long base,disp;
    long z,w;
{
    int reg, mode;

    COMPUTE_MODE(base,disp,reg,mode);
    MOVE(reg,mode,disp,D1,DDIRECT,0);	/* --> d1 for call to getnum */

    JSRI(S,MEMINDIRECT,GETNUM_INDEX*4);

    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}

void
ic_mth_putnum(base,disp,z,w)
    long base, disp;
    long z, w;
{
    int reg, mode;
    
    JSRI(S,MEMINDIRECT,PUTNUM_INDEX*4);	/* leaves result in d1 */

    COMPUTE_MODE(base,disp,reg,mode);	/* get target */
    MOVE(D1,DDIRECT,0,reg,mode,disp);	/* target := D1  (result of putnum) */

    stacksize -= 1;	
}

extern void *  mth_pushdbl0(void);

void
ic_mth_pushdbl(d0,d1,z,w)
    long d0;
    long d1;
    long z, w;
{
#ifdef notdef
    BSR(BLAB(mth_pushdbl0));
#endif
    JSR((int) &mth_pushdbl0);
#ifdef PACKAGE
RELOC_INFO(RELOC_GVAR,(ic_ptr-(sizeof(long)/sizeof(Code))),symidx_mth_pushdbl0)
#endif
    ic_putl(d0);				/* put down the pieces of the double */
    ic_putl(d1);
    stacksize += 1;			/* stack is now bigger */
    if (stackmax < stacksize)
	stackmax = stacksize;		/* adjust maximum stack size */
}


void
ic_mth_pushint(val,y,z,w)
    long val;
    long y, z, w;
{

    MOVEIMMTOD(val,D1);
    JSRI(S,MEMINDIRECT,PUSHINT_INDEX*4);


    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;

}


void
ic_mth_add(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,ADD_INDEX*4);
    stacksize -= 1;
}


void
ic_mth_addi(i,x,y,z)
    long i;
    long x, y, z;
{
    MOVEIMMTOD(i,D0);
    JSRI(S,MEMINDIRECT,ADDI_INDEX*4);
}


void
ic_mth_sub(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,SUB_INDEX*4);
    stacksize -= 1;
}


void
ic_mth_subi(i,x,y,z)
    long i;
    long x, y, z;
{

    MOVEIMMTOD(i,D0);
    JSRI(S,MEMINDIRECT,SUBI_INDEX*4);
}


void
ic_mth_mul(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,MUL_INDEX*4);
    stacksize -= 1;
}


void
ic_mth_div(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,DIV_INDEX*4);
    stacksize -= 1;
}


void
ic_mth_fdiv(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,FDIV_INDEX*4);
    stacksize -= 1;
}


void
ic_mth_mod(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,MOD_INDEX*4);
    stacksize -= 1;
}


void
ic_mth_neg(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,NEG_INDEX*4);
}

void
ic_mth_band(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,BAND_INDEX*4);
    stacksize -= 1;
}


void
ic_mth_bor(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,BOR_INDEX*4);
    stacksize -= 1;
}


void
ic_mth_bxor(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,BXOR_INDEX*4);
    stacksize -= 1;
}


void
ic_mth_not(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,NOT_INDEX*4);
}


void
ic_mth_lshft(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,LSHFT_INDEX*4);
    stacksize -= 1;
}


void
ic_mth_rshft(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,RSHFT_INDEX*4);
    stacksize -= 1;
}


void
ic_mth_power(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,POWER_INDEX*4);
    stacksize -= 1;
}


void
ic_mth_abs(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,ABS_INDEX*4);
}

void
ic_mth_sin(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,SIN_INDEX*4);
}


void
ic_mth_cos(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,COS_INDEX*4);
}


void
ic_mth_tan(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,TAN_INDEX*4);
}


void
ic_mth_asin(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,ASIN_INDEX*4);
}


void
ic_mth_acos(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,ACOS_INDEX*4);
}


void
ic_mth_atan(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,ATAN_INDEX*4);
}


void
ic_mth_sqrt(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,SQRT_INDEX*4);
}


void
ic_mth_exp(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,EXP_INDEX*4);
}


void
ic_mth_exp10(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,EXP10_INDEX*4);
}


void
ic_mth_log(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,LOG_INDEX*4);
}


void
ic_mth_log10(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,LOG10_INDEX*4);
}


void
ic_mth_floor(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,FLOOR_INDEX*4);
}


void
ic_mth_round(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,ROUND_INDEX*4);
}


void
ic_mth_trunc(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,TRUNC_INDEX*4);
}


void
ic_mth_heapused(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,HEAPUSED_INDEX*4);
    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


void
ic_mth_cputime(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,CPUTIME_INDEX*4);
    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


void
ic_mth_realtime(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,REALTIME_INDEX*4);
    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


void
ic_mth_random(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,RANDOM_INDEX*4);
    stacksize += 1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}

void
ic_mth_callout_init(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,CALLOUT_INIT_INDEX*4);
    stacksize+=1;
    if (stackmax < stacksize)
	stackmax = stacksize;
}


void
ic_mth_callout(x,y,z,w)
    long x, y, z, w;
{
    /* rhs of is/2 sitting on TOS
       pop the rhs into d2
    */

#ifdef notdef    
    MOVE(SP,INDIRECT,0,D2,DDIRECT,0);	/* move.l (SP), d2 */
    ADDQ(4,SP,ADIRECT,0);			/* addq.l #4, sp */
#endif
    MOVE(SP,POSTINCR,0,D2,DDIRECT,0);	/* move.l (SP)+, d2 */
    JSRI(S,MEMINDIRECT,CALLOUT_INDEX*4);
}

/* 
 * The following were added by Kev, 5-18-93
 */

void
ic_mth_sinh(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,SINH_INDEX*4);
}

void
ic_mth_cosh(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,COSH_INDEX*4);
}

void
ic_mth_tanh(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,TANH_INDEX*4);
}

void
ic_mth_ceil(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,CEIL_INDEX*4);
}

void
ic_mth_erf(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,ERF_INDEX*4);
}

void
ic_mth_erfc(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,ERFC_INDEX*4);
}

void
ic_mth_gamma(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,GAMMA_INDEX*4);
}

void
ic_mth_j0(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,J0_INDEX*4);
}

void
ic_mth_j1(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,J1_INDEX*4);
}

void
ic_mth_y0(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,Y0_INDEX*4);
}

void
ic_mth_y1(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,Y1_INDEX*4);
}

void
ic_mth_atan2(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,ATAN2_INDEX*4);
    stacksize -= 1;
}

void
ic_mth_fmod(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,FMOD_INDEX*4);
    stacksize -= 1;
}

void
ic_mth_hypot(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,HYPOT_INDEX*4);
    stacksize -= 1;
}

void
ic_mth_jn(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,JN_INDEX*4);
    stacksize -= 1;
}

void
ic_mth_yn(x,y,z,w)
    long x, y, z, w;
{
    JSRI(S,MEMINDIRECT,YN_INDEX*4);
    stacksize -= 1;
}
