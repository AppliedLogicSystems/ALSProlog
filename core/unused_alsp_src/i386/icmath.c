/*
 * icmath.c	-- stuff to emit math instructions
 *
 *	Copyright (c) 1987-1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 3/23/87
 * Revision History:
 *	Revised: 01/11/89, 	KMH, 	--	Modified for the 386 system
 *	Revised: 09/01/90, 	Ilyas, 	-- 	Completed for the 386 system
 *
 * Packaging notes :
 *		the extern wm_b_reg that is referenced by code generated
 *		in this file has already been loaded into coff symbol
 *		table in icode1.c
 */


#include <stdio.h>

#include "config.h"
#include "types.h"
#include "alloc.h"
#include "parser.h"
#include "compile.h"
#include "mtypes.h"
#include "wintcode.h"
#include "machinst.h"
#include "icodegen.h"
#include "machreg.h"
#include "wamregs.h"
#include "fmath.h"
#include "fatal.h"
#include "rinfo.h"

#ifdef InMath

int IsFloat;

#if NTEMPS > 0
static int ntemps;
static int tempmap[NTEMPS];
#endif

static int toast = -1;		/* Top Of Arithmetic STack */


#define COMPUTE_TOAST(reg,disp) {							\
	reg = H_REG;											\
	disp = (toast+1)*(sizeof(double)+sizeof(short));		\
}


#define InMathOptimize 1

#ifdef InMathOptimize
int inmath_optimize;
int inmath_optimize_imm;
#endif


ic_mth_init(map,x,y,z)
	int map;
	int x,y,z;
{
	int i;

	IsFloat = 0;
	/* Only bother is we have any temporary registers */
#if NTEMPS > 0
	for (ntemps=0,i=0; i<=NTEMPS; i++)
		if (map&(1<<i)) 
			tempmap[ntemps++]=i;
#endif
	toast = -1;
}



/*
 * Procedure:	ic_mth_getval
 * Function:	 emits code appropriate for the left hand side of an is
 * Parameters:  
 *		base	 -- index of base register
 *		disp	 -- displacement from base register
 */
ic_mth_getval(base,disp)
	int base;
	int disp;
{
	CodePtr dpat, dpat1;
	int treg, tdisp;
	CodePtr l1,l2,l3,l4;

	/* Put the result into register EAX */
#ifdef InMathOptimize
	if (inmath_optimize) {
		/*
		 * We will need the register EBX, 
		 * store the result in arithmetic stack 
		 */
		toast++;
		COMPUTE_TOAST(treg,tdisp)
		MOVMR(treg,tdisp,EBX)
		MOVRR(EAX,EBX)
	}
	else {
		COMPUTE_TOAST(treg,tdisp)
		MOVRM(EAX,treg,tdisp)
	}
#else
	COMPUTE_TOAST(treg,tdisp)
	MOVRM(EAX,treg,tdisp)
#endif

	/* Check the result whether it can be a prolog integer or not */
	/* If result < MINPROLOGINT, stop to execute inline code */
	CMPRI(EAX,MINPROLOGINT)
	if ((BLDISP(ic_macropatch1)+2) >= -128) 
		JL(BDISP(ic_macropatch1))
	else
		JLL(BLDISP(ic_macropatch1))
	/* If result > MAXPROLOGINT, stop to execute inline code */
	CMPRI(EAX,MAXPROLOGINT)
	if ((BLDISP(ic_macropatch1)+2) >= -128) 
		JG(BDISP(ic_macropatch1))
	else
		JGL(BLDISP(ic_macropatch1))

	/* Get the item which will be unified with the result */
	MOVRM(EAX,ic_base_nums[base],sizeof(PWord)*disp)
	ic_deref1(1,&dpat, &dpat1);

	/* It is a variable. Store the result in the variable */
	MOVRM(EAX,treg,tdisp)
	SHLR(EAX,MTP_CONSTSHIFT)
	ORRI(EAX,MTP_INT)
	MOVMR(EBX,0,EAX)

	/* See if we must trail */
	CMPRR(HB_REG,EBX)
	JB(0) 
	LABEL(l1) 

	CMPRR(SPB_REG,EBX)
	JA(0)
	LABEL(l2)

	LEA(TR_REG,TR_REG,-sizeof(PWord))	/* Must trail */
	MOVMR(TR_REG,0,EBX)
	JMP(0)
	LABEL(l3)

	/* It is not a variable, compore it with the result */
	ic_drf1fix(dpat,CURRENTPOSITION,dpat1);

	/* Tag TOAST as an int */
	MOVRM(EAX,treg,tdisp)
	SHLR(EAX,MTP_CONSTSHIFT)
	ORRI(EAX,MTP_INT)

	CMPRR(EAX,EBX)
	JE(0)
	LABEL(l4)
	FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)

	/* Where to go if we can continue */
	PATCHDISP(l1)
	PATCHDISP(l2)
	PATCHLDISP(l3)
	PATCHDISP(l4)
}



/*
 * Procedure:	ic_mth_putint
 * Function :	emits code appropriate for the left hand side of an 'is'
 *				when the left hand side is a variable an this is its
 *				first occurance.
 */ 
ic_mth_putint(base,disp)
	int base;
	int disp;
{
	int treg, tdisp;

	/* Figure out where to get TOAST from */
	COMPUTE_TOAST(treg,tdisp)
#ifdef InMathOptimize
	if (inmath_optimize)
		MOVRR(EAX,EBX)
	else
		MOVRM(EAX,treg,tdisp)
#else
	MOVRM(EAX,treg,tdisp)
#endif

	/* Check the result whether it can be a prolog integer or not 	*/
	/* If result < MINPROLOGINT, stop to execute inline code 		*/
	CMPRI(EAX,MINPROLOGINT)
	if ((BLDISP(ic_macropatch1)+2) >= -128) 
		JL(BDISP(ic_macropatch1)) 
	else
		JLL(BLDISP(ic_macropatch1))
	/* If result > MAXPROLOGINT, stop to execute inline code */
	CMPRI(EAX,MAXPROLOGINT)
	if ((BLDISP(ic_macropatch1)+2) >= -128) 
		JG(BDISP(ic_macropatch1)) 
	else
		JGL(BLDISP(ic_macropatch1))

	/* Get the item which will be unified with the result */
	LEA(EBX,ic_base_nums[base],sizeof(PWord)*disp)

	/* Store the result in the variable */
	SHLR(EAX,MTP_CONSTSHIFT)
	ORRI(EAX,MTP_INT)
	MOVMR(EBX,0,EAX)
}



/*
 * Instruction: ic_mth_getint
 * Function:	Dereferences the effective address specified by sbase and sdisp
 *		leaving the result in A0.  If the result is a variable, failure
 *		occurs.  If the result is ground, but not an integer, the afp
 *		is jumped to.
 * Parameters:	sbase	-- base register
 *		sdisp	-- displacement
 */
ic_mth_getint(sbase,sdisp,y,z)
	int sbase;
	int	sdisp;
	int y,z;
{
	int sreg, smode;
	int dpat,dpat1;
	int reg, disp;
	CodePtr l1;

	/* Get variable and dereference it */
	MOVRM(EAX,ic_base_nums[sbase],sizeof(PWord)*sdisp)
	ic_deref1(1,&dpat, &dpat1);

	/* It is a variable. */
	FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)

	/* It is not a variable */
	ic_drf1fix(dpat,CURRENTPOSITION,dpat1);

	/* Check whether it is an integer or not */
	MOVRR(EAX,EBX)
	ANDRI(EAX,MTP_CONSTMASK)
	CMPRI(EAX,MTP_INT)
	JE(0)
	LABEL(l1)

	/* It is not an integer, jump to emulator code */
	JMP(BLDISP(ic_macropatch1))

	/* It is an integer, push it into the arithmatic stack */
	PATCHDISP(l1)
	SARR(EBX,MTP_CONSTSHIFT)
#ifdef InMathOptimize
	if (!(inmath_optimize)) {
		toast++;
		COMPUTE_TOAST(reg,disp)
		MOVMR(reg,disp,EBX)
	}
#else
	toast++;
	COMPUTE_TOAST(reg,disp)
	MOVMR(reg,disp,EBX)
#endif
}



/*
 * ic_pop deletes an item from the arithmetic stack
 */
ic_pop()
{
	toast--;
}



/*
 * If there is an overflow during an arithmetic operation,
 * stop to execute inline code.
 */
ic_mth_overflow_check()
{
	if ((BLDISP(ic_macropatch1)+2) >= -128) 
		JO(BDISP(ic_macropatch1))
	else
		JOL(BLDISP(ic_macropatch1))
}




/*
 * Instruction: ic_mth_pushint
 * Function: Pushs a constant onto the arithmetic evaluation stack
 */
ic_mth_pushint(i)
	int i;
{
	int reg, disp;

#ifdef InMathOptimize
	if (inmath_optimize)
		inmath_optimize_imm = i;
	else {
		toast++;
		COMPUTE_TOAST(reg,disp)
		MOVMI(reg,disp,i)
	}
#else
	/* Calculate where item will go */
	toast++;
	COMPUTE_TOAST(reg,disp)

	/* And put it there */
	MOVMI(reg,disp,i)
#endif
}



ic_mth_add()
{
	int sreg, sdisp;
	int dreg, ddisp;

#ifdef InMathOptimize
	if (inmath_optimize)
		ADDRI(EBX,inmath_optimize_imm)
	else {
		COMPUTE_TOAST(sreg,sdisp)
		MOVRM(EAX,sreg,sdisp)
		ic_pop();
		COMPUTE_TOAST(dreg,ddisp)
		ADDMR(dreg,ddisp,EAX)
	}
	ic_mth_overflow_check();
#else
	/* Pop the second operand */
	COMPUTE_TOAST(sreg,sdisp)
	MOVRM(EAX,sreg,sdisp)
	ic_pop();

	/* Add the second operand to the first operand */
	COMPUTE_TOAST(dreg,ddisp)
	ADDMR(dreg,ddisp,EAX)
	
	/* Check the overflow */
	ic_mth_overflow_check();
#endif
}



ic_mth_sub()
{
	int sreg, sdisp;
	int dreg, ddisp;

#ifdef InMathOptimize
	if (inmath_optimize)
		SUBRI(EBX,inmath_optimize_imm)
	else {
		COMPUTE_TOAST(sreg,sdisp)
		MOVRM(EAX,sreg,sdisp)
		ic_pop();
		COMPUTE_TOAST(dreg,ddisp)
		SUBMR(dreg,ddisp,EAX)
	}
	ic_mth_overflow_check();
#else
	/* Pop the second operand */
	COMPUTE_TOAST(sreg,sdisp)
	MOVRM(EAX,sreg,sdisp)
	ic_pop();

	/* Subtract the second operand from the first operand */
	COMPUTE_TOAST(dreg,ddisp)
	SUBMR(dreg,ddisp,EAX)
	
	/* Check the overflow */
	ic_mth_overflow_check();
#endif
}



ic_mth_neg()
{
	int sreg, sdisp;

	/* Get the negation of the operand */
	COMPUTE_TOAST(sreg,sdisp)
	NEGM(sreg,sdisp)
	
	/* Check the overflow */
	ic_mth_overflow_check();
}


ic_mth_abs()
{
	int sreg, sdisp;
	CodePtr l1;

	/* Get the operand */
	COMPUTE_TOAST(sreg,sdisp)
	MOVRM(EAX,sreg,sdisp)

	/* Check whether it is a negative number or not */
	ANDRR(EAX,EAX)
	JA(0)
	LABEL(l1)

	/* It is a negative number, make it a postive number */
	NEGR(EAX)

	/* Check the overflow */
	ic_mth_overflow_check();

	/* Store the result */
	MOVMR(sreg,sdisp,EAX)

	/* Where to go when we continue */
	PATCHDISP(l1)
}



ic_mth_mul()
{
	int sreg, sdisp;
	int dreg, ddisp;

	/* Pop the second operand */
	COMPUTE_TOAST(sreg,sdisp)
	ic_pop();

	/* Multiply the first operand with the second operand */
	COMPUTE_TOAST(dreg,ddisp)
	MOVRM(EAX,dreg,ddisp)
	IMULRM(EAX,sreg,sdisp)

	/* Check the overflow */
	ic_mth_overflow_check();

	/* Store the result */
	MOVMR(dreg,ddisp,EAX)
}



ic_mth_div()
{
	int sreg, sdisp;
	int dreg, ddisp;
	CodePtr l1;

	/* Pop the second operand (divisor) */
	COMPUTE_TOAST(sreg,sdisp)
	MOVRM(EBX,sreg,sdisp)
	ic_pop();

	/* Check whether divisor is zero or not */
	CMPRI(EBX,0)
	JNE(0)
	LABEL(l1)
	FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)
	PATCHDISP(l1)

	/* Get the first operand (dividend) */
	COMPUTE_TOAST(dreg,ddisp)
	MOVRM(EAX,dreg,ddisp)

	/* Save EDX register */
	PUSHR(EDX)
	MOVRI(EDX,0) 

	/* Divide dividend by divisor  */
	IDIVEAXR(EBX) 
	
	/* Restore EDX */
	POPR(EDX)

	/* Save Quotient */
	MOVMR(dreg,ddisp,EAX)
}



ic_mth_mod()
{
	int sreg, sdisp;
	int dreg, ddisp;
	CodePtr l1;

	/* Pop the second operand (divisor) */
	COMPUTE_TOAST(sreg,sdisp)
	MOVRM(EBX,sreg,sdisp)
	ic_pop();

	/* Check whether divisor is zero or not */
	CMPRI(EBX,0)
	JNE(0)
	LABEL(l1)
	FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)
	PATCHDISP(l1)

	/* Get the first operand (dividend) */
	COMPUTE_TOAST(dreg,ddisp)
	MOVRM(EAX,dreg,ddisp)

	/* Save EDX register */
	PUSHR(EDX)
	MOVRI(EDX,0)

	/* Divide dividend by divisor  */
	IDIVEAXR(EBX) 

	/* Move Remainder into register into EAX */
	MOVRR(EAX,EDX) 
	
	/* Restore EDX */
	POPR(EDX)

	/* Save Remainder */
	MOVMR(dreg,ddisp,EAX)
}



ic_mth_band()
{
	int sreg, sdisp;
	int dreg, ddisp;

	/* Pop the second operand */
	COMPUTE_TOAST(sreg,sdisp)
	MOVRM(EAX,sreg,sdisp)
	ic_pop();

	/* Do 'and' operation */
	COMPUTE_TOAST(dreg,ddisp)
	ANDMR(dreg,ddisp,EAX)
}


ic_mth_bor()
{
	int sreg, sdisp;
	int dreg, ddisp;

	/* Pop the second operand */
	COMPUTE_TOAST(sreg,sdisp)
	MOVRM(EAX,sreg,sdisp)
	ic_pop();

	/* Do 'or' operation */
	COMPUTE_TOAST(dreg,ddisp)
	ORMR(dreg,ddisp,EAX)
}


ic_mth_bxor()
{
	int sreg, sdisp;
	int dreg, ddisp;

	/* Pop the second operand */
	COMPUTE_TOAST(sreg,sdisp)
	MOVRM(EAX,sreg,sdisp)
	ic_pop();

	/* Do 'xor' operation */
	COMPUTE_TOAST(dreg,ddisp)
	XORMR(dreg,ddisp,EAX)
}


ic_mth_lshft()
{
	int sreg, sdisp;
	int dreg, ddisp;
	CodePtr l1;

	/* Save ECX register */
	PUSHR(ECX)

	/* Pop the second operand */
	COMPUTE_TOAST(sreg,sdisp)
	MOVRM(ECX,sreg,sdisp)
	ic_pop();

	/* Check the second operand, if it is greater than 255
	   assume that it is 255 */
	CMPRI(ECX,255)
	JB(0)
	LABEL(l1)
	MOVRI(ECX,255)
	PATCHDISP(l1)

	/* Do 'left shift' operation */
	COMPUTE_TOAST(dreg,ddisp)
	SHLCLM(dreg,ddisp)

	/* Restore ECX */
	POPR(ECX)
}


ic_mth_rshft()
{
	int sreg, sdisp;
	int dreg, ddisp;
	CodePtr l1;

	/* Save ECX register */
	PUSHR(ECX)

	/* Pop the second operand */
	COMPUTE_TOAST(sreg,sdisp)
	MOVRM(ECX,sreg,sdisp)
	ic_pop();

	/* Check the second operand, if it is greater than 255
	   assume that it is 255 */
	CMPRI(ECX,255)
	JB(0)
	LABEL(l1)
	MOVRI(ECX,255)
	PATCHDISP(l1)

	/* Do 'right shift' operation */
	COMPUTE_TOAST(dreg,ddisp)
	SHRCLM(dreg,ddisp)

	/* Restore ECX */
	POPR(ECX)
}


ic_mth_not()
{
	int sreg, sdisp;

	/* Get the operand */
	COMPUTE_TOAST(sreg,sdisp)

	/* Do 'not' operation */
	NOTM(sreg,sdisp)
}



ic_mth_cmp()
{
	int sreg, sdisp;
	int dreg, ddisp;

	/* Pop the second operand */
	COMPUTE_TOAST(sreg,sdisp)
	MOVRM(EAX,sreg,sdisp)
	ic_pop();

	/* Compare it with the second operand */
	COMPUTE_TOAST(dreg,ddisp)
	CMPMR(dreg,ddisp,EAX)
}


ic_mth_eq()
{
	CodePtr l1;
	ic_mth_cmp();
	JE(0)
	LABEL(l1)
	FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)
	PATCHDISP(l1)
}

ic_mth_ne()
{
	CodePtr l1;
	ic_mth_cmp();
	JNE(0)
	LABEL(l1)
	FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)
	PATCHDISP(l1)
}

ic_mth_lt()
{
	CodePtr l1;
	ic_mth_cmp();
	JL(0)
	LABEL(l1)
	FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)
	PATCHDISP(l1)
}

ic_mth_le()
{
	CodePtr l1;
	ic_mth_cmp();
	JLE(0)
	LABEL(l1)
	FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)
	PATCHDISP(l1)
}

ic_mth_gt()
{
	CodePtr l1;
	ic_mth_cmp();
	JG(0)
	LABEL(l1)
	FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)
	PATCHDISP(l1)
}

ic_mth_ge()
{
	CodePtr l1;
	ic_mth_cmp();
	JGE(0)
	LABEL(l1)
	FAIL_RELOC(RELOC_GVAR,symidx_wm_b_reg)
	PATCHDISP(l1)
}

#endif /* InMath */
