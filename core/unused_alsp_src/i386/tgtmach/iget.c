/*
 * icget.c		-- Emit get instructions
 *
 *Copyright (c) 1987 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 2/12/87
 * Revision History:
 *		Revised: 03/22/87,	Kev		-- 	icode.c split into icode1.c,
 *									   	icode2.c, and icode.h
 *		Revised: 01/14/88,	KMH,	-- 	icode*.c split into files
 *									   	by function. The files were too big.
 * 		Revised: 02/01/91, 	Ilyas, 	--	Relocation information added
 *
 *		Revised: mm/dd/yy,	Who		-- 	Reason
 *
 */

#include <stdio.h>

#include "config.h"
#include "types.h"
#include "coerce.h"
#include "alloc.h"
#include "parser.h"
#include "mtypes.h"
#include "wamregs.h"
#include "machinst.h"
#include "icodegen.h"
#include "rinfo.h"


extern CodePtr capturepatch1;
extern CodePtr capturepatch3;
				
extern long UnifyPtr;


/*
 * Instruction: ic_g_uia
 * Function:	Emits code for matching a uia in the head
 * Parameters:	
 * 		uiastr		-- string corresponding to uia
 *		base		-- index of base register
 *		disp		-- displacement from base register
 */
ic_g_uia(uiastr,base,disp,x)
	char *uiastr;
	int base,disp,x;
{
	int mode, reg;
	extern wm_g_uia();

	MOVRM(EAX,ic_base_nums[base],sizeof(PWord)*disp)
	CALL((CodePtr) wm_g_uia);

	RELOC_INFO(RELOC_GVAR,(ic_ptr-2-sizeof(long)),symidx_wm_g_uia);

	/* Lay down UIA after instruction */
	ic_uiastr(uiastr);
}


/* 
 * Instruction ic_g_sym
 * Function:	Emits code corresponding to the symbol part of Warren's
 *				get_constant instruction
 * Parameters:	
 * 		tokid	-- token index
 *		base	-- index of base register
 *		disp	-- displacement from base register
 */
ic_g_sym(tokid,base,disp,x)
	int tokid,base,disp,x;
{
	PWord con = MMK_SYM(tokid);

#ifdef AllUIAConsts
	CodePtr dpat, dpat1;
	CodePtr l1,l2,l3,l4;
 
	MOVRM(EAX,ic_base_nums[base],sizeof(PWord)*disp)
 
	ic_deref1(0,&dpat, &dpat1);	/* dereference instructions */

	/* If we get to here, we are a variable */

	MOVMI(EBX,0,con)	/* Bind Variable */ 

	CMPRR(HB_REG,EBX)	/* Do trail check */
	JB(0)
	LABEL(l1)

	CMPRR(SPB_REG,EBX)
	JA(0)
	LABEL(l2)

	LEA(TR_REG,TR_REG,-sizeof(PWord))	/* Must trail */
	MOVMR(TR_REG,0,EBX)
  	JMP(0)
	LABEL(l3) 

	if (!firstargprocessed)
		ic_1stargalloc();

	ic_drf1fix(dpat,CURRENTPOSITION,dpat1);

	/* Read mode section goes here */
	CMPRI(EBX,con)

	JE(0)
	LABEL(l4)
	FAIL

	RELOC_INFO(RELOC_GVAR,(ic_ptr-2-sizeof(long)),symidx_wm_b_reg)

	PATCHDISP(l1)
	PATCHDISP(l2)
	PATCHLDISP(l3)
	PATCHDISP(l4)
#else
	extern wm_g_sym();

	PUSHI(con)

	MOVRM(EAX,ic_base_nums[base],sizeof(PWord)*disp)

	CALL((CodePtr)wm_g_sym)

	RELOC_INFO(RELOC_GVAR,(ic_ptr-2-sizeof(long)),symidx_wm_g_sym);

	/* Get rid of the constant we pushed above */
	POPR(EBX)
#endif
}


/*
 * Instruction: ic_g_int
 * Function:	Emits code corresponding to the integer part of Warren's
 *				get_constant instruction
 * Parameters:	
 * 		i		-- integer to get
 *		base	-- index of base register
 *		disp	-- displacement from base register
 */
ic_g_int(i,base,disp,x)
	int i,base,disp,x;
{
	CodePtr dpat, dpat1;
	CodePtr l1,l2,l3,l4;
	PWord con = MMK_INT(i);
 
	MOVRM(EAX,ic_base_nums[base],sizeof(PWord)*disp)
 
	ic_deref1(0,&dpat,&dpat1);	/* dereference instructions */

	/* If we get to here, we are a variable */

	MOVMI(EBX,0,con)	/* Bind Variable */ 

	CMPRR(HB_REG,EBX)	/* Do trail check */
	JB(0)
	LABEL(l1)

	CMPRR(SPB_REG,EBX)
	JA(0)
	LABEL(l2)

	LEA(TR_REG,TR_REG,-sizeof(PWord))	/* Must trail */
	MOVMR(TR_REG,0,EBX)
  	JMP(0)
	LABEL(l3) 

	if (!firstargprocessed)
		ic_1stargalloc();

	ic_drf1fix(dpat,CURRENTPOSITION,dpat1);

	/* Read mode section goes here */
	CMPRI(EBX,con)
	JE(0)
	LABEL(l4)
	FAIL

	RELOC_INFO(RELOC_GVAR,(ic_ptr-2-sizeof(long)),symidx_wm_b_reg)

	PATCHDISP(l1)
	PATCHDISP(l2)
	PATCHLDISP(l3)
	PATCHDISP(l4)
}


/*
 * Instruction: ic_g_value
 * Function:	emits code to unify the two operands.
 * Parameters:	
 * 		sbase	-- source base register
 *		sdisp	-- source displacement
 *		dbase	-- destination base register
 *		ddisp	-- destination displacement
 * Note: If either sbase or dbase is TREG, then the displacement is the number
 *	 of the temporary to use.
 *
 * Code Summary:
 *		move	SEA, D0
 *		move	DEA, A0
 *		jsr	unify
 */
ic_g_value(sbase,sdisp,dbase,ddisp)
	int sbase, sdisp, dbase, ddisp;
{
	MOVRM(EAX,ic_base_nums[sbase],sizeof(PWord)*sdisp)
	MOVRM(EBX,ic_base_nums[dbase],sizeof(PWord)*ddisp)

	/* See comment in front of wm_unify for why these registers
	   are saved */
	PUSHR(EDX)
	PUSHR(EBP)

	/* Call the unifier. */
	CALLI(NOBASE,&UnifyPtr)

	RELOC_INFO(RELOC_GVAR,(ic_ptr-sizeof(long)),symidx_UnifyPtr);

	/* Restore the registers saved above. */
	POPR(EBP)
	POPR(EDX)
}


/*
 * Instruction: ic_g_list
 * Function:	emits code to unify an argument with a list
 * Parameters:	
 * 		base	-- base register
 *		disp	-- displacement
 * Note: If base is TREG, then the displacement is the number of the 
 *	 temporary to use.
 *
 */
ic_g_list(base,disp,x,y)
	int base,disp,x,y;
{
	CodePtr l1,l2;

	if (capturemode == WRITEMODE) {
		/* dereference loop */
		MOVRM(EAX,ic_base_nums[base],sizeof(PWord)*disp)
		ic_deref1(1,&capturepatch1,&capturepatch3);

		LEA(EAX,H_REG,MTP_LIST)
		MOVMR(S_REG,0,EAX)
		CMPRR(HB_REG,S_REG)
		JB(0)
		LABEL(l2)
		CMPRR(SPB_REG,S_REG)
		JA(0)
		LABEL(l1)
		LEA(TR_REG,TR_REG,-sizeof(PWord))
		MOVMR(TR_REG,0,S_REG)

		PATCHDISP(l1)
		PATCHDISP(l2)
	} else {
		/* sub al,MTP_LIST (short form) */
		ic_put(0x2c); ic_put(MTP_LIST);
		JE(0)
		LABEL(l1)
		FAIL

		RELOC_INFO(RELOC_GVAR,(ic_ptr-2-sizeof(long)),symidx_wm_b_reg)

		if (!firstargprocessed)
			ic_1stargalloc();

		PATCHDISP(l1)
		LEA(S_REG,S_REG,-MTP_LIST)
	}
}


/*
 * Instruction: ic_g_structure
 * Function:	emits code to unify a structure with an argumetn
 * Parameters:	
 * 		funcid	-- token index of functor
 *		arity	-- arity of structure
 *		base	-- base register
 *		disp	-- displacement
 * Note: If base is TREG, the the displacement is the number of the
 *	 temporary to use.
 */
ic_g_structure(funcid,arity,base,disp)
	int funcid,arity,base,disp;
{
	CodePtr l1,l2;
	int functor = MMK_FUNCTOR(funcid,arity);

	if (capturemode == WRITEMODE) {
		MOVRM(EAX,ic_base_nums[base],sizeof(PWord)*disp)

		ic_deref1(EBX,&capturepatch1,&capturepatch3);

		LEA(EAX,H_REG,MTP_STRUCT)
		MOVMR(EBX,0,EAX)

		CMPRR(HB_REG,EBX)
		JB(0)
		LABEL(l1)
		CMPRR(SPB_REG,EBX)
		JA(0)
		LABEL(l2)
		LEA(TR_REG,TR_REG,-sizeof(PWord))
		MOVMR(TR_REG,0,EBX)

		PATCHDISP(l1)
		PATCHDISP(l2)

		MOVMI(H_REG,0,functor)

		LEA(H_REG,H_REG,sizeof(PWord))
	} else {
		/* sub al,MTP_STRUCT (short form) */
		ic_put(0x2c); ic_put(MTP_STRUCT);
		JE(0)
		LABEL(l2)
		LABEL(l1)
		FAIL

		RELOC_INFO(RELOC_GVAR,(ic_ptr-2-sizeof(long)),symidx_wm_b_reg)

		if (!firstargprocessed)
			ic_1stargalloc();

		PATCHDISP(l2)
		LEA(S_REG,S_REG,-MTP_STRUCT)
		MOVRM(EAX,S_REG,0)	/* Make into cmp [],imm at some point */
		CMPRI(EAX,functor)

		JNE(BDISP(l1))
		LEA(S_REG,S_REG,sizeof(PWord))
	}
}


