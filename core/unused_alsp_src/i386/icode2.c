/*
 * icode2.c			-- Emit indexing instructions
 *
 * Copyright (c) 1987-1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 2/12/87
 * Revision History:
 */

#include <stdio.h>

#include "config.h"

#include "types.h"
#include "coerce.h"
#include "alloc.h"
#include "parser.h"
#include "mtypes.h"
#include "wintcode.h"
#include "tokens.h"
#include "module.h"
#include "wamregs.h"
#include "machinst.h"
#include "icodegen.h"
#include "rinfo.h"
#include "pckgcoff.h"

#ifndef INLINECHOICE
extern wm_trust_me();
extern wm_try_me();
extern wm_retry_me();
#endif

extern long UnifyPtr;
extern long OverflowPtr;

extern dbg_spycheck();

#define CLEARL(addr) *((long *)(addr)) = (long) 0

/*
 *
 * ic_install_spy is passed the exec_entry field of a procedure
 *	table entry and installs the following code to do the spy check:
 *
 * Code Summary:
 *		Size		Code
 *		----		----------
 *		 2		movl	E,SP
 *		 1      nop
 *		 1		nop
 *		 1    	nop
 * 		 5   	movl	EBX,dbg_spycheck
 *		 2 		call	EBX
 *
 * The first instruction should stay where it is so that the wm_overflowgc()
 * code will work correctly. If the instruction changes, wm_overflowgc()
 * will have to be changed.
 *
 * Packaging Notes :
 *		we never include a spy pointed exec_entry
 *		in a package. A normal_exec_entry is packaged
 *		in its place.
 */

ic_install_spy(n)
	ntbl_entry *n;
{
	Code *oldptr = ic_ptr;

	ic_ptr = n->exec_entry;

	/* Properly set the E pointer */
	MOVRR(E_REG,SP_REG)

	/* And do the overflow check */
	NOP
	NOP
	NOP
	CALL(dbg_spycheck)
	JBE((-(NTBL_OVERFLOWSIZE+NTBL_CALLENTRYSIZE+NTBL_EXECENTRYSIZE)) &
		(0xff));

	ic_ptr = oldptr;
}


/*
 * ic_install_libbreak is passed a procedure entry and an interrupt number.  
 * The wm_interrupt_caught variable is set to the interrupt number and
 * a branch back to the overflow code is installed in the code area.
 *
 * It is then up to the interrupt handling code to determine what to do with
 * the interrupt.  The original purpose of this code is to handle the library
 * loading code.  It is parameterized, however, for other applications.
 *
 * Packaging Notes :
 *		wm_interrupt_caught and wm_safety have to be relocated
 */

ic_install_libbreak(n,i)
    ntbl_entry *n;
    int i;
{
	Code *oldptr = ic_ptr;
    
    ic_ptr = n->code;

	MOVRI(EAX,(long)&wm_interrupt_caught)  /* 5 bytes */
	MOVMI(EAX,0,(long)i)                   /* 6 bytes */
	MOVRI(EAX,(long)&wm_safety)
	MOVMI(EAX,0,(long)-1)
	JA(BDISP(n->overflow))

	ic_ptr = oldptr;
}

#ifdef PACKAGE

#define libbreak_INSTR2_OFFSET       5
#define libbreak_wm_safety_OFFSET    12
#define libbreak_SIZE1 \
           (libbreak_wm_safety_OFFSET - libbreak_INSTR2_OFFSET)
#define libbreak_INSTR4_OFFSET \
           (libbreak_wm_safety_OFFSET+4)
#define libbreak_SIZE2 \
           (CODESIZE_BYTES - libbreak_INSTR4_OFFSET)

coff_init_libbreak()
{
  INSERT_SYM(symidx_wm_interrupt_caught, "wm_interrupt_caught");
  /* wm_safety inserted in  icode1.c */
}

coff_pckg_libbreak(ent)
	 ntbl_entry *ent;
{
  COFF_BYTE_RAWDATA(ent->code);
  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_interrupt_caught),P_R_VIR32,0);
  COFF_LONG_RAWDATA(0);
  COFF_RAWDATA((char *)(ent->code)+libbreak_INSTR2_OFFSET,libbreak_SIZE1);
  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_safety),P_R_VIR32,0);
  COFF_LONG_RAWDATA(0);
  COFF_RAWDATA((char *)(ent->code)+libbreak_INSTR4_OFFSET,libbreak_SIZE2);
}

#endif
  
/*
 * ic_install_decr_icount is passed the name table entry of a procedure
 *	call to dbg_decr_icount is installed.
 *
 * Packaging Notes :
 *		we do not package this code. Instead we package a
 *		normal_exec_entry in its place.
 */

ic_install_decr_icount(n)
   ntbl_entry *n;
{
    extern dbg_decr_icount();
	Code *oldptr = ic_ptr;

    ic_ptr = n->exec_entry;
	/* Properly set the E pointer */
	MOVRR(E_REG,SP_REG)

	/* And skip the overflow check */
	NOP
	NOP
	NOP
	NOP
	NOP
	/* the return address from CALL should point to
	 * code area. This is passed as argument to decr_icount.
	 */
	CALL(dbg_decr_icount)

	ic_ptr = oldptr;
}


/*
 *
 * ic_install_normal_exec_entry is passed the exec_entry field of a procedure
 *	table entry and installs the following code to do the interrupt check:
 *
 * Code Summary:
 *		Size		Code
 *		----		----------
 *		 2		movl	E,SP
 *		 2		movl	EBX,TR
 *		 2		subl	EBX,H
 *		 6		cmpl	EBX, wm_safety
 *		 2		jbe     -offset
 * The first instruction should stay where it is so that the wm_overflowgc()
 * code will work correctly. If the instruction changes, wm_overflowgc()
 * will have to be changed.
 *
 * Packaging Notes :
 *		"wm_safety" has to be marked for relocation. This is at
 *		an offset of 8 bytes from start of exec_entry and is
 *		4 bytes long.
 */

ic_install_normal_exec_entry(n)
	ntbl_entry *n;
{
	Code *oldptr = ic_ptr;

	ic_ptr = n->exec_entry;

	/* Properly set the E pointer */
	MOVRR(E_REG,SP_REG)

	/* And do the overflow check */
	MOVRR(EBX,TR_REG)
	SUBRR(EBX,H_REG)
	CMPRM(EBX,NOBASE,&wm_safety)
	
	JBE((-(NTBL_OVERFLOWSIZE+NTBL_CALLENTRYSIZE+NTBL_EXECENTRYSIZE)) &
		(0xff));

	ic_ptr = oldptr;
}

#ifdef PACKAGE
   /* extern wm_safety loaded into coff symbol table in icode1.c */

#define wm_safety_OFFSET  8
#define wm_safety_BOFFSET (EXECENTRYSIZE_BYTES - wm_safety_OFFSET)

static ntbl_entry exec_entry_template;

coff_init_exec_entry()
{
  ic_install_normal_exec_entry(&exec_entry_template);
  CLEARL((char *)(exec_entry_template.exec_entry) + wm_safety_OFFSET);
}

coff_pckg_exec_entry(ent)
	 ntbl_entry *ent;
{
  /* package exec entry */
  COFF_RAWDATA(exec_entry_template.exec_entry,EXECENTRYSIZE_BYTES);

  /* insert a relocation entry into  relocation table */
  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_safety),P_R_VIR32,-wm_safety_BOFFSET);
  return(1);
}

#endif

/*
 * ic_install_call_entry
 *
 * Packaging Notes : no relocation needed.
 */

ic_install_call_entry(n)
	ntbl_entry *n;
{
    Code *oldptr = ic_ptr;

	ic_ptr = n->call_entry;

	/* Save the E register in the Old E slot in the environment */
	PUSHR(E_REG)

	ic_ptr = oldptr;
}

#ifdef PACKAGE

coff_pckg_call_entry(ent)
	 ntbl_entry *ent;
{
  COFF_RAWDATA(ent->call_entry,CALLENTRYSIZE_BYTES);
  return(1);
}

#endif


extern wm_nciadc();
 
ic_install_next_choice_in_a_deleted_clause(buf)
   	Code *buf;
{
    Code *oldptr = ic_ptr;

   	ic_ptr = buf;

	PUSHI(buf);
   	JMP((int) wm_nciadc)

	ic_ptr = oldptr;
}


/*
 * ic_install_try_me
 *
 * Packaging Notes :
 *		the following addresses need to be relocated
 *		wm_try_me at offset 2 bytes
 *		secondclause at offset 8 bytes
 *		firstclause at offset 13 bytes
 */

ic_install_try_me_jmp(n,firstclause,secondclause)
	ntbl_entry *n;
	Code *firstclause, *secondclause;
{
    Code *oldptr = ic_ptr;

	ic_ptr = n->code;

	/* Want to use long form of MOVI so that things line up on boundary */
	LMOVRI(EAX,(BigOffset)wm_try_me)
	
	CALLR(EAX)

	ic_putl((BigOffset)secondclause);

	MOVRI(EAX,(BigOffset)firstclause)

	JMPR(EAX)

	ic_ptr = oldptr;
}

#ifdef PACKAGE

#define wm_try_me_OFFSET  2
#define wm_try_me_BOFFSET (CODESIZE_BYTES - wm_try_me_OFFSET)
#define next_clause_OFFSET 8
#define next_clause_BOFFSET (CODESIZE_BYTES - next_clause_OFFSET)
#define first_clause_OFFSET 13
#define first_clause_BOFFSET (CODESIZE_BYTES - first_clause_OFFSET)

static ntbl_entry code_multiple_template;

coff_init_code_multiple()
{
  INSERT_SYM(symidx_wm_try_me, 			"wm_try_me");

  ic_install_try_me_jmp(&code_multiple_template,0,0);

  CLEARL((char *)(code_multiple_template.code)+wm_try_me_OFFSET);

}

coff_pckg_code_multiple(ent,procname)
	 ntbl_entry *ent;
	 char * procname;
{
  char buf[64];

  COFF_RAWDATA(code_multiple_template.code,CODESIZE_BYTES);

  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_try_me),P_R_VIR32,-wm_try_me_BOFFSET);

  sprintf(buf,"%s_%x_%s",procname,
		  clauseId(nextClauseAddr(ent->first_clause)),"choice");
  COFF_RELOC_SYMBOL(buf,P_R_VIR32,-next_clause_BOFFSET);

  sprintf(buf,"%s_%x_%s",procname,clauseId(ent->first_clause),"code");
  COFF_RELOC_SYMBOL(buf,P_R_VIR32,-first_clause_BOFFSET);
  return(1);
}

#endif


/*
 * ic_install_try_me
 *
 */
ic_install_try_me(buf,nextclause,nargs)
	Code *buf;
	Code *nextclause;
	int nargs;
{
    Code *oldptr = ic_ptr;

	ic_ptr = buf;

	/* Want to use long form of MOVI so that things line up on boundary */
	LMOVRI(EAX,wm_try_me)
	CALLR(EAX)
	ic_putl((long)nextclause);

	ic_ptr = oldptr;
}


/*
 * ic_install_retry_me
 *
 * Packaging Notes :
 *		wm_retry_me at offset 2 and 
 *		nextclause at offset 8 have to be relocated
 */
ic_install_retry_me(buf,nextclause,arity,emask)
	Code *buf;
	Code *nextclause;
	int arity;
	long emask;
{
    Code *oldptr = ic_ptr;

	ic_ptr = buf;

	LMOVRI(EAX,wm_retry_me)
	
	CALLR(EAX)

	ic_putl(nextclause);

	ic_ptr = oldptr;
}

#ifdef PACKAGE

#define CHOICESIZE  (WCI_CLAUSECODE - WCI_CHOICECODE)
#define CHOICESIZE_BYTES (CHOICESIZE*sizeof(long))

static long retry_me_template[CHOICESIZE];

#define wm_retry_me_OFFSET  2
#define wm_retry_me_BOFFSET (CHOICESIZE_BYTES - wm_retry_me_OFFSET)
#define retry_next_OFFSET   8
#define retry_next_BOFFSET  (CHOICESIZE_BYTES - retry_next_OFFSET)

coff_init_retry_me()
{
  INSERT_SYM(symidx_wm_retry_me, 			"wm_retry_me");

  ic_install_retry_me(retry_me_template,0,0,0);
  CLEARL((char *)retry_me_template+wm_retry_me_OFFSET);
}

coff_pckg_retry_me(procname,ca,ent)
	 char *procname;
	 long *ca;
	 ntbl_entry *ent;
{
  char buf[64];

  COFF_RAWDATA(retry_me_template,CHOICESIZE_BYTES);

  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_retry_me),P_R_VIR32,-wm_retry_me_BOFFSET);

  sprintf(buf,"%s_%x_%s",procname,clauseId(nextClauseAddr(ca)),"choice");
  COFF_RELOC_SYMBOL(buf,P_R_VIR32,-retry_next_BOFFSET);

  return(1);
}
#endif

/*
 * ic_install_trust_me
 *
 * Packaging Notes :
 *		wm_trust_me at offset 1 has to be relocated
 */
ic_install_trust_me(buf,whereto,arity,emask)
	Code *buf;
	Code *whereto;
	int arity;
	long emask;
{
	BigOffset disp;
    Code *oldptr = ic_ptr;

	ic_ptr = buf;

	MOVRI(EAX,wm_trust_me)
	
	CALLR(EAX)
	
	/* 5 is size of following jump instruction */

	disp = whereto-(ic_ptr+5);
	JMP(disp)

	ic_ptr = oldptr;
}

#ifdef PACKAGE

#define MOVRISIZE_BYTES  5
#define TAILSIZE_BYTES (CHOICESIZE_BYTES - MOVRISIZE_BYTES)

coff_init_trust_me()
{
  INSERT_SYM(symidx_wm_trust_me, 			"wm_trust_me");
}

coff_pckg_trust_me(ca,ent)
	 long *ca;
	 ntbl_entry *ent;
{
  COFF_BYTE_RAWDATA(*(char *)choiceEntry(ca));
  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_trust_me),P_R_VIR32,0);
  COFF_LONG_RAWDATA(0);
  COFF_RAWDATA((((char *)choiceEntry(ca))+MOVRISIZE_BYTES),TAILSIZE_BYTES);

  return(1);
}
#endif

/*
 * icIndexPatch: Put down a choice point instruction in an index patch.
 * 
 * The instruction looks like
 * 
 *	MOV EAX, choicepoint instruction address pointer
 *	CALL EAX
 *	NOP
 *	where address
 *
 * The code is 12 bytes long.
 *
 * The nop is to long word align the whole thing. If this moves, the wm_try,
 * wm_retry, and wm_trust instructions must be modified.
 * 
 * The choice point instruction address is placed in a register so that we can
 * jump to an absolute address, something the 386 only allows to be done from
 * a register or memory location. An offset could be placed here, but then the
 * code couldn't be swapped by Prolog without a lot of work to fix these
 * offsets.
 */
Code *icIndexPatch(place,choiceInstr,where)
	Code *place;		/* Where this code is to go */
	Code *choiceInstr;	/* The index patch choice point instruction address */
	Code *where;		/* Where to go after the choice instruction */
{
	*place++ = 0xb8;	/* mov EAX, choiceInstr */
	*(CodePtrPtr(place))++ = choiceInstr;

	*place++ = 0xff;	/* Call EAX */
	*place++ = 0xd0;

	*place++ = 0x90;	/* nop */

	*(CodePtrPtr(place))++ = where;

	return place;
}


/*
 * ic_install_switch_on_term is used to install the switch on term code in
 *	the given buffer.  If any of straddr, lisaddr, and/or conaddr is
 *	equal to varaddr then code will be emitted to take these through
 *	the try_me_else code emitted for varaddr.  If any of these are zero,
 *	then appropriate code will be emitted for failure.
 *
 * Code Summary:
 *		Size		Code
 *		----		--------
 *	 	IEC_END*2	exception_check code
 *		 4			movl	4(SP), EAX
 *		 2			movl	EAX,EBX
 *		 2			andb	$3,AL
 *		 2			je	3f
 *		 2		1:	subb	$2, AL
 *
 *	Subtotal 1: IEC_END*2+14 bytes = IEC_END+7 words required for the above
 *
 *	If the list address to jump to is not fail, the
 *	following code is emitted:
 *
 *		 6			jel	lisaddroffset
 *
 *	Else nothing is emitted for the list address, but a jump to wm_fail
 *	will need to be emitted later on.
 *
 *	
 *	If the structure address to jump to is not fail, the
 *	following code is emitted:
 *
 *	 	 6			jal	straddroffset
 *
 *	Else nothing is emitted for the list address, but a jump to wm_fail
 *	will be emitted later on
 *
 *	If the constant address to jump to is not fail, the
 *	following code is emitted:
 *
 *		 6			jbl	conaddroffset
 *
 *	Else nothing is emitted for the constant address.
 *
 *	If in any of the three above cases, one or more of the jel, jal, or
 *	jbl was not emitted, then the following code is emitted:
 *
 *		 2			movl	wm_b_re, EAX
 *		 2			jmp	(EAX)
 *
 *	Subtotal 2: 	At most 18 bytes = 9 words are required for the above 
 *			branches.
 *	
 *
 *		 2		3:	movl	(EBX), EAX
 *		 2			cmp.l	EAX, EBX
 *		 6			jel	varaddroffset
 *		 2			movl	EAX, EBX
 *		 2			andb	$3, EAX
 *		 2			je	3b
 *		 4			movl	EAX, 4(SP)
 *		 2			jmp	1b
 *
 *	Subtotal 3:	22 bytes = 11 words required
 *
 *	At most IEC_END*2+54 bytes = IEC_END+27 words are required for the
 *	switch code.
 *
 * Packaging Notes :
 *		switch_on_term is not packaged. A try_me_jmp
 *		is packaged in its place.
 */ 
ic_install_switch_on_term(nameEntry,straddr,lisaddr,conaddr)
	ntbl_entry *nameEntry;
	Code *straddr, *lisaddr, *conaddr;
{
	Code *oldptr = ic_ptr;
	Code *tagCheck, *drf, *naive;
	Code *pstr, *plis, *pcon;
	int jfail;
	int temp;

	ic_ptr = nameEntry->code;

	MOVRM(EAX,SP_REG,2*sizeof(PWord))
	MOVRR(EBX,EAX)
	ic_put(0x24);	ic_put(0x03);	/* andb $3,AL */
	JE(0) LABEL(drf)

	LABEL(tagCheck)

	jfail = 0;

	/* We must see what kind of tag we have. Since the non-variable
	   ones are in the range 1-3, subtracting by 2 allows easy
	   differentiation between them. */

	ic_put(0x2c);	ic_put(0x02);	/* subb $2,AL */

	if (lisaddr == 0)
		jfail = 1;
	else if (lisaddr == (CodePtr)(-1)) {	/* Need naive chain */
		JE(0) LABEL(plis)
	} else {
		/* 6 is to account for the JEL instruction */
		temp = lisaddr - (CodePtr) ic_ptr - 6;
		JEL(temp)
	}

	if (straddr == 0)
		jfail = 1;
	else if (straddr == (CodePtr)(-1)) {	/* Need naive chain */
		JB(0) LABEL(pstr)
	} else {
		/* 6 is to account for the JBL instruction */
		temp = straddr - (CodePtr) ic_ptr - 6;
		JBL(temp)
	}

	if (conaddr == 0)
		jfail = 1;
	else if (conaddr == (CodePtr)(-1)) {	/* Need naive chain */
		JA(0) LABEL(pcon)
	} else {
		/* 6 is to account for the JAL instruction */
		temp = conaddr - (CodePtr) ic_ptr - 6;
		JAL(temp)
	}

	if (jfail) FAIL

	PATCHDISP(drf)		/* Patch offset above */
	LABEL(drf)

	MOVRM(EAX,EBX,0)
	CMPRR(EAX,EBX)

	JE(0)	LABEL(naive)

	MOVRR(EBX,EAX)
	ic_put(0x24);	ic_put(0x03);	/* andb $3,AL */
	JE(BDISP(drf))

	MOVMR(ESP,8,EBX)
	JMP(BLDISP(tagCheck))

	/* Get the naive chain ground items here */

	if (lisaddr == (CodePtr)(-1))
		PATCHDISP(plis);

	if (straddr == (CodePtr)(-1))
		PATCHDISP(pstr);

	if (conaddr == (CodePtr)(-1))
		PATCHDISP(pcon);

	PATCHDISP(naive)

	/* try_me instruction for naive chain */

	LMOVRI(EAX,wm_try_me)
	CALLR(EAX)
	ic_putl((CodePtr)((long *)*(nameEntry->first_clause
					+ WCI_NEXTCLAUSEADDR) +
				WCI_CHOICEENTRY));

	MOVRI(EAX,(CodePtr)(nameEntry->first_clause + WCI_CLAUSECODE))
	JMPR(EAX)

	/* Reset the ic_ptr */
	ic_ptr = oldptr;
}


/*
 * ic_install_no is to install both the no part for queries and the little
 *	piece of code which from which execution will start from.  The address
 *	of the place to start is returned as the value from ic_install_no
 */
Code *ic_install_no(buf,clausestart,nocatcher)
	Code *buf;
	Code *clausestart;
	char *nocatcher;
{
	Code *oldptr = ic_ptr;
	Code *startaddr;

	ic_ptr = buf;

	/*
	 * We are using retry_me instruction instead of trust_me instruction
	 * because we don't want to get rid of the first choice point (that
	 * is only in the choice point stack). 	-- Ilyas 5/15/91
	 */

	CALL(wm_retry_me)
	ic_putl(buf);

	MOVRI(EAX,w_nameentry(MODULE_BUILTINS,
			(PWord)find_token(nocatcher),0)->exec_entry)

	JMPR(EAX)		/* Go to return address */

	/* This is where wm_exec will start execution. */
	startaddr = ic_ptr;

	/* One fake procedure entry coming up. */

	/* Fake call_entry. Complete the top environment. */
	PUSHR(E_REG)

	/* Fake exec_entry. Set E register. */
	MOVRR(E_REG,SP_REG)

	/* Fake code entry. Set up choice point. */
	CALL(wm_try_me)
	ic_putl(buf);

	/* mark first chpt as compacted (at SPB) */

	ORMI(TR_REG,8,1)

	/*
	 * When Prolog starts, we need an environment at the top level
	 * so that the top-level goal
	 *
	 *		:- !,fail.
	 *
	 * will have an environment to stop at.
	 */

	PUSHM(E_REG,4)
	PUSHR(E_REG)	/* PUSHM(E_REG,0) */

	MOVRR(E_REG,SP_REG)	/* Make sure E points at this environment */

	/*
	 * We are  going to create the second choice point after we create
	 * the environment above, so that the second choice point can
	 * point at that environment. This is necessarry for GC.
	 * We cannot put an environment which cannot be accessed by
	 * anybody. 	-- Ilyas 5/15/91
	 */
	/* Another for cutting away */
	CALL(wm_try_me)
	ic_putl(buf);
	/* mark second chpt as compacted too (at SPB) */
	ORMI(TR_REG,8,1)

	/* Now we start the user query. */

	MOVRI(EAX,clausestart)
	JMPR(EAX)

	ic_ptr = oldptr;

	return startaddr;
}


/*
 * This is used to install builtins written in C
 */
ic_install_builtin(n,builtin)
	ntbl_entry *n;
	int (*builtin)();
{
    Code *oldptr = ic_ptr;

	ic_ptr = n->code;

	MOVRI(EAX,builtin)
	MOVRI(EBX,wm_execute_builtin)
	JMPR(EBX)

	ic_ptr = oldptr;
}

#ifdef PACKAGE

#define builtin_OFFSET      1
#define builtin_BOFFSET     (CODESIZE_BYTES - builtin_OFFSET)
#define wm_execute_builtin_OFFSET  6
#define wm_execute_builtin_BOFFSET (CODESIZE_BYTES-wm_execute_builtin_OFFSET)

static ntbl_entry code_builtin_template;

coff_init_code_builtin()
{
  INSERT_SYM(symidx_wm_execute_builtin, 	"wm_execute_builtin");

  ic_install_builtin(&code_builtin_template,0);

  CLEARL((char *)(code_builtin_template.code)+wm_execute_builtin_OFFSET);
}

coff_pckg_code_builtin(ent)
	 ntbl_entry *ent;
{
  extern char * builtin_name();
  char *ptr;

  COFF_RAWDATA(code_builtin_template.code,CODESIZE_BYTES);

  ptr = builtin_name(*(long *)((char *)(ent->code)+builtin_OFFSET));
  COFF_RELOC_SYMBOL(ptr,P_R_VIR32,-builtin_BOFFSET);

  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_execute_builtin),P_R_VIR32,-wm_execute_builtin_BOFFSET);

  return(1);
}
#endif

  
/*  
 * This is used most of the time to install builtins written in Assembly 
 */
ic_install_jmp(n,whereto)
	ntbl_entry *n;
	Code *whereto;
{
    Code *oldptr = ic_ptr;

	ic_ptr = n->code;

	MOVRI(EAX,whereto)
	JMPR(EAX)

	ic_ptr = oldptr;
}

#ifdef PACKAGE

#define jmp_whereto_OFFSET  1
#define jmp_whereto_BOFFSET (CODESIZE_BYTES - jmp_whereto_OFFSET)

static ntbl_entry code_jmp_template;

coff_init_code_jmp()
{
  ic_install_jmp(&code_jmp_template,0);
}

coff_pckg_code_jmp(ent)
	 ntbl_entry *ent;
{
  extern char * builtin_name();
  char *ptr;

  COFF_RAWDATA(code_jmp_template.code,CODESIZE_BYTES);

  ptr = builtin_name(*(long *)((char *)(ent->code)+jmp_whereto_OFFSET));
  COFF_RELOC_SYMBOL(ptr,P_R_VIR32,-jmp_whereto_BOFFSET);

  return(1);
}

coff_pckg_code_single(ent,procname)
	 ntbl_entry *ent;
	 char *procname;
{
  char buf[64];

  COFF_RAWDATA(code_jmp_template.code,CODESIZE_BYTES);

  sprintf(buf,"%s_%x_%s",procname,clauseId(ent->first_clause),"dstart");
  COFF_RELOC_SYMBOL(buf,P_R_VIR32,-jmp_whereto_BOFFSET);

  return(1);
}

#endif

/*
 * This is used to (surprise!) install resolve_refs.
 *
 * If the size of this code changes, the constant SIZERESOLVECODE
 * in wntbl.m4 must be changed to reflect the new size. The size
 * is in Code words.
 */
ic_install_resolve_ref(n)
	ntbl_entry *n;
{
    Code *oldptr = ic_ptr;

	ic_ptr = n->code;

	MOVRI(EAX,(BigOffset)wm_resolve_ref)
	CALLR(EAX)

	ic_ptr = oldptr;
}

#ifdef PACKAGE

static ntbl_entry resolve_ref_template;

#define wm_resolve_ref_OFFSET 1
#define wm_resolve_ref_BOFFSET (CODESIZE_BYTES - wm_resolve_ref_OFFSET)

coff_init_resolve_ref()
{
  INSERT_SYM(symidx_wm_resolve_ref, 		"wm_resolve_ref");

  ic_install_resolve_ref(&resolve_ref_template);
  CLEARL((char *)(resolve_ref_template.code)+wm_resolve_ref_OFFSET);
}

coff_pckg_resolve_ref(ent)
	 ntbl_entry *ent;
{
  COFF_RAWDATA(resolve_ref_template.code,CODESIZE_BYTES);

  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_wm_resolve_ref),P_R_VIR32,-wm_resolve_ref_BOFFSET);

  return(1);
}

#endif


/*
 * I don't believe that this works anymore.  The environment pointer
 * needs to be picked up off of the stack and put back into E.  But
 * I don't know how to fix it on the 386.   -kev.
 *
 * I think just copying the code out of ic_proceed should be enough. -keith
 *
 * Note:  Whoever fixes it (or at least verifies that it is indeed correct
 * and that I am badly mistaken) should delete the comment.  
 */
ic_install_true(n)
	ntbl_entry *n;
{
	Code *oldptr = ic_ptr;

	ic_ptr = n->code;

	POPR(EBP)
	RET

	ic_ptr = oldptr;
}

ic_install_fail(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    ic_ptr = n->code;
    FAIL
    ic_ptr = oldptr;
}


ic_install_equal(n)
	ntbl_entry *n;
{
	Code *oldptr = ic_ptr;

	ic_ptr = n->code;

	MOVRM(EAX,SP_REG,2*sizeof(PWord))
	MOVRM(EBX,SP_REG,3*sizeof(PWord))

	/* See comment in front of wm_unify for why these registers
	   are saved */
	PUSHR(EDX)
	PUSHR(EBP)

	/* Call the unifier. */
	CALLI(NOBASE,&UnifyPtr)

	/* Restore the registers saved above. */
	POPR(EBP)
	POPR(EDX)

	/* Get Old E and then go home */
	POPR(EBP)
	RET

	ic_ptr = oldptr;
}


ic_install_call()
{
	fprintf(stderr,"\nInternal Error: ic_install_call function not implemented");
	als_exit(1);
}


/*
 * ic_install_reference is called by resolve_reference to install a jump
 *	to a non-builtin.
 */
ic_install_reference(buf,whereto)
	CodePtr buf;
	CodePtr whereto;
{
    Code *oldptr = ic_ptr;

	ic_ptr = buf;

	/* Assume jump location does the overflow check */
	MOVRI(EAX,whereto)
	JMPR(EAX)

	ic_ptr = oldptr;
}


/*
 * ic_install_overflow_call is used to initialize the overflow field in a
 *	procedure table entry (ntbl_entry)
 *
 *	Takes up 3 words.
 */
ic_install_overflow_call(n)
	ntbl_entry *n;
{
	Code *oldptr = ic_ptr;

	ic_ptr = n->overflow;

	CALLI(NOBASE, &OverflowPtr)

	ic_ptr = oldptr;
}

#ifdef PACKAGE

#define OverflowPtr_OFFSET   2
#define OverflowPtr_BOFFSET  (OVERFLOWSIZE_BYTES - OverflowPtr_OFFSET)

static ntbl_entry overflow_template;

coff_init_overflow()
{
  INSERT_SYM(symidx_OverflowPtr,		 	"OverflowPtr");

  ic_install_overflow_call(&overflow_template);
  CLEARL((char *)(overflow_template.overflow) + OverflowPtr_OFFSET);
}

coff_pckg_overflow(ent)
	 ntbl_entry *ent;
{
  COFF_RAWDATA(overflow_template.overflow,OVERFLOWSIZE_BYTES);
  
  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_OverflowPtr),P_R_VIR32,-OverflowPtr_BOFFSET);

  return(1);
}
#endif
  
/*
 * ic_install_module_closure installs code which gets the module id (an
 *	integer) of the current procedure and installs this integer as
 *	the first argument.
 *
 *
 *  Size: 12 bytes
 *
 * If the size of the code in here changes, SIZEMODCODE in wntbl.m4 must be
 * changed to reflect the new size.
 */
ic_install_module_closure(ent,whereto)
	ntbl_entry *ent;
	Code *whereto;
{
	int call_mod_closure();
    Code *oldptr = ic_ptr;

	ic_ptr = ent->code;

	MOVRI(EBX,(BigOffset)whereto)
	MOVRI(EAX,(BigOffset)(call_mod_closure))
	CALLR(EAX)

	ic_ptr = oldptr;
}

#ifdef PACKAGE

#define whereto_OFFSET   1
#define whereto_BOFFSET  (CODESIZE_BYTES- whereto_OFFSET)
#define call_mod_closure_OFFSET 6
#define call_mod_closure_BOFFSET (CODESIZE_BYTES - call_mod_closure_OFFSET)

static ntbl_entry code_modclosure_template;

coff_init_code_modclosure()
{
  INSERT_SYM(symidx_call_mod_closure, 	"call_mod_closure");

  ic_install_module_closure(&code_modclosure_template,0);

  CLEARL((char *)(code_modclosure_template.code)+call_mod_closure_OFFSET);
}

coff_pckg_code_modclosure(ent)
	 ntbl_entry *ent;
{
  Code *codeentry;
  ntbl_entry *toent;
  char buf[64];

  COFF_RAWDATA(code_modclosure_template.code,CODESIZE_BYTES);

  codeentry = (Code *)*(long*)((char *)(ent->code)+whereto_OFFSET);
  toent = (ntbl_entry *)(codeentry - NTBL_ENTRYSIZE);
  ntbl_entry_name(toent,"code",buf);

  COFF_RELOC_SYMBOL(buf,P_R_VIR32,-whereto_BOFFSET);

  COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(symidx_call_mod_closure),P_R_VIR32,-call_mod_closure_BOFFSET);

  return(1);
}
#endif


#ifdef huh
/*
 * mk_cfunc was written so Chris could do the user interface stuff.
 * 	It creates and returns a pointer to a C-callable function.  It is
 *	given an integer n and a function pointer f.  When called with
 *	arguments A,B,C,...Z, it will in turn call f with arguments
 *	n,A,B,C,...,Z.  The parameter n is intended to be used as a function
 *	identifier.  f will in all likelyhood be a common function which will
 *	handle all of the function identifiers and dispatch to other 
 *	(possibly Prolog) code.
 *	
 */
int (*mk_cfunc(n,f))()
	int n;
	int (*f)();
{
	Code buf[100];
	Code *oldptr = ic_ptr;
	Code *newbuf;
	register Code *p,*q;
	extern char *malloc();

	ic_ptr = buf;
	MOVE(SP_REG, INDIRECT, 0, SP_REG, PREDECR, 0)
	MOVI(n, SP_REG, DISPL, 4)
	JMP(((long) f))
   
	newbuf = (CodePtr ) malloc((ic_ptr - buf) * CodeSize);

	for (p=buf,q=newbuf; p<ic_ptr; )
		*q++ = *p++;
   
	ic_ptr = oldptr;

	return (int (*)()) newbuf;
}
#endif

#ifdef PACKAGE

coff_init_icode2()
{
  coff_init_libbreak();
  /* there is no init routine for call entry */
  coff_init_exec_entry();
  coff_init_overflow();
  coff_init_code_builtin();
  coff_init_code_modclosure();
  coff_init_code_jmp(); /*  same as coff_init_code_single() */
  coff_init_code_multiple();
  coff_init_resolve_ref();
  coff_init_retry_me();
  coff_init_trust_me();
}
#endif
