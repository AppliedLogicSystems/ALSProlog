/*
 * icode2.c			-- more stuff to emit instructions
 *	Copyright (c) 1987 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 2/12/87
 * Revision History:
 *	Revised: 3/22/87	Kev		-- icode.c split into icode1.c
 *						   and icode2.c
 *	Revised: 2/22/88	Kev		-- Addition of code to take
 *						   care of the codeg field.
 *						   Addition of code to
 *						   symbolically branch back
 *						   to the overflow code.
 *	Revised: 3/28/90	Kev		-- Modified for VAX
 *	Revised: mm/dd/yy	Who		-- Reason
 */

#include "config.h"
#include <stdio.h>
#include "types.h"
#include "alloc.h"
#include "parser.h"
#include "icom.h"
#include "icode.h"
#include "compile.h"
#include "mtypes.h"
#include "wintcode.h"
#include "winter.h"
#include "module.h"
#include "codegen.h"
#include "machinst.h"
#include "tokens.h"

extern wm_trust_me();
extern wm_try_me();
extern wm_retry_me();
extern wm_overflow();


/*
 * ic_install_overflow_call is used to initialize the overflow field in a
 *	procedure table entry (ntbl_entry).  It also fills in the call_entry
 *	field
 *
 * 	This portion of the name entry is assumed to have the following
 *	structure:
 *	
 *	Size		Instr
 *	----		-----
 *	  6		jsb	wm_overflow
 *	  2		brb	code
 *
 *	The call entry has the following structure:
 *
 *	  2		pushl	E
 *	  3		movl	SP, E
 *	
 *
 */

ic_install_overflow_call(n)
    ntbl_entry *n;
{
    int disp;
    Code *oldptr = ic_ptr;
    ic_ptr = n->overflow;

    jsb		ABSADDR(wm_overflow)
    brb		BLAB(n->code)

    /*
     * The overflow entry should now be filled in.  The ic_ptr ought to be
     * pointing at the call entry.  Fill it in.
     */
   
    pushl	REG(E)
    movl	REG(SP) REG(E)

    ic_ptr = oldptr;
}


/*
 * ic_install_call_entry is given a procedure entry point and does nothing
 * 	with it since ic_install_overflow_call does it already.
 */

ic_install_call_entry(n)
    ntbl_entry *n;
{
    /* do nothing */
}



/*
 * ic_install_normal_exec_entry is given a procedure entry point and fills
 *	in the execute entry with the normal garbage collection checking
 *	code.
 *
 *
 * Code Summary:
 *	Size		Code
 *	----		----------
 *	 2		move.l	TR, D0
 *	 2		sub.l	H, D0
 *	 2		cmp.l	D7, D0		; D7 is the safety
 *
 *
 */

ic_install_normal_exec_entry(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    ic_ptr = n->exec_entry;

    subl3	REG(H) REG(TR) REG(r0)
    cmpl	REG(r0) REG(SAFETY)

    ic_ptr = oldptr;
}


/*
 * ic_install_spy is passed a procedure entry and a subroutine branch is
 * installed to the spy checking code at the execute entry point
 *
 * This sequence is padded to take up exactly the same amount of space as
 * the subtract and comparison above.
 *
 */


ic_install_spy(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    extern dbg_spycheck();

    ic_ptr = n->exec_entry;

    nop
    jsb		ABSADDR(dbg_spycheck)

    ic_ptr = oldptr;
}


/*
 * ic_install_decr_icount is passed a procedure entry and a subroutine call
 *	to dbg_decr_icount is installed.  This sequence is padded to take
 *	up exactly the same amount of space as the normal execute entry
 */

ic_install_decr_icount(n)
   ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    extern dbg_decr_icount();

    ic_ptr = n->exec_entry;

    nop
    jsb		ABSADDR(dbg_decr_icount)

    ic_ptr = oldptr;
}



/*
 * ic_install_exception_check takes a procedure entry and puts instructions
 *	in the code entry point for branching to the overflow code should
 *	an exception condition arise.  It leaves ic_ptr set to the next
 *	word to write to in the code entry point.
 *
 */


ic_install_exception_check(n)
   ntbl_entry *n;
{
    ic_ptr = n->code;
    blssu	BLAB(n->overflow)
}


/*
 * ic_install_resolve_ref is given a procedure entry point and fills in the
 *	code region with instructions necessary to resolve the (undefined)
 *	reference.
 *
 */

ic_install_resolve_ref(n)
   ntbl_entry *n;
{
    int disp;
    Code *oldptr = ic_ptr;
    ic_install_exception_check(n);

    moval	RELADDR(n) REG(r0)
    jmp		ABSADDR(wm_resolve_ref)

    ic_ptr = oldptr;
}


/*
 * ic_install_jmp is given a procedure entry and puts instructions
 *	in the code field to jump to the start of a clause.  This subroutine
 *	is used to set up the jump in procedures with only one clause.
 *
 */

ic_install_jmp(n,clausestart)
    ntbl_entry *n;
    long clausestart;
{
    Code *oldptr = ic_ptr;

    ic_install_exception_check(n);
    jmp		ABSADDR(clausestart)

    ic_ptr = oldptr;
}

/*
 * ic_install_try_me_jmp takes the given buffer and put code in it to do
 *	a try_me_else followed by a jump to the clause.
 */

ic_install_try_me_jmp(n,clausestart,nextclause)
   ntbl_entry *n;
   long  clausestart;
   long	 nextclause;
{
   Code *oldptr = ic_ptr;

   ic_install_exception_check(n);

   jsb		ABSADDR(wm_try_me)
   ic_putl(nextclause); nop nop
   jmp		ABSADDR(clausestart)

   ic_ptr = oldptr;
}


/*
 * ic_install_switch_on_term is used to install the switch on term code in
 *	the given buffer.  If any of straddr, lisaddr, and/or conaddr is
 *	equal to varaddr then code will be emitted to take these through
 *	the try_me_else code emitted for varaddr.  If any of these are zero,
 *	then appropriate code will be emitted for failure.
 *
 *
 */ 


ic_install_switch_on_term(n,varaddr,straddr,lisaddr,conaddr)
   ntbl_entry *n;
   long  varaddr, straddr, lisaddr, conaddr;
{
    Code *oldptr = ic_ptr;
    LABEL deref, ground, table;
    int jfail;
    int temp;
    ic_install_exception_check(n);

	movl	BDISP(E,8) REG(r0)		/*  4, get first arg */
    BLABDCL(deref)				/*  0, deref the argument */
	movl	REG(r0)	REG(S)			/*  3 */
	bicb2	BIMM(0xfc) REG(r0)		/*  4 */
	bneq	FLAB(ground)			/*  2 */
	movl	ATREG(S) REG(r0)		/*  3 */
	cmpl	REG(S) REG(r0)			/*  3 */
	bneq	BLAB(deref)			/*  2 */
	jmp	ABSADDR(varaddr)		/*  6 */
    BLABDCL(table)				/*  0 */
	if (straddr)
	    ic_putl(straddr);			/*  4 */
	else
	    ic_putl(wm_fail);
	if (lisaddr)
	    ic_putl(lisaddr);			/*  4 */
	else
	    ic_putl(wm_fail);
	if (conaddr)
	    ic_putl(conaddr);			/*  4 */
	else
	    ic_putl(wm_fail);
    FLABDCL(ground)				/*  0 */
	movzbl	REG(r0) REG(r0)			/*  3 */
	moval 	INDEX(r0) RELADDR(table-4) REG(r0) /* 5 */
	jmp	ATDISP(r0,0)			/*  3 */
	/***jmp	INDEX(r0) ATRELADDR(table-4)***/	/*  4 */
						/* -- */
						/* 50 bytes total for entry */


    

    ic_ptr = oldptr;
}

ic_install_builtin(n,builtin)
    ntbl_entry *n;
    int (*builtin)();
{
    Code *startaddr;
    Code *oldptr = ic_ptr;

    ic_install_exception_check(n);

    movl	LIMM(builtin) REG(r0)
    jmp		ABSADDR(wm_execute_builtin)

    ic_ptr = oldptr;
}

ic_install_true(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    ic_install_exception_check(n);

    movl	AUTOINCR(SP) REG(E)
    rsb

    ic_ptr = oldptr;
}

ic_install_fail(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    ic_install_exception_check(n);

    jmp	ATREG(FAIL)

    ic_ptr = oldptr;
}

extern wm_unify();
ic_install_equal(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    ic_install_exception_check(n);
    
    movl	AUTOINCR(SP) REG(E)
    movl	BDISP(SP,4) REG(r0)
    movl	BDISP(SP,8) REG(r1)
    jmp		ABSADDR(wm_unify)

    ic_ptr = oldptr;
}


/*
 * ic_install_call is used to install call/1.
 */


ic_install_call(n,whereto)
   ntbl_entry *n;
   int whereto;
{
    Code *oldptr = ic_ptr;
    ic_install_exception_check(n);

    movzwl	RELADDR(&n->modid) REG(T1)
    ashl	SIMM(4) REG(T1) REG(T1)
    addl2	SIMM(MTP_INT) REG(T1)
    jmp		ABSADDR(whereto)

    ic_ptr = oldptr;
}



/*
 * ic_install_module_closure installs code which gets the module id (an
 *	integer) of the current procedure and installs this integer as
 *	the first argument.
 */


ic_install_module_closure(n,whereto)
    ntbl_entry *n;
    int whereto;
{
    Code *oldptr = ic_ptr;
    ic_install_exception_check(n);

    movzwl	RELADDR(&n->modid) REG(r0)
    ashl	SIMM(4) REG(r0) REG(r0)
    pushl	ATREG(SP)
    movl	BDISP(SP,8) BDISP(SP,4)
    movl	REG(SP) REG(E)
    addl3	SIMM(MTP_SYM) REG(r0) BDISP(SP,8)
    jmp		ABSADDR(whereto)

    ic_ptr = oldptr;
}




extern wm_nciadc();

ic_install_next_choice_in_a_deleted_clause(buf)
   Code *buf;
{
   int disp;
   Code *oldptr = ic_ptr;
   ic_ptr = buf;

   pushal	RELADDR(buf)
   jmp		ABSADDR(wm_nciadc)

   ic_ptr = oldptr;
}



/*
 * ic_install_try_me
 *
 */

ic_install_try_me(buf,nextclause)
    Code *buf;
    long  nextclause;
{
    Code *oldptr = ic_ptr;
    ic_ptr = buf;

    jsb		ABSADDR(wm_try_me)
    ic_putl(nextclause); nop nop

    ic_ptr = oldptr;
}


/*
 * ic_install_retry_me
 *
 */

ic_install_retry_me(buf,nextclause)
    Code *buf;
    long nextclause;
{
    Code *oldptr = ic_ptr;
    ic_ptr = buf;

    jsb		ABSADDR(wm_retry_me)
    ic_putl(nextclause); nop nop

    ic_ptr=oldptr;
}


/*
 * ic_install_trust_me
 *
 */

ic_install_trust_me(buf,entrypoint)
    Code *buf;
    Code *entrypoint;
{
    Code *oldptr = ic_ptr;
    ic_ptr = buf;

    jsb		ABSADDR(wm_trust_me)
    movl	REG(E) REG(SP)
    brw		BLABW(entrypoint)

    ic_ptr = oldptr;
}


extern panic_fail();


/*
 * ic_install_no is to install both the no part for queries and the little
 *	piece of code which from which execution will start from.  The address
 *	of the place to start is returned as the value from ic_install_no
 */


Code *ic_install_no(buf,clausestart,nocatcher)
    Code *buf;
    long clausestart;
    char *nocatcher;
{
    Code *oldptr = ic_ptr;
    Code *startaddr;

    ic_ptr = buf;


    jsb	ABSADDR(wm_retry_me)
    ic_putl(panic_fail); nop nop

    jmp	ABSADDR(w_nameentry(MODULE_BUILTINS,find_token(nocatcher),0)->exec_entry)
    startaddr = ic_ptr;

    pushl	REG(E)
    movl	REG(SP) REG(E)
    bisl2	SIMM(1) REG(SPB)	/* set so first chpt is compacted */
    jsb		ABSADDR(wm_try_me)
    ic_putl(buf); nop nop
    subl2	SIMM(8)    REG(SP)
    movl	ATREG(E)   ATREG(SP)
    movl	BDISP(E,4) BDISP(SP,4)
    movl	REG(SP)    REG(E)
    jsb		ABSADDR(wm_try_me)
    ic_putl(buf); nop nop
    jmp		ABSADDR(clausestart)

    ic_ptr = oldptr;
    return startaddr;
}


/*
 * ic_install_reference is called by resolve_reference to install a jump
 *	to a non-builtin.
 */

ic_install_reference(buf,where)
    Code *buf;
    long where;
{
    Code *oldptr = ic_ptr;
    ic_ptr = buf;

    jmp		ABSADDR(where)

    ic_ptr = oldptr;
}



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

    pushl	ATREG(SP)
    movl	LIMM(n) BDISP(SP,4)
    jmp		ABSADDR(f)

    newbuf = (Code *) malloc((ic_ptr - buf) * sizeof (Code));

    for (p=buf,q=newbuf; p<ic_ptr; )
      *q++ = *p++;

    ic_ptr = oldptr;

    return (int (*)()) newbuf;

}


/*
 * ic_install_try
 *	This procedure will install a try sequence for the indexer.
 *
 *	ptr	is the address to start installing the try sequence at
 *	cstart	is the address to jump to after the choice point is created.
 *	nargs 	is the number of arguments in the clause
 *
 *	The next free address is returned as the value of the function.  The
 *	indexer expects longword (32 bit) alignment.  ic_install_try has been
 *	carefully constructed to maintain this alignment.
 *
 */

extern wm_try();

long *ic_install_try(ptr, cstart, nargs)
   long *ptr;
   long *cstart;
   int nargs;
{
   int displ;
   Code *oldptr = ic_ptr;
   ic_ptr = (Code *) ptr;

   tstl		REG(r0)		/* 2 bytes */
   jsb		ABSADDR(wm_try)	/* 6 bytes */
   ic_putl(((int) cstart));	/* 4 bytes */
				/* 12 bytes or 3 longwords total */

   ptr = (long *) ic_ptr;
   ic_ptr = oldptr;
   return ptr;
}


/*
 * ic_install_retry
 *
 *	This function is called by the indexer to install a retry sequence.
 *
 *	ptr	is the place to start installing the retry at
 *	cstart	is the pointer to the place to jump to after performing the
 *		retry operation.
 *	nargs	is the number of arguments in the procedure.
 *	emask	is the mask from the clause indicating whether or not
 *		the cp and ce need to be restored. (Not applicable on all
 *		machines)
 *
 *	The next free location is returned as the result of this function.
 *	Since the indexer needs to maintain longword alignment, this code
 *	has been carefully constructed to also maintain longword alignment.
 *
 */

extern int wm_retry();

long *ic_install_retry(ptr, cstart, nargs, emask)
   long *ptr;
   long *cstart;
   int nargs;
   int emask;
{
   int displ;
   Code *oldptr = ic_ptr;
   ic_ptr = (Code *) ptr;

   tstl		REG(r0)
   jsb		ABSADDR(wm_retry)
   ic_putl(((int) cstart));

   ptr = (long *) ic_ptr;
   ic_ptr = oldptr;
   return ptr;
}


/*
 * ic_install_trust
 *
 *	Installs a trust sequence for the indexer.
 *
 *	ptr	is the address to start installing the trust sequence at
 *	cstart	is a pointer to the address to jump to
 *	nargs	is the number of arguments for the procedure of which the
 *		clause is a part
 *	emask	is the mask indicating whether the continuation pointer
 *		needs to be restored or not
 *
 *	The next free address to continue installation at will be returned
 *	as the value of this function.  The indexer expects longword alignment
 *	to be maintained on this return value.  The code has been constructed
 *	to facilitate this.
 */


extern wm_trust();

long *ic_install_trust(ptr, cstart, nargs, emask)
   long *ptr;
   long *cstart;
   int nargs;
   int emask;
{
   Code *oldptr = ic_ptr;

   ic_ptr = (Code *) ptr;

   tstl		REG(r0)
   jsb		ABSADDR(wm_trust)
   ic_putl(((int) cstart));

   ptr = (long *) ic_ptr;
   ic_ptr = oldptr;
   return ptr;
}


/*
 * ic_install_tree_overhead
 *
 *	Installs switch_on_constant or switch_on_structure instructions for
 *	the indexer.
 *
 *	swaddr		is address of the switch_on subroutine
 *	nentries	are the number of entries which we are concerned with
 *	ic_ptr		is the place to start installing the sequence at
 *
 *	Note that ic_ptr is also a global variable which is usually referenced
 *	by the ic_put macros.  In this case, however, it refers to the
 *	parameter.
 *
 *	The following code is emitted for the VAX
 *
 *      Size            Instr
 *      ----            ---------
 *	  6		jsb	wm_sw_const (or wm_sw_struct)
 *	  2		.word	nentries
 *
 *      Total Size: 8 bytes = 2 long words
 *
 */
 
Code *ic_install_tree_overhead(swaddr, nentries, ic_ptr)
    long *swaddr;
    int nentries;
    register Code *ic_ptr;
{
    jsb		ABSADDR(swaddr)
    ic_putw(nentries);
    return ic_ptr;
}
