/*
 * icode2.c			-- more stuff to emit instructions
 *	Copyright (c) 1987-1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation: 2/12/87
 * 03/22/87 - K.Buettner -- icode.c split into icode1.c and icode2.c
 * 02/22/88 - K.Buettner -- Add code to take care of the codeg field.
 *					Add code to symbolically branch back
 *					to the overflow code.
 * 10/26/94 - C. Houpt	-- Rename ic_install_tree_overhead()'s
 *						   parameter ic_ptr and create local to shadow the
 *						   global ic_uptr.
 *						-- Also Various UCHAR* casts.
 */

#include <stdio.h>

#include "defs.h"
#include "icom.h"
#include "icode.h"
#include "compile.h"
#include "wintcode.h"
#include "module.h"
#include "machinst.h"

static	void	ic_install_exception_check	PARAMS(( ntbl_entry * ));

/*
 * if we're compiling the Mac System, we will need to make sure the
 * functions in this file know where the Fail address is being stored.
 * The actual declaration of Fail is in winter.c.
 */

#ifdef MacOS
extern long *Fail;
#endif

/*
 * ic_install_overflow_call is used to initialize the overflow field in a
 *	procedure table entry (ntbl_entry).  It also fills in the call_entry
 *	field
 *
 * 	This portion of the name entry is assumed to have the following
 *	structure:
 *	
 *	6			jsr	overflow
 *	2			bra.s	code
 *	
 *
 */

void
ic_install_overflow_call(n)
    ntbl_entry *n;
{
    int disp;
    Code *oldptr = ic_ptr;
    ic_ptr = n->overflow;
    JSR(OVERFLOW)
    disp = LDISP(n->code);
    BRA(disp)

    /*
     * The overflow entry should now be filled in.  The ic_ptr ought to be
     * pointing at the call entry.  Fill it in.
     */
   
    LINK(E, 0)

    ic_ptr = oldptr;
}


/*
 * ic_install_call_entry is given a procedure entry point and does nothing
 * 	with it since ic_install_overflow_call does it already.
 */

void
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

void
ic_install_normal_exec_entry(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    ic_ptr = n->exec_entry;

    MOVE(TR,ADIRECT,0,D0,DDIRECT,0);
    SUBAD(H,D0)
    CMPDD(OV,D0)

    ic_ptr = oldptr;
}


/*
 * ic_install_spy is passed a procedure entry and a jsr to the spy checking 
 * code is installed at the execute entry point
 *
 *
 *	Note that this jsr takes up exactly six bytes (the same as the above
 *	move, subtract and compare sequence.
 */


extern	void	dbg_spycheck	PARAMS(( void ));

void
ic_install_spy(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;

    ic_ptr = n->exec_entry;
    JSR((long) dbg_spycheck)

    ic_ptr = oldptr;
}


/*
 * ic_install_libbreak is passed a procedure entry and an interrupt number.
 * The wm_interrupt_caught variable is set to the interrupt number and
 * a branch back to the overflow code is installed in the code area.
 *
 * It is then up to the interrupt handling code to determine what to so with
 * the interrupt.  The original purpose of this code is to handle the library
 * loading code.  It is parameterized, however, in order to support other
 * applications.
 */

void
ic_install_libbreak(n,i)
    ntbl_entry *n;
    int i;
{
    int displ;
    Code *oldptr = ic_ptr;

    ic_install_exception_check(n);
    MOVI2Addr(((long) i),((long) &wm_interrupt_caught))
    MOVI(-1,OV,DDIRECT,0)
    displ = LDISP(n->overflow);
    BRA(displ)

    ic_ptr = oldptr;
}



/*
 * ic_install_decr_icount is passed the codeg filed of a procedure entry and a
 *	jsr to dbg_decr_icount is installed.
 *
 *	Note that this jsr takes up exactly six bytes.
 */

extern	void	dbg_decr_icount		PARAMS(( void ));

void
ic_install_decr_icount(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;

    ic_ptr = n->exec_entry;
    JSR((long) dbg_decr_icount)

    ic_ptr = oldptr;
}



/*
 * ic_install_exception_check takes a procedure entry and puts instructions
 *	in the code entry point for branching to the overflow code should
 *	an exception condition arise.  It leaves ic_ptr set to the next
 *	word to write to in the code entry point.
 *
 */

static void
ic_install_exception_check(n)
    ntbl_entry *n;
{
    int ldisp;

    ic_ptr = n->code;
    ldisp = LDISP(n->overflow);
    BLO(ldisp)
}


/*
 * ic_install_resolve_ref is given a procedure entry point and fills in the
 *	code region with instructions necessary to resolve the (undefined)
 *	reference.
 *
 */

extern	void	wm_resolve_ref		PARAMS(( void ));

void
ic_install_resolve_ref(n)
    ntbl_entry *n;
{
    int disp;
    Code *oldptr = ic_ptr;

    ic_install_exception_check(n);
    ic_put(040772);			/* lea (disp,PC), A0 */
    disp = ((char *) n) - ((char *) ic_ptr);
    ic_put(disp);			/* displacement to get us back to
					 * the beginning of the name entry
					 */
    JMP(wm_resolve_ref)

    ic_ptr = oldptr;
}


/*
 * ic_install_jmp is given a procedure entry and puts instructions
 *	in the code field to jump to the start of a clause.  This subroutine
 *	is used to set up the jump in procedures with only one clause.
 *
 */

void
ic_install_jmp(n, clausestart, emask)
    ntbl_entry *n;
    Code *clausestart;
    int emask;
{
    Code *oldptr = ic_ptr;

    ic_install_exception_check(n);
    JMP(clausestart)

    ic_ptr = oldptr;
}

/*
 * ic_install_try_me_jmp takes the given buffer and put code in it to do
 *	a try_me_else followed by a jump to the clause.
 */

void
ic_install_try_me_jmp(n,clausestart,nextclause)
   ntbl_entry *n;
   Code *clausestart;
   PWord nextclause;
{
   Code *oldptr = ic_ptr;

   ic_install_exception_check(n);
#ifdef MacOS
		/* NOTE:  At 11:55 PM even the best of C programmers may forget that
   		   an integer beginning with 0 is really treated as an OCTAL number
   		   and so spending an hour trying to figure out how the decimal 
   		   44340 comes close to representing the movem.l instruction as a 
   		   result is entirely possible.   AAAAARRRGGGGHHHH!  Why use octal, 
   		   anyway?  As you've noticed, those numbers which I needed to change
   		   for the Mac's sake are passed as hex.  Sorry for the inconsistency,
   		   feel free to translate the hex to octal, if your heart is set on it.
   		   We need to use 0x690 instead of 0x630 because I'm storing the Fail
   		   addr (A2 on other M68000 systems) in memory and loading it into
   		   a0 before the movem.l.  So, a0 must take a2's spot on the movem list,
   		   and hence the difference between the Mac and other systems...
		*/
   MOVAbs((long) &Fail, 0, ADIRECT, 0)
   ic_put(044340 | TR);     /* movem.l */
   ic_put(0x690);               /* HB, SPB, Fail (a0), B */
   MOVAddr2Addr((long) nextclause, (long) &Fail);
#else
   ic_put(044340 | TR);		/* movem.l */
   ic_put(003060);			/* HB, SPB, Fail, B */
   MOVI(nextclause,Fail,ADIRECT,0)
#endif
   MOVE(H,ADIRECT,0,HB,DDIRECT,0)
   MOVE(SP,ADIRECT,0,SPB,DDIRECT,0)
   MOVE(TR,ADIRECT,0,B,ADIRECT,0)
   JMP((long)clausestart)

   ic_ptr = oldptr;
}

/*
 * ic_install_switch_on_term is used to install the switch on term code in
 *	the given buffer.  If any of straddr, lisaddr, and/or conaddr is
 *	equal to varaddr then code will be emitted to take these through
 *	the try_me_else code emitted for varaddr.  If any of these are zero,
 *	then appropriate code will be emitted for failure.
 *
 * Code Summary:
 *	Size		Code
 *	----		--------
 *	 IEC_END*2	exception_check code
 *	 4			move.l	4(SP), D0
 *	 2			moveq	#3, D1
 *	 2			move.l	D0, A1
 *	 2			and.l	D1, D0
 *	 2			beq.s	3f
 *	 2		1:	subql	#2, D0
 *
 *	Subtotal 1: IEC_END*2+14 bytes = IEC_END+7 words required for the above
 *
 *	If the list address to jump to is not fail, the
 *	following code is emitted:
 *	 6			beq.l	lisaddroffset
 *	Else nothing is emitted for the list address, but a jump to wm_fail
 *	will need to be emitted later on.
 *
 *	
 *	If the structure address to jump to is not fail, the
 *	following code is emitted:
 *	 6			blo.l	straddroffset
 *	Else nothing is emitted for the list address, but a jump to wm_fail
 *	will be emitted later on
 *
 *	If the constant address to jump to is not fail, the
 *	following code is emitted:
 *	 6			bhi.l	conaddroffset
 *	Else nothing is emitted for the constant address.
 *
 *	If in any of the three above cases, one or more of the beq, blo, or
 *	bhi was not emitted, then the following code is emitted:
 *	 2			move.l	(B), A0
 *	 2			jmp	(A0)
 *
 *	Subtotal 2: 	At most 18 bytes = 9 words are required for the above 
 *			branches.
 *	
 *
 *	 2		3:	move.l	(A1), d0
 *	 2			cmp.l	A1, D0
 *	 6			beq.l	varaddroffset
 *	 2			move.l	D0, A1
 *	 2			and.l	D1, D0
 *	 2			beq.s	3b
 *	 4			move.l	A1, 4(SP)
 *	 2			bra.s	1b
 *
 *	Subtotal 3:	22 bytes = 11 words required
 *
 *	At most IEC_END*2+54 bytes = IEC_END+27 words are required for the
 *	switch code.
 *
 */ 

void
ic_install_switch_on_term(n,varaddr,straddr,lisaddr,conaddr,emask)
    ntbl_entry *n;
    Code *varaddr, *straddr, *lisaddr, *conaddr;
    int emask;
{
    Code *oldptr = ic_ptr;
    Code *p1, *p3;
    int jfail;
    int temp;

    ic_install_exception_check(n);

    MOVE(SP,DISPL,8,D0,DDIRECT,0)
    MOVEQ(3,D1)
    MOVE(D0,DDIRECT,0,S,ADIRECT,0)
    ANDDD(D1,D0)
    BEQ(0) LABEL(p3)

    LABEL(p1)

    jfail = 0;
    SUBQW(2,D0,DDIRECT,0)
    if (lisaddr == 0)
        jfail = 1;
    else {
        BEQ(0xff)
        temp = ((int) lisaddr) - ((int) ic_ptr);
        ic_putl(temp);
    }

    if (straddr == 0) 
        jfail = 1;
    else {
        BLO(0xff)
        temp = ((int) straddr) - ((int) ic_ptr);
        ic_putl(temp);
    }

    if (conaddr == 0) 
        jfail = 1;
    else {
        BHI(0xff)
        temp = ((int) conaddr) - ((int) ic_ptr);
        ic_putl(temp);
    }

    if (jfail) DoFail


    PATCHDISP(p3)
    LABEL(p3)

    MOVE(S,INDIRECT,0,D0,DDIRECT,0)
    CMPAD(S,D0)
    BEQ(0xff)
    temp = ((int) varaddr) - ((int) ic_ptr);
    ic_putl(temp);
    MOVE(D0,DDIRECT,0,S,ADIRECT,0)
    ANDDD(D1,D0)
    temp = LDISP(p3);
    BEQ(temp)
    MOVE(S,ADIRECT,0,SP,DISPL,8)
    temp = LDISP(p1);
    BRA(temp)

    ic_ptr = oldptr;
}

extern	void	wm_execute_builtin	PARAMS(( void ));
void
ic_install_builtin(n,builtin)
   ntbl_entry *n;
   int (*builtin) PARAMS(( void ));
{
   Code *oldptr = ic_ptr;

   ic_install_exception_check(n);
   MOVI((int) builtin,A0,ADIRECT,0)
   JMP(wm_execute_builtin)

   ic_ptr = oldptr;
}

void
ic_install_true(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;

    ic_install_exception_check(n);
    UNLK(E)
    RTS

    ic_ptr = oldptr;
}

void
ic_install_fail(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;

    ic_install_exception_check(n);
    DoFail

    ic_ptr = oldptr;
}

void
ic_install_equal(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;

    ic_install_exception_check(n);
    UNLK(E)
    MOVE(SP,DISPL,4,A0,ADIRECT,0)
    MOVE(SP,DISPL,8,D0,DDIRECT,0)
    JMP(UNIFY)

    ic_ptr = oldptr;
}

/*
 * ic_install_call is used to install call/1.
 */

void
ic_install_call(n, whereto)
    ntbl_entry *n;
    long *whereto;
{
    int offset;
    Code *oldptr = ic_ptr;

    ic_install_exception_check(n);
    CLRD(D0)
    ic_put(030072);			/* move.w  (PC,disp), D0	*/
    offset = (((char *) (&n->modid)) - ((char *) ic_ptr));
    ic_put(offset);
    LSLND(4,D0)
    ADDQ(7,D0, DDIRECT, 0)
    JMP(whereto)

    ic_ptr = oldptr;
}



/*
 * ic_install_module_closure installs code which gets the module id (a
 *	symbol) of the current proceduure and installs this symbol as
 *	the first argument.
 */


void
ic_install_module_closure(n,whereto)
    ntbl_entry *n;
    Code *whereto;
{
    int offset;
    Code *oldptr = ic_ptr;

    ic_install_exception_check(n);
    CLRD(D0)
    ic_put(030072);			/* move.w  (PC,disp), D0	*/
    offset = (((char *) (&n->modid)) - ((char *) ic_ptr));
    ic_put(offset);
    MOVE(SP, INDIRECT, 0, SP, PREDECR, 0)
    MOVE(SP, DISPL, 8, SP, DISPL, 4)
    LSLND(4,D0)
    ADDQ(7,D0, DDIRECT, 0)
    MOVE(D0, DDIRECT, 0, SP, DISPL, 8)
    MOVE(SP, ADIRECT, 0, E, ADIRECT, 0)
    JMP((long)whereto)

    ic_ptr = oldptr;
}


extern	void	wm_nciadc	PARAMS(( void ));

void
ic_install_next_choice_in_a_deleted_clause(buf)
    Code *buf;
{
    Code *oldptr = ic_ptr;
    ic_ptr = buf;
    ic_put(040772);			/* lea (disp,PC), A0 	*/
    ic_put(-2);				/*      ^^^^		*/
    MOVE(A0,ADIRECT,0,SP,PREDECR,0)
    JMP(wm_nciadc)
    ic_ptr = oldptr;
}

/*
 * ic_install_try_me
 *
 *	Size		Code
 *	----		----
 *	  2			nop
 *	  2			nop
 *	  4		entry:	movem.l	#<HB,SPB,Fail,B>, -(TR)
 *	  6			move.l	#nextclause, Fail
 *	  2			move.l	H, HB
 *	  2			move.l	SP, SPB
 *	  2			move.l	TR, B
 *	  2			bra.s	1f
 *	  2			nop
 *	  2			nop
 *	  2			nop
 *			1:	
 *	----
 *	 28
 */

void
ic_install_try_me(buf, nextclause, emask)
    Code *buf;
    PWord nextclause;
    int emask;
{
    Code *l;
    Code *oldptr = ic_ptr;

    ic_ptr = buf;

    NOP
    NOP
#ifdef MacOS
    NOP
    MOVAbs((long) &Fail,0,ADIRECT,0)
    ic_put(044340 | TR);
    ic_put(0x690);
    MOVAddr2Addr((long) nextclause , (long) &Fail);
#else
    ic_put(044340 | TR);		/* movem.l SRC, -(TR) */
    ic_put(003060);			/* SRC = <HB, SPB, Fail, B> */
    MOVI(nextclause,Fail,ADIRECT,0)
#endif
    MOVE(H,ADIRECT,0,HB,DDIRECT,0)
    MOVE(SP,ADIRECT,0,SPB,DDIRECT,0)
    MOVE(TR,ADIRECT,0,B,ADIRECT,0)
    BRA(0) LABEL(l)
    NOP
    NOP
    NOP
    PATCHDISP(l)

    ic_ptr = oldptr;
}


/*
 * ic_install_retry_me
 *
 *	Size		Code
 *	----		----
 *	  2			nop
 *	  2			nop
 *	  2		entry:	cmp.l	B, TR
 *	  2			bhs.s	2f
 *	  2		1:	move.l	(TR)+, A0
 *	  2			move.l	A0, (A0)
 *	  2			cmp.l	B, TR
 *	  2			blo.s	1b
 *	  2		2:	move.l	HB, H
 *	  2			move.l	SPB, SP
 *	  2			move.l	SPB, E
 *	  6			move.l	#nextclause, Fail
 *	----
 *	 28
 */

void
ic_install_retry_me(buf,nextclause,nargs,emask)
    Code *buf;
    PWord nextclause;
    int nargs;
    int emask;
{
    Code *l1,*l2;
    int ldisp;
    Code *oldptr = ic_ptr;

    ic_ptr = buf;

    NOP
    NOP
#ifdef MacOS
    NOP
    NOP
    NOP
    NOP
#endif
    CMPA(B,ADIRECT,0,TR)
    BHS(0)
    LABEL(l2)
    LABEL(l1)
    MOVE(TR,POSTINCR,0,A0,ADIRECT,0)
    MOVE(A0,ADIRECT,0,A0,INDIRECT,0)
    CMPA(B,ADIRECT,0,TR)
    ldisp = LDISP(l1);
    BLO(ldisp)
    PATCHDISP(l2)
    MOVE(HB,DDIRECT,0,H,ADIRECT,0)
    MOVE(SPB,DDIRECT,0,SP,ADIRECT,0)
    MOVE(SPB,DDIRECT,0,E,ADIRECT,0)
#ifdef MacOS
    MOVAddr2Addr((long) nextclause, (long) &Fail)
#else
    MOVI(nextclause,Fail,ADIRECT,0)
#endif

    ic_ptr=oldptr;
}

/*
 * ic_install_trust_me
 *
 *	Size		Code
 *	----		----
 *	  2		1:	move.l	(TR)+, A0
 *	  2			move.l	A0, (A0)
 *	  2		entry:	cmp.l	B, TR
 *	  2			blo.s	1b
 *	  2			move.l	HB, H
 *	  2			move.l	SPB, SP
 *	  2			move.l	SPB, E
 *	  2			move.l	(TR)+, HB
 *	  2			move.l	(TR)+, SPB
 *	  2			move.l	(TR)+, Fail
 *	  4			and.w	0xfffc, SPB
 *	  2			move.l	(TR)+, B
 *	  2			bra.s	entrypoint
 *	----
 *	 28
 */

void
ic_install_trust_me(buf, entrypoint, nargs, emask)
    Code *buf;
    PWord entrypoint;
    int nargs, emask;
{
    Code *l;
    int ldisp;
    Code *oldptr = ic_ptr;

    ic_ptr = buf;

    LABEL(l)
    MOVE(TR,POSTINCR,0,A0,ADIRECT,0)
    MOVE(A0,ADIRECT,0,A0,INDIRECT,0)
    CMPA(B,ADIRECT,0,TR)
    ldisp = LDISP(l);
    BLO(ldisp)
    MOVE(HB,DDIRECT,0,H,ADIRECT,0)
    MOVE(SPB,DDIRECT,0,SP,ADIRECT,0)
    MOVE(SPB,DDIRECT,0,E,ADIRECT,0)
    MOVE(TR,POSTINCR,0,HB,DDIRECT,0)
    MOVE(TR,POSTINCR,0,SPB,DDIRECT,0)
#ifdef MacOS
	MOVReg2Addr(TR,POSTINCR,(long) &Fail)
#else
    MOVE(TR,POSTINCR,0,Fail,ADIRECT,0)
#endif
    ANDIW(0xfffc,SPB,DDIRECT,0)
    MOVE(TR,POSTINCR,0,B,ADIRECT,0)
    ldisp = LDISP((Code *)entrypoint);
    if (ldisp == 0) {
        MOVE(D0,DDIRECT,0,D0,DDIRECT,0)
    }
    else if (ldisp < 0 || ldisp > 255) {
        fprintf(stderr,
		"ic_install_trust : Internal Error -- branch too far.\n");
        als_exit(1);
    }
    else {
        BRA(ldisp)
    }

    ic_ptr = oldptr;
}

/*
 * ic_install_no is to install both the no part for queries and the little
 *	piece of code which from which execution will start from.  The address
 *	of the place to start is returned as the value from ic_install_no
 */

extern	void	wm_try_me	PARAMS(( void ));
extern	void	wm_retry_me	PARAMS(( void ));

Code *
ic_install_no(buf,clausestart,nocatcher)
    Code *buf;
    Code *clausestart;
    char *nocatcher;
{
    Code *startaddr;
    Code *oldptr = ic_ptr;

    ic_ptr = buf;


    JSR((int) wm_retry_me)

    JMP(w_nameentry(MODULE_BUILTINS,find_token((UCHAR *)nocatcher),0)->exec_entry)
    startaddr = ic_ptr;
    LINK(E,0)
    ORIW(1,SPB,DDIRECT,0)	/* set it up so first chpt is compacted. */
    JSR((int) wm_try_me)
#ifdef MacOS
    MOVAddr2Addr((int) buf, (long) &Fail)
#else
    MOVI(((int) buf), Fail, ADIRECT, 0)
#endif
    SUBQ(8,SP,ADIRECT,0)
    MOVE(E,INDIRECT,0,SP,INDIRECT,0)
    MOVE(E,DISPL,4,SP,DISPL,4)
    MOVE(SP,ADIRECT,0,E,ADIRECT,0)
    JSRX((int) wm_try_me)
#ifdef MacOS
    MOVAddr2Addr((int) buf, (long) &Fail)
#else
    MOVI(((int) buf), Fail, ADIRECT, 0)
#endif
    JMP(clausestart);

    ic_ptr = oldptr;

    return startaddr;
}

/*
 * ic_install_reference is called by resolve_reference to install a jump
 *	to a non-builtin.
 */

void
ic_install_reference(ic,where)
    Code *ic;
    PWord where;
{
    Code *oldptr = ic_ptr;

    ic_ptr = ic;
    JMP(where)

    ic_ptr = oldptr;
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

extern	void	wm_try		PARAMS(( void ));

long *
ic_install_try(ptr, cstart, nargs)
    long *ptr;
    Code *cstart;
    int nargs;
{
    Code *oldptr = ic_ptr;
    ic_ptr = (Code *) ptr;

    NOP				/* 2 bytes */
    JSR(((int) wm_try))		/* 6 bytes */
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

extern	void	wm_retry	PARAMS(( void ));

long *
ic_install_retry(ptr, cstart, nargs, emask)
    long *ptr;
    Code *cstart;
    int nargs;
    int emask;
{
    Code *oldptr = ic_ptr;
    ic_ptr = (Code *) ptr;

    NOP
    JSR(((int) wm_retry))
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
 *	to be maintained on this return value.  The code has been contructed
 *	to facilitate this.
 */


extern	void	wm_trust	PARAMS(( void ));

long *
ic_install_trust(ptr, cstart, nargs, emask)
    long *ptr;
    Code *cstart;
    int nargs;
    int emask;
{
    Code *oldptr = ic_ptr;
    ic_ptr = (Code *) ptr;

    NOP
    JSR(((int) wm_trust))
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
 *	Note that ic_uptr is also a global variable which is usually referenced
 *	by the ic_put macros.  In this case, however, it refers to the
 *	local variable.
 *
 *	The following code is emitted for the 680x0:
 *
 *      Size            Instr
 *      ----            ---------
 *       4              lea     (PC,14), A0
 *       2              move.l  A0, D0
 *       4              lea     (PC,(8*N)), A0
 *       6              jmp     switch_on_const   (or switch_on_struct)
 *
 *      Total Size: 16 bytes = 4 long words
 *
 */
 
Code *
ic_install_tree_overhead(swaddr, nentries, ic)
    long *swaddr;
    int nentries;
    Code *ic;
{
    Code *oldptr;
    Code *tmp_ic_ptr;
	register ic_uptr_type ic_uptr;

	oldptr = ic;
	ic_uptr.code_ptr = ic;

    LEA_PCREL(A0)		/* lea (PC, 14), A0	*/
    ic_put(14);			/*          ^^		*/
    MOVE(A0,ADIRECT,0,D0,DDIRECT,0)
    LEA_PCREL(A0)		/* lea (PC, (8*N)), A0	*/
    ic_put(8*nentries);		/*          ^^^^^	*/
    JMP(swaddr)

	tmp_ic_ptr = ic_ptr;
	ic_ptr = oldptr;
    return tmp_ic_ptr;
}
