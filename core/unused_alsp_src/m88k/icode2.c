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
 *	Revised: mm/dd/yy	Who		-- Reason
 */

#include "defs.h"
#include "module.h"
#include "wintcode.h"
#include "compile.h"
#include "icodegen.h"
#include "codegen.h"
#include "machinst.h"
#include "rinfo.h"
#include "pckgcoff.h"

extern	void	wm_overflow0	PARAMS(( void ));
extern	void	wm_overflow1	PARAMS(( void ));
extern	void	wm_overflow2	PARAMS(( void ));
extern	void	wm_overflow3	PARAMS(( void ));

static long * ovtab[] = {
   (long *) wm_overflow0,
   (long *) wm_overflow1,
   (long *) wm_overflow2,
   (long *) wm_overflow3
};


/*
 * ic_install_overflow_call is used to initialize the overflow field in a
 *	procedure table entry (ntbl_entry)
 *
 * 	This portion of the name entry is assumed to have the following
 *	structure:
 *	
 *	bsr	overflow
 *	br	exec_entry
 *
 * It is assumed that the call entry immediately follows this entry and
 * so the following two instructions are also stored:
 *	addu	CP, RET, #gc_info_size
 *	addu	E, SP, ZERO
 */

void
ic_install_overflow_call(n)
    ntbl_entry *n;
{
    int disp;
    Code *oldptr = ic_ptr;

    ic_ptr = n->overflow;

    disp = ovtab[(n->nargs > NAREGS) ? NAREGS : n->nargs] - ic_ptr;
    bsr(disp);
    disp = n->exec_entry - ic_ptr;
    br(disp);
    addui(CP_REG,RET_REG,GC_INFO_SIZE);
    addu(E_REG, SP_REG, ZERO_REG);

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
 * ic_install_normal_exec_entry is used to install the overflow checking
 * code in the execute entry slot for a procedure entry.
 */

void
ic_install_normal_exec_entry(n)
    ntbl_entry *n;
{
    int disp;
    Code *oldptr = ic_ptr;
    ic_ptr = n->exec_entry;

    subu(TMP1_REG, TR_REG, H_REG);
    cmp(TMP1_REG,TMP1_REG, OV_REG);
    disp = n->overflow - ic_ptr;
    bb1(LO, TMP1_REG, disp);

#ifndef HAVE_MMAP
    tbnd(STKBOT_REG, SP_REG);
#endif /* HAVE_MMAP */

    ic_ptr = oldptr;
}


/*
 * ic_install_spy is given a name table entry pointer and a bsr to the spy
 * 	checking code is installed in the space reserved for the execute entry.
 *	Note that this code takes up exactly the same amount of space as the
 *	normal execute entry.  Also note that the overflow entry is placed
 *	in UARG1.  dbg_spycheck is exepected to jump to this point when
 *	either an overflow has occurred or when we leave due to a spy point.
 *
 *	Code:
 *		subu	TMP1, TR, H
 *		or	UARG1, ZERO, overflow(low half)
 *		bsr.n	dbg_spycheck
 *		or.u	UARG1, UARG1, overflow(high half)
 *		tbnd	StkBot, SP
 *
 *
 *
 */

extern	void	dbg_spycheck	PARAMS(( void ));

void
ic_install_spy(n)
    ntbl_entry *n;
{
    int disp;
    Code *oldptr = ic_ptr;

    ic_ptr = n->exec_entry;

#ifndef HAVE_MMAP
    tbnd(STKBOT_REG, SP_REG);
#endif /* HAVE_MMAP */
    ori(UARG1_REG, ZERO_REG, (((long) n->overflow) & 0xffff));
    disp = ((long *) dbg_spycheck) - ic_ptr;
    bsrn(disp);
    oru(UARG1_REG, UARG1_REG, ((((long) n->overflow) >> 16) & 0xffff));

    ic_ptr = oldptr;
}


/*
 * ic_install_libbreak is passed a procedure entry and an interrupt number.
 * The wm_interrupt_caught variable is set to the interrupt number and a
 * branch back to the overflow code is installed in the code area.
 *
 * It is then up to the interrupt handling code to determine what to do with
 * the interrupt.  The original purpose of this code is to handle the library
 * loading code.  It is parameterized, however, for other applications.
 */

void
ic_install_libbreak(n,i)
    ntbl_entry *n;
    int i;
{
    int disp;
    Code * oldptr = ic_ptr;
    ic_ptr = n->code;

    move_const((unsigned long)i,UARG1_REG);
    oru(TMP1_REG, ZERO_REG, ((((long) &wm_interrupt_caught) >> 16) & 0xffff));
    sti(UARG1_REG,TMP1_REG, (((long) &wm_interrupt_caught) & 0xffff));
    disp = n->overflow - ic_ptr;
    brn(disp);
    subui(OV_REG, ZERO_REG, 1);

    ic_ptr = oldptr;
}



/*
 * ic_install_decr_icount is passed the codeg field of a procedure entry and a
 *	bsr to dbg_decr_icount is installed.  This procedure works similar
 *	to dbg_spycheck.
 *
 */

extern	void	dbg_decr_icount	PARAMS(( void ));

void
ic_install_decr_icount(n)
    ntbl_entry *n;
{
    int disp;
    Code *oldptr = ic_ptr;

    ic_ptr = n->exec_entry;
 
#ifndef HAVE_MMAP
    tbnd(STKBOT_REG, SP_REG);
#endif
    ori(UARG1_REG, ZERO_REG, (((long) n->overflow) & 0xffff));
    disp = ((long *) dbg_decr_icount) - ic_ptr;
    bsrn(disp);
    oru(UARG1_REG, UARG1_REG, ((((long) n->overflow) >> 16) & 0xffff));

    ic_ptr = oldptr;
}




/*
 * ic_install_resolve_ref takes the given buffer and puts code in the buffer
 *	to resolve the (undefined) reference.
 *
 */

extern	void	wm_resolve_ref0	PARAMS(( void ));
extern	void	wm_resolve_ref1	PARAMS(( void ));
extern	void	wm_resolve_ref2	PARAMS(( void ));
extern	void	wm_resolve_ref3 PARAMS(( void ));

static long *rrtab[] = {
	(long *) wm_resolve_ref0,
	(long *) wm_resolve_ref1,
	(long *) wm_resolve_ref2,
	(long *) wm_resolve_ref3
};


void
ic_install_resolve_ref(n)
   ntbl_entry *n;
{
   int disp;
   Code *oldptr = ic_ptr;


   ic_ptr = n->code;

   /*
    * Put down the subroutine branch to the appropriate version of
    * wm_resolve_ref.
    */

   disp = rrtab[(n->nargs > NAREGS) ? NAREGS : n->nargs] - ic_ptr;
   bsrn(disp);

   /*
    * We want to set r1 to point at the procedure entry.  The following code
    * which goes into the branch delay slot will take care of this.
    */

   disp = (int) (char *) (((ntbl_entry *) 0)->code+2);
   subui(RET_REG, RET_REG, disp);

   ic_ptr = oldptr;
}


/*
 * ic_install_jmp takes the given buffer and puts code in the buffer
 *	to jump to the start of a the single clause.
 *
 * Code Summary:
 *	Code will differ depending on the value of emask.  The following is the
 *	most that will be produced.
 *
 *	st	OldE, E, 0+32K
 *	st	CP, E, 4+32K
 *	st	A1, E, 8+32K
 *	st	A2, E, 12+32K
 *	st	A3, E, 16+32K
 *	br	clausestart
 *	
 */

void
ic_install_jmp(n,clausestart,emask)
   ntbl_entry *n;
   Code *clausestart;
   int emask;
{
   Code *oldptr = ic_ptr;
   int displ;
   ic_ptr = n->code;
   if (emask & EMSK_OLDE) {
      sti(OldE_REG, E_REG, EBIAS+0);
   }
   if (emask & EMSK_CP) {
      sti(CP_REG, E_REG, EBIAS+4);
   }
   if (emask & EMSK_A1) {
      sti(A1_REG, E_REG, EBIAS+8);
   }
   if (emask & EMSK_A2) {
      sti(A2_REG, E_REG, EBIAS+12);
   }
   if (emask & EMSK_A3) {
      sti(A3_REG, E_REG, EBIAS+16);
   }
   
   displ = clausestart - (long *) ic_ptr;
   br(displ);
   ic_ptr = oldptr;
}




/*
 * wm_try0, wm_try1, wm_try2, and wm_try3 are entry points to the wm_tryX
 * subroutine.
 */

extern	void	wm_try0		PARAMS(( void ));
extern	void	wm_try1		PARAMS(( void ));
extern	void	wm_try2		PARAMS(( void ));
extern	void	wm_try3		PARAMS(( void ));
static long *trytab[] = {
	(long *) wm_try0,
	(long *) wm_try1,
	(long *) wm_try2,
	(long *) wm_try3
};

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
    int displ;
    ic_ptr = n->code;
    displ = trytab[(n->nargs > NAREGS) ? NAREGS : n->nargs] - ic_ptr;
    bsrn(displ);
    subui(TR_REG, TR_REG, 16);
    ori(FAIL_REG, ZERO_REG, nextclause&0xffff);
    displ = clausestart - ic_ptr;
    brn(displ);
    oru(FAIL_REG, FAIL_REG, (nextclause>>16)&0xffff);
    ic_ptr = oldptr;
}


/*
 * ic_install_switch_on_term is used to install the switch on term code in
 *	the given buffer.  If any of straddr, lisaddr, and/or conaddr is
 *	equal to varaddr then code will be emitted to take these through
 *	the try_me_else code emitted for varaddr.  If any of these are zero,
 *	then appropriate code will be emitted for failure.
 *
 * At most 13 long words are needed for the switch on term sequence. 
 *
 *
 */ 

void
ic_install_switch_on_term(n,varaddr,straddr,lisaddr,conaddr,emask)
    ntbl_entry *n;
    Code *varaddr, *straddr, *lisaddr, *conaddr;
    int emask;
{
    Code *oldptr = ic_ptr;
    long *p1,*p2,*p3;
    int displ;

    ic_ptr = n->code;

    if (emask & EMSK_OLDE) {
        sti(OldE_REG, E_REG, EBIAS+0);
    }
    if (emask & EMSK_CP) {
        sti(CP_REG, E_REG, EBIAS+4);
    }
    if (emask & EMSK_A1) {
        sti(A1_REG, E_REG, EBIAS+8);
    }
    if (emask & EMSK_A2) {
        sti(A2_REG, E_REG, EBIAS+12);
    }
    if (emask & EMSK_A3) {
        sti(A3_REG, E_REG, EBIAS+16);
    }

    p1 = ic_ptr;
    bb0(BREF, A1_REG, 0);

    p3 = ic_ptr;
    bb0n(BLIST,A1_REG,0);
    extui(S_REG, A1_REG, 26, 0);
    if (lisaddr) {
	displ = lisaddr - ic_ptr;
	br(displ);
    }
    else {
	jmp(FAIL_REG);
    }
    PATCHDISP(p3);

    p3 = ic_ptr;
    bb0(BSTRC, A1_REG, 0);
    if (straddr) {
	displ = straddr - ic_ptr;
	br(displ);
    }
    else {
	jmp(FAIL_REG);
    }
    PATCHDISP(p3);

    if (conaddr) {
        displ = conaddr - ic_ptr;
        br(displ);
    }
    else {
        jmp(FAIL_REG);
    }

    PATCHDISP(p1);

    ldi(TMP1_REG, A1_REG, EBIAS);
    cmp(A1_REG, TMP1_REG, A1_REG);
    displ = p1-ic_ptr;
    bb0n(EQ, A1_REG, displ);
    addui(A1_REG, TMP1_REG, 0);

    p2 = ic_ptr;
    displ = varaddr - ic_ptr;
    br(displ);

    ic_ptr = oldptr;
}


extern	void	wm_exec_builtin0	PARAMS(( void ));
extern	void	wm_exec_builtin1	PARAMS(( void ));
extern	void	wm_exec_builtin2	PARAMS(( void ));
extern	void	wm_exec_builtin3	PARAMS(( void ));

static long * ebtab[] = {
	(long *) wm_exec_builtin0,
	(long *) wm_exec_builtin1,
	(long *) wm_exec_builtin2,
	(long *) wm_exec_builtin3
};

void
ic_install_builtin(n,builtin)
    ntbl_entry *n;
    int (*builtin) PARAMS(( void ));
{
    Code *oldptr = ic_ptr;
    long displ;
    ic_ptr = n->code;
    ori(T1_REG, ZERO_REG, ((long) builtin) & 0xffff);
    displ = ebtab[(n->nargs > NAREGS) ? NAREGS : n->nargs] - ic_ptr;
    brn(displ);
    oru(T1_REG, T1_REG, (((long) builtin) >> 16) & 0xffff);
    ic_ptr = oldptr;
}


void
ic_install_true(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    ic_ptr = n->code;
    jmpn(CP_REG);
    add(E_REG, OldE_REG, ZERO_REG);
    ic_ptr = oldptr;
}


/*
 * ic_install_fail is used to install code in a procedure entry which fails.
 * This is useful for establishing a defined procedure with no clauses.
 */

void
ic_install_fail(n)
    ntbl_entry *n;
{
     Code *ic_ptr = n->code;
     jmp(FAIL_REG);
}


void
ic_install_equal(n)
    ntbl_entry *n;
{
    Code *oldptr = ic_ptr;
    int displ;
    ic_ptr = n->code;
    add(UARG1_REG, A1_REG, ZERO_REG);
    add(UARG2_REG, A2_REG, ZERO_REG);
    add(RET_REG, CP_REG, ZERO_REG);
    displ = ((long *) wm_unify) - ic_ptr;
    brn(displ);
    add(E_REG, OldE_REG, ZERO_REG);
    ic_ptr = oldptr;
}




/*
 * ic_install_call is used to install call/1 and other procedures which
 * 	need the module id of the caller.
 */

void
ic_install_call(n,whereto)
    ntbl_entry *n;
    long *whereto;
{
    int disp;
    Code *oldptr = ic_ptr;

    ic_ptr = n->code;

    bsrn(2);

    /*
     * We want to set T1 to point at the procedure entry.  The following code
     * which goes into the branch delay slot will take care of this.
     */

    disp = (int) (char *) (((ntbl_entry *) 0)->code+2);
    subui(T1_REG, RET_REG, disp);

    /*
     * Put down the subroutine branch. 
     */

    disp = whereto - ic_ptr;
    bsrn(disp);

    /*
     * put down the load which will put an unsigned module id into t1
     */
   
    ldhui(T1_REG, T1_REG, ((int) (char *) &(((ntbl_entry *) 0)->modid)));

    ic_ptr = oldptr;
}


/*
 * ic_install_module_closure installs code which gets the module token of 
 *	the current procedure, installs this token as the first argument
 *	and branches to whereto
 */

void
ic_install_module_closure(n,whereto)
    ntbl_entry *n;
    Code *whereto;
{
    int disp;
    Code *oldptr = ic_ptr;

    ic_ptr = n->code;

    bsrn(2);

    /*
     * We want to set T1 to point at the procedure entry.  The following code
     * which goes into the branch delay slot will take care of this.
     */

    disp = (int) (char *) (((ntbl_entry *) 0)->code+2);
    subui(T1_REG, RET_REG, disp);

    /*
     * put down the load which will put an unsigned module id into UArg1
     */
   
    ldhui(UARG1_REG, T1_REG, ((int) (char *) &(((ntbl_entry *) 0)->modid)));
 
    /*
     * Emit a store instruction if the third argument is in use
     */
   
    if (n->nargs >= 3) {
        sti(A3_REG, E_REG, EBIAS+16);
    }

    /*
     * Shift the other registers
     */
    
    addui(A3_REG, A2_REG, 0);
    addui(A2_REG, A1_REG, 0);

    /*
     * Tag the module id and store it into A1
     */
   
    oru(A1_REG, UARG1_REG, SYM_MASK);

    /*
     * Increase the size of the frame by one word.
     */

    subui(E_REG, E_REG, 4);
    addui(SP_REG, E_REG, 0);

    /*
     * Put down the branch to the place where we really want to go
     */

    disp = whereto - ic_ptr;
    br(disp);


    ic_ptr = oldptr;
}


extern	void	wm_nciadc	PARAMS(( void ));

void
ic_install_next_choice_in_a_deleted_clause(buf)
    Code *buf;
{
    int disp;
    Code *oldptr = ic_ptr;
    ic_ptr = buf;
    disp = ((long *) wm_nciadc) - ic_ptr;
    bsrn(disp);
    subui(A1_REG, RET_REG, 8);		/* get us back to the choice entry */
    ic_ptr = oldptr;
}


/*
 * ic_install_try
 *
 *	This procedure will install a try sequence for the indexer.
 *
 *	ptr	is the address to start installing the try sequence at
 *	cstart	is the address to jump to after the choice point is created
 *	nargs 	is the number of arguments in the clause
 *
 *	The next free address is returned as the value of the function
 */

long *
ic_install_try(ptr, cstart, nargs)
   long *ptr;
   Code *cstart;
   int nargs;
{
   int displ;
   Code *oldptr = ic_ptr;
   ic_ptr = ptr;

   if (nargs > NAREGS) nargs = NAREGS;
   displ = trytab[nargs] - ic_ptr;
   bsrn(displ);
   subui(TR_REG, TR_REG, 16);
   displ = cstart - ic_ptr;
   bsrn(displ);
   addui(FAIL_REG, RET_REG, 0);

   ptr = ic_ptr;
   ic_ptr = oldptr;
   return ptr;
}


/*
 * ic_install_try_me
 *
 */

void
ic_install_try_me(buf,nextclause,nargs)
   Code *buf;
   PWord nextclause;
   int	 nargs;
{
   Code *oldptr = ic_ptr;
   int displ;
   ic_ptr = buf;

   if (nargs > NAREGS) nargs = NAREGS;
   displ = trytab[nargs] - ic_ptr;
   bsrn(displ);
   subui(TR_REG, TR_REG, 16);
   ori(FAIL_REG, ZERO_REG, nextclause&0xffff);
   oru(FAIL_REG, FAIL_REG, (nextclause>>16)&0xffff);

   ic_ptr = oldptr;
}

extern	void	wm_retry_u0	PARAMS(( void ));
extern	void	wm_retry_u1	PARAMS(( void ));
extern	void	wm_retry_u2	PARAMS(( void ));
extern	void	wm_retry_u3	PARAMS(( void ));
static long *retry_u_tab[]= {
	(long *) wm_retry_u0,
	(long *) wm_retry_u1,
	(long *) wm_retry_u2,
	(long *) wm_retry_u3
};

extern	long	wm_retry0	PARAMS(( void ));
extern	void	wm_retry1	PARAMS(( void ));
extern	void	wm_retry2	PARAMS(( void ));
extern	void	wm_retry3	PARAMS(( void ));
static long *retry_tab[] = {
	(long *) wm_retry0,
	(long *) wm_retry1,
	(long *) wm_retry2,
	(long *) wm_retry3
};


/*
 * ic_install_retry_me
 *
 */

void
ic_install_retry_me(buf,nextclause,nargs,emask)
   Code *buf;
   PWord nextclause;
   int nargs;
   int emask;
{
   Code *oldptr = ic_ptr;
   int displ;

   ic_ptr = buf;
   if (nargs > NAREGS) nargs=NAREGS;
   if (emask & EMSK_CP)
      displ = retry_tab[nargs] - ic_ptr;
   else
      displ = retry_u_tab[nargs] - ic_ptr;
   
   bsrn(displ);
   add(E_REG, SPB_REG, ZERO_REG);
   ori(FAIL_REG, ZERO_REG, nextclause&0xffff);
   oru(FAIL_REG, FAIL_REG, (nextclause >> 16) & 0xffff);
   

   ic_ptr=oldptr;
}

/*
 * ic_install_retry
 *
 *	This function is called by the indexer to install a retry sequence.
 *
 *	ptr	is the place to start installing the retry at
 *	cstart 	is a pointer to the place to jump to after performing the 
 *		retry operation.
 *	nargs	is the number of arguments in the procedure.
 *	emask	is the mask from the clause indicating whether or not
 *		the cp and ce need to be restored.
 *
 *	The next free location is returned at the result of the function
 *
 *	
 */

long *ic_install_retry(ptr,cstart,nargs,emask)
   long *ptr;
   Code *cstart;
   int nargs;
   int emask;
{
   int displ;
   Code *oldptr = ic_ptr;

   ic_ptr = ptr;

   if (nargs > NAREGS) nargs = NAREGS;
   if (emask & EMSK_CP)
      displ = retry_tab[nargs] - ic_ptr;
   else
      displ = retry_u_tab[nargs] - ic_ptr;
   
   bsrn(displ);
   addui(E_REG, SPB_REG, 0);

   displ = cstart - ic_ptr;

   bsrn(displ);
   addui(FAIL_REG, RET_REG, 0);

   ptr = ic_ptr;
   ic_ptr = oldptr;
   return ptr;
}





extern	void	wm_trust_u0	PARAMS(( void ));
extern	void	wm_trust_u1	PARAMS(( void ));
extern	void	wm_trust_u2	PARAMS(( void ));
extern	void	wm_trust_u3	PARAMS(( void ));
extern	void	wm_trust0	PARAMS(( void ));
extern	void	wm_trust1	PARAMS(( void ));
extern	void	wm_trust2	PARAMS(( void ));
extern	void	wm_trust3	PARAMS(( void ));
static long *trust_u_tab[] = {
	(long *) wm_trust_u0,
	(long *) wm_trust_u1,
	(long *) wm_trust_u2,
	(long *) wm_trust_u3
};

static long *trust_tab[] = {
	(long *) wm_trust0,
	(long *) wm_trust1,
	(long *) wm_trust2,
	(long *) wm_trust3
};


/*
 * ic_install_trust_me
 *
 */

void
ic_install_trust_me(buf,clausestart,nargs,emask)
   Code *buf;
   PWord clausestart;
   int nargs, emask;
{
   Code *oldptr = ic_ptr;
   int displ;

   ic_ptr = buf;
   if (nargs >NAREGS) nargs = NAREGS;
   if (emask & EMSK_CP)
      displ = trust_tab[nargs] - ic_ptr;
   else
      displ = trust_u_tab[nargs] - ic_ptr;
   
   bsrn(displ);
   add(E_REG, SPB_REG, ZERO_REG);
   displ = (PWord *)clausestart - ic_ptr;
   brn(displ);
   addui(SP_REG, E_REG, 0);

   ic_ptr = oldptr;
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
 *	as the value of this function.
 */

long *
ic_install_trust(ptr, cstart, nargs, emask)
   long *ptr;
   Code *cstart;
   int nargs;
   int emask;
{
   int displ;
   Code *oldptr = ic_ptr;

   ic_ptr = ptr;

   if (nargs > NAREGS) nargs = NAREGS;
   if (emask & EMSK_CP)
      displ = trust_tab[nargs] - ic_ptr;
   else
      displ = trust_u_tab[nargs] - ic_ptr;
   
   bsrn(displ);
   addui(E_REG, SPB_REG, 0);
   displ = cstart - ic_ptr;
   brn(displ);
   addui(SP_REG, E_REG, 0);

   ptr = ic_ptr;
   ic_ptr = oldptr;
   return ptr;
}


/*
 * ic_install_tree_overhead
 *
 *	Installs switch_on_constant or switch_on_structure instructions for
 *	the indexer.
 *
 *	swaddr		is the address of the switch_on subroutine
 *	nentries	are the number of entries which we are concerned with
 *	ic_ptr		is the place to start installing the sequence at
 *
 *	Note that ic_ptr is also a global variable which is usually referenced
 *	by the ic_put macros.  In this case, however, it refers to the
 *	parameter.
 *
 *	The following code is emitted on for the 88000.
 *
 *	bsr	wm_sw_const	(or wm_sw_struct)
 *	number of entries in table * 8
 *
 *	wm_sw_const and wm_sw_struct are expected to use r1 to get the
 *	number of entries and use this value for doing a binary search of
 *	the table which will immediately follow the word containing the
 *	number of entries.
 */

Code *
ic_install_tree_overhead(swaddr, nentries, ic_ptr)
    long *swaddr;
    int nentries;
    Code *ic_ptr;
{
    int displ;
    displ = swaddr - ic_ptr;
    bsr(displ);
    ic_put(nentries*8);

    return ic_ptr;
}




/*
 * ic_install_no is to install both the no part for queries and the little
 *	piece of code which from which execution will start from.  The address
 *	of the place to start is returned as the value from ic_install_no
 */


Code *
ic_install_no(buf,clausestart,nocatcher)
   Code *buf;
   Code *clausestart;
   char *nocatcher;
{
   Code *oldptr = ic_ptr;
   long *startaddr;
   int displ;


   ic_ptr = buf;

   /*
    * Install a retry_me for the no catcher
    */
   
   displ = ((long *) wm_retry_u0) - ic_ptr;
   bsrn(displ);
   add(E_REG, SPB_REG, ZERO_REG);
   ori(FAIL_REG, ZERO_REG, ((long) buf) & 0xffff);
   oru(FAIL_REG, FAIL_REG, (((long) buf) >> 16) & 0xffff);

   /*
    * Run the no catcher
    */
   
   displ = 
      w_nameentry(MODULE_BUILTINS,find_token(nocatcher),0)->exec_entry - ic_ptr;
   brn(displ);
   add(E_REG, SP_REG, ZERO_REG);


   /*
    * This is where the query actually starts, so set startaddr.
    */
   
   startaddr = ic_ptr;


   /*
    * Allocate space on the stack for the CP and OldE.
    */
   
   subui(SP_REG, SP_REG, 8);
   add(E_REG, SP_REG, ZERO_REG);

   /*
    * Create a choice point with no arguments
    */
   
   seti(SPB_REG, SPB_REG, 1, 0);
   displ = ((long *) wm_try0) - ic_ptr;
   bsrn(displ);
   subui(TR_REG, TR_REG, 16);
   ori(FAIL_REG, ZERO_REG, ((long) buf) & 0xffff);
   oru(FAIL_REG, FAIL_REG, (((long) buf) >> 16) & 0xffff);

   /*
    * Now we should somehow mark the choice point as compacted
    */
   
   /*
    * allocate another space on the stack for CP and oldE
    */
   
   subui(SP_REG, SP_REG, 8);
   add(E_REG, SP_REG, ZERO_REG);

   /* 
    * Create another choice point which we will permit cut to be cut away.
    */
   
   displ = ((long *) wm_try0) - ic_ptr;
   bsrn(displ);
   subui(TR_REG, TR_REG, 16);
   ori(FAIL_REG, ZERO_REG, ((long) buf) & 0xffff);
   oru(FAIL_REG, FAIL_REG, (((long) buf) >> 16) & 0xffff);

   /*
    * Branch to the start of the query or command.
    */
   
   displ = clausestart - ic_ptr;
   br(displ);

   ic_ptr = oldptr;
   return startaddr;
}



/*
 * ic_install_reference is called by resolve_reference to install a jump
 *	to a non-builtin.
 */

void
ic_install_reference(buf,where)
   Code *buf;
   PWord where;
{
   Code *oldptr = ic_ptr;
   int disp;
   ic_ptr = buf;
   disp = ((Code *) where)-ic_ptr;
   br(disp);
   ic_ptr = oldptr;
}
