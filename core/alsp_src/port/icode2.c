/*
 * icode2.c                     -- emit more instructions
 *      Copyright (c) 1992 Applied Logic Systems, Inc.
 *
 * Author: Prabhakaran Raman
 * Creation: 8/19/92
 * Revision History:
 */

#include "defs.h"

#include "coerce.h"
#include "wintcode.h"
#include "module.h"
#include "labels.h"
#include "icodegen.h"
#include "machinst.h"


#include "wamops.h"

/*
 *
 * ic_install_spy is passed the exec_entry field of a procedure
 *      table entry and installs the following code to do the spy check:
 *
 */

void
ic_install_spy(n)
    ntbl_entry *n;
{
    ic_punch(n->exec_entry, W_SPY);
}


/*
 * ic_install_libbreak is passed a procedure entry and an interrupt number.
 * The wm_interrupt_caught variable is set to the interrupt number and
 * a branch back to the overflow code is installed in the code area.
 *
 * It is then up to the interrupt handling code to determine what to do with
 * the interrupt.  The original purpose of this code is to handle the library
 * loading code.  It is parameterized, however, for other applications.
 */

void
ic_install_libbreak(n, i)
    ntbl_entry *n;
    int   i;
{
    Code *oldptr = ic_ptr;

    ic_ptr = n->code;

    ic_puti(W_LIBBREAK);
    ic_put(i);

    ic_puti(W_JUMP);
    ic_putl((PWord) n->exec_entry);

    ic_ptr = oldptr;
    ic_install_normal_exec_entry(n);
}


/*
 * ic_install_decr_icount
 */

void
ic_install_decr_icount(n)
    ntbl_entry *n;
{
    ic_punch(n->exec_entry, W_DECR_ICOUNT);
}


/*
 *
 * ic_install_normal_exec_entry is passed the exec_entry
 * field of a procedure table entry and installs the following
 * code to do the interrupt check:
 */

void
ic_install_normal_exec_entry(n)
    ntbl_entry *n;
{
    if (n->flags & NMSK_SPYSET)
	ic_punch(n->exec_entry, W_SPY);
    else if ((n->flags & NMSK_USAGE) == NFLG_SWITCH)
	ic_punch(n->exec_entry, W_OVSW_TERM);
    else
	ic_punch(n->exec_entry, W_OVFLOW_CHECK);
}


void
ic_install_call_entry(n)
    ntbl_entry *n;
{
    ic_punch(n->call_entry, W_NOP);
}


void
ic_install_next_choice_in_a_deleted_clause(buf)
    Code *buf;
{
    ic_punch(buf, W_NCIADC);
}


/*
 * ic_install_try_me_jmp
 */

void
ic_install_tmjmp(n, firstclause, secondclause)
    ntbl_entry *n;
    Code *firstclause;
    PWord secondclause;
{
    Code *oldptr = ic_ptr;

    ic_ptr = n->code;

    ic_put_align(W_TRY_ME_JUMP);
    ic_putl(secondclause);
    ic_putl((PWord)firstclause);

    ic_ptr = oldptr;
    if (n->flags & NMSK_SPYSET)
	ic_punch(n->exec_entry, W_SPY);
    else
	ic_punch(n->exec_entry, W_OVFLOW_CHECK);
}

/*
 * ic_install_try_me
 *
 */

void
ic_install_try_me(buf, nextclause, nargs)
    Code *buf;
    PWord nextclause;
    int   nargs;
{
    Code *oldptr = ic_ptr;

    ic_ptr = buf;

    ic_put_align(W_TRY_ME);
    ic_putl(nextclause);

    ic_ptr = oldptr;
}

/*
 * ic_install_retry_me
 *
 */

void
ic_install_retry_me(buf, nextclause, nargs, emask)
    Code *buf;
    PWord nextclause;
    int   nargs;
    int   emask;
{
    Code *oldptr = ic_ptr;

    ic_ptr = buf;

    ic_put_align(W_RETRY_ME);
    ic_putl(nextclause);

    ic_ptr = oldptr;
}


/*
 * ic_install_trust_me
 *
 */

void
ic_install_trust_me(buf, whereto, nargs, emask)
    Code *buf;
    PWord whereto;
    int  nargs, emask;
{
    Code *oldptr = ic_ptr;

    ic_ptr = buf;

    ic_put_align(W_TRUST_ME);
    ic_putl(whereto);

    ic_ptr = oldptr;
}

/* FIXME: Delete the following if it is no longer being used */
#if 0
/*
 * icIndexPatch: Put down a choice point instruction in an index patch.
 */

Code *
icIndexPatch(place, choiceInstr, where)
    Code *place;		/* Where this code is to go */
    Code *choiceInstr;		/* The index patch choice point instruction */
    Code *where;		/* Where to go after the choice instruction */
{
    long *longptr;

    *place++ = (Code) choiceInstr;
    longptr = (long *) (((char *) place) + sizeof (long) - sizeof (Code));
    *longptr++ = (long) where;

    return (Code *) longptr;
}
#endif

/*
 * ic_install_switch_on_term is used to install the switch on term code in
 *      the given buffer.  If any of straddr, lisaddr, and/or conaddr is
 *      equal to varaddr then code will be emitted to take these through
 *      the try_me_else code emitted for varaddr.  If any of these are zero,
 *      then appropriate code will be emitted for failure.
 */

/*//extern Code *wm_fail;*/

void
ic_install_switch_on_term(nameEntry, varaddr, straddr, lisaddr, conaddr, emask)
    ntbl_entry *nameEntry;
    Code *varaddr, *straddr, *lisaddr, *conaddr;
    int emask;	/* not used */
{
    ic_ptr = nameEntry->code;

    ic_puti(W_SW_TERM);

#define RESOLVE(u) ((u) == (Code *)0 ? wm_fail : (u))

    ic_putl((PWord)varaddr);
    ic_putl((PWord)RESOLVE(straddr));
    if (lisaddr != (Code *) 0 && lisaddr != (Code *) -1 
			      && *lisaddr == abinst(W_G_LIST_E_p2))
	ic_putl((PWord)(lisaddr+1));	/* don't need to do the get list */
    else
	ic_putl((PWord)RESOLVE(lisaddr));
    ic_putl((PWord)RESOLVE(conaddr));

#undef RESOLVE

    if (!(nameEntry->flags & NMSK_SPYSET))
	ic_punch(nameEntry->exec_entry, W_OVSW_TERM);
}

/*
 * ic_install_no is to install both the no part for queries and
 * the little piece of code which from which execution will
 * start from. The address of the place to start is returned
 * as the value from ic_install_no
 */

/*//extern Code *wm_return_success;*/

Code *
ic_install_no(buf, clausestart, nocatcher)
    Code *buf;
    Code *clausestart;
    const char *nocatcher;
{
    Code *oldptr = ic_ptr;
    Code *startaddr;

    ic_ptr = buf;

    ic_put_align(W_TRUST_ME);
    ic_putl((PWord) w_nameentry((PWord) MODULE_BUILTINS,
			        (PWord) find_token((UCHAR *)nocatcher), 0)->exec_entry);

    startaddr = ic_ptr;

    ic_puti(W_WAM_START1);
    ic_putl((PWord) wm_return_success);

    ic_put_align(W_TRY_ME);
    ic_putl((PWord)buf);

    ic_put_align(W_TRY_ME);	/* choice pt for cutting away */
    ic_putl((PWord)buf);

    ic_puti(W_WAM_START2);

    ic_puti(W_JUMP);
    ic_putl((PWord)clausestart);

    ic_ptr = oldptr;

    return startaddr;
}


void
ic_install_builtin(ne, builtin)
    ntbl_entry *ne;
    int   (*builtin) ( void );
{
    Code *oldptr = ic_ptr;

    ic_ptr = ne->code;

    ic_puti(W_FOREIGN_JUMP);
    ic_putl((PWord)builtin);

    ic_ptr = oldptr;
    if (!(ne->flags & NMSK_SPYSET))
	ic_punch(ne->exec_entry, W_OVFLOW_CHECK);
}



/* This is used most of the time to install builtins */

void
ic_install_jmp(n, whereto, emask)
    ntbl_entry *n;
    Code *whereto;
    int   emask;
{
    Code *oldptr = ic_ptr;

    ic_ptr = n->code;

    ic_puti(W_JUMP);
    ic_putl((PWord)whereto);

    ic_ptr = oldptr;
    if (!(n->flags & NMSK_SPYSET))
	ic_punch(n->exec_entry, W_OVJUMP);
}

/*
 * This is used to (surprise!) install resolve_refs.
 *
 * If the size of this code changes, the constant SIZERESOLVECODE
 * in wntbl.m4 must be changed to reflect the new size. The size
 * is in Code words.
 */

void
ic_install_resolve_ref(n)
    ntbl_entry *n;
{
    ic_punch(n->code, W_RESOLVE_REF);
    if (!(n->flags & NMSK_SPYSET))
	ic_punch(n->exec_entry, W_OVFLOW_CHECK);
}

/*
 * ic_install_fail is used to install code which will fail into a 
   procedure entry.
 * This is useful for establishing a defined procedure with no clauses.
 */

void
ic_install_fail(n)
    ntbl_entry *n;
{
	Code *old_ptr = ic_ptr;
    ic_ptr = (n->code);

	ic_puti(W_FAIL);

    if (!(n->flags & NMSK_SPYSET))
		ic_punch(n->exec_entry, W_OVFLOW_CHECK);

	ic_ptr = old_ptr;
}


/*
 * ic_install_reference is called by resolve_reference to install a jump
 *      to a non-builtin.
 */

void
ic_install_reference(buf, whereto)
    Code *buf;
    PWord whereto;
{
    Code *oldptr = ic_ptr;

    ic_ptr = buf;

    if (abinst(W_OVFLOW_CHECK) == buf[-1])
		ic_punch(buf-1,W_OVJUMP);
    ic_puti(W_JUMP);
    ic_putl(whereto);

    ic_ptr = oldptr;
}

/* ic_install_builtin_ref is used to implement (,) (->) and (;)
 */

void
ic_install_bref(ent, name, arity)
    ntbl_entry *ent;
    PWord name;
    PWord arity;
{
    ntbl_entry *dst;

    dst = w_nametable[w_namelookup((PWord) MODULE_BUILTINS,
				   (PWord) find_token((UCHAR *)name),
				   (int) arity)];

    ic_install_reference(ent->code, (PWord) dst->code);

}


void
ic_install_instr(ent, instr, dummy)
    ntbl_entry *ent;
    PWord instr;
    PWord dummy;
{
    ic_punch(ent->code, (Code) instr);
}

/*
 * ic_install_overflow_call is used to initialize the overflow field in a
 *      procedure table entry (ntbl_entry)
 *
 *      Takes up 3 words.
 */

void
ic_install_overflow_call(n)
    ntbl_entry *n;
{
    ic_punch(n->overflow, W_NOP);
}

/*
 * ic_install_module_closure installs code which gets the module id (an
 *      integer) of the current procedure and installs this integer as
 *      the first argument.
 *
 *
 *  Size: 12 bytes
 *
 * If the size of the code in here changes, SIZEMODCODE in wntbl.m4 must be
 * changed to reflect the new size.
 */

void
ic_install_module_closure(ent, whereto)
    ntbl_entry *ent;
    Code *whereto;
{
    Code *oldptr = ic_ptr;

    ic_ptr = ent->code;

    ic_puti(W_MOD_CLOSURE);
    ic_putl((PWord)whereto);

    ic_ptr = oldptr;
}


void
ic_install_call(ent, whereto)
    ntbl_entry *ent;
    long *whereto;
{
}

void
ic_install_equal(ent)
    ntbl_entry *ent;
{
}

void
ic_install_true(ent)
    ntbl_entry *ent;
{
}

/*
 * ic_install_try
 *
 *	This procedure will install a try sequence for the indexer.
 *
 *	ptr	is the address to start installing the try sequence at
 *	cstart	is the address to jump to after the choice point is created
 *	nargs	is the number of arguments in the clause
 *
 *	The next free address is returned as the value of the function.
 *
 */


long *
ic_install_try(ptr, cstart, nargs)
    long *ptr;
    Code *cstart;
    int nargs;
{
    ic_ptr = ptr;
    ic_put_align(W_TRY);
    ic_putl((PWord)cstart);
    return (long *) ic_ptr;
}


/*
 * ic_install_retry
 *
 *	This procedure will install a retry sequence for the indexer.
 *
 *	ptr	is the address to start installing the retry sequence at
 *	cstart	is the address to jump to after the choice point is created
 *	nargs	is the number of arguments in the clause
 *
 *	The next free address is returned as the value of the function.
 *
 */


long *
ic_install_retry(ptr, cstart, nargs, emask)
    long *ptr;
    Code *cstart;
    int nargs;
    int emask;
{
    ic_ptr = (Code *)ptr;
    ic_put_align(W_RETRY);
    ic_putl((PWord)cstart);
    return (long *) ic_ptr;
}


/*
 * ic_install_trust
 *
 *	This procedure will install a trust sequence for the indexer.
 *
 *	ptr	is the address to start installing the trust sequence at
 *	cstart	is the address to jump to after the choice point is created
 *	nargs	is the number of arguments in the clause
 *
 *	The next free address is returned as the value of the function.
 *
 */


long *
ic_install_trust(ptr, cstart, nargs, emask)
    long *ptr;
    Code *cstart;
    int nargs;
    int emask;
{
    ic_ptr = (Code *)ptr;
    ic_put_align(W_TRUST);
    ic_putl((PWord) cstart);
    return (long *) ic_ptr;
}

/*
 * ic_install_tree_overhead
 *
 *	Installs switch_on_constant or switch_on_structure instructions for
 *	the indexer.
 *
 *	swinst	is the instruction to install
 *	nentries are the number of entries which we are concerned with
 *	ptr	is the place to start installing the sequence at
 */

Code *
ic_install_tree_overhead(swinst,nentries,ptr)
    long * swinst;
    int nentries;
    Code *ptr;
{
    ic_ptr = ptr;

    ic_put_align((Code) swinst);
    ic_putl((PWord)nentries);

    return ic_ptr;
}
