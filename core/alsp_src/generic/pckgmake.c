/*
 * pckgmake.c  -- make package
 *
 * Copyright (c) 1989-1993, Applied Logic Systems, Inc.
 *
 * Author : Ilyas Cicekli
 * Date   : 3/28/89
 */

#include "defs.h"

#ifdef PACKAGE

/*
 * include this file if PACKAGE is defined.
 */

/*
 * #define PACKAGE_DEBUG        1
 */


#include <stdio.h>

#include "wintcode.h"
#include "pckg.h"
#include "pckgcoff.h"
#include "pckgmake.h"
#include "rinfo.h"
#include "compile.h"



/*
 * Name of the package currently being created.
 * (Not currently loaded package)
 */
static char *currpckgname;




/*
 * Mark the procedure to indicate that it is will be in the current package.
 */
mark_package_proc(ent)
    ntbl_entry *ent;
{
    dbprot_t odbrs = w_dbprotect(DBRS_WRITABLE);
    ent->flags |= NMSK_PCKG_MARK;
    (void) w_dbprotect(odbrs);
    return (1);
}

/*
 * Unmark the procedure.
 */
unmark_package_proc(ent)
    ntbl_entry *ent;
{
    dbprot_t odbrs = w_dbprotect(DBRS_WRITABLE);
    ent->flags &= ~NMSK_PCKG_MARK;
    (void) w_dbprotect(odbrs);
    return (1);
}





static char *
get_pckg_name(ent)
    ntbl_entry *ent;
{
    long *curr_pckg;

    /*
     * Is it in the package that we are creating?
     */
    if (ent->flags & NMSK_PCKG_MARK)
	return (currpckgname);

    /*
     * Is it in one of packages in the package list?
     */
    curr_pckg = system_pckg;
    while (!ENDOF_PCKGLIST(curr_pckg)) {
	if (IN_PCKG((long) ent, curr_pckg))
	    return (PCKG_NAME(curr_pckg));
	curr_pckg = (long *) PCKG_PREVIOUS_PCKG(curr_pckg);
    }

    /*
     * It is not in one of packages in the package list and
     * in the package we are creating. Give warning message and
     * return the name of the package that we are creating.
     */
    fprintf(stderr,
	"\nWarning: Referenced procedure %s:%s/%d won't be in the package.",
	    TOKNAME(ent->modid),
	    TOKNAME(MFUNCTOR_TOKID(ent->tokid_arity)),
	    MFUNCTOR_ARITY(ent->tokid_arity));

    return (currpckgname);
}


ntbl_entry_name(ent, suffix, buf)
    ntbl_entry *ent;
    char *suffix;
    char *buf;
{
    char *pckgofent;

    pckgofent = get_pckg_name(ent);

    if (pckgofent != currpckgname)
	sprintf(buf, "%s_%x_%s", pckgofent, *(((long *) (ent)) - 1), suffix);
    else
	sprintf(buf, "%s_%x_%s", currpckgname, (long) (ent), suffix);
}



listasm_ntblentry(ent, pckgname)
    ntbl_entry *ent;
    char *pckgname;
{
    ntbl_entry tempent;
    char  procname[64];
    char  buf[64];

    /*
     * Get procedure name and current package name
     */
    sprintf(procname, "%s_%x", pckgname, ent);
    currpckgname = pckgname;

#ifdef PACKAGE_DEBUG
    fprintf(stdout, "\nProcedure %s:%s/%d",
	    TOKNAME(ent->modid),
	    TOKNAME(MFUNCTOR_TOKID(ent->tokid_arity)),
	    MFUNCTOR_ARITY(ent->tokid_arity));
#endif /* PACKAGE_DEBUG */

    /*
     * Procedure Header
     */
    COFF_ALIGN4;
    COFF_LONG_RAWDATA(ent);
    COFF_DECLARE_SYMBOL(procname);

    COFF_LONG_RAWDATA(ent->tokid_arity);
    if (isPrologProc(ent)) {
	sprintf(buf, "%s_%x", procname, clauseId(ent->first_clause));
	COFF_REFERENCE_SYMBOL(buf);
	sprintf(buf, "%s_%x", procname, clauseId(ent->last_clause));
	COFF_REFERENCE_SYMBOL(buf);
    }
    else {
	COFF_LONG_RAWDATA(0);
	COFF_LONG_RAWDATA(0);
    }
    COFF_LONG_RAWDATA(0);
    COFF_LONG_RAWDATA(0);
    COFF_LONG_RAWDATA(ent->lo_id);
    COFF_LONG_RAWDATA(ent->hi_id);
    if ((ent->flags & NMSK_USAGE) == NFLG_IMPORTED) {
	COFF_SHORT_RAWDATA(CHANGE_FLAGS(ent->flags, NFLG_UNDEFINED) & NMSK_SAVE);
    }
    else if ((ent->flags & NMSK_USAGE) == NFLG_SWITCH) {
	COFF_SHORT_RAWDATA(CHANGE_FLAGS(ent->flags, NFLG_MULTIPLE) & NMSK_SAVE);
    }
    else {
	COFF_SHORT_RAWDATA(ent->flags & NMSK_SAVE);
    }
    COFF_SHORT_RAWDATA(ent->modid);
    COFF_SHORT_RAWDATA(ent->icount);
    COFF_SHORT_RAWDATA(ent->nargs);

    /*
     * Procedure overflow, call, exec and code entries
     */

    COFF_PCKG_OVERFLOW(ent);

    sprintf(buf, "%s_%s", procname, "call");
    COFF_DECLARE_GLOBAL_SYMBOL(buf);

    COFF_PCKG_CALL_ENTRY(ent);

    sprintf(buf, "%s_%s", procname, "exec");
    COFF_DECLARE_GLOBAL_SYMBOL(buf);

    COFF_PCKG_EXEC_ENTRY(ent);

    sprintf(buf, "%s_%s", procname, "code");
    COFF_DECLARE_GLOBAL_SYMBOL(buf);

    switch (ent->flags & NMSK_USAGE) {

	case NFLG_BUILTIN:	/* builtin cases */
	    switch (ent->flags & NMSK_BLT_TYPE) {

		case NFLG_BLT_BUILTIN:
		    COFF_PCKG_CODE_BUILTIN(ent);
		    break;

		case NFLG_BLT_MODCLOSURE:
		    COFF_PCKG_CODE_MODCLOSURE(ent);
		    break;

		case NFLG_BLT_JMP:
		    COFF_PCKG_CODE_JMP(ent);
		    break;

		case NFLG_BLT_CALL:
		    COFF_PCKG_CODE_CALL(ent);
		    break;

		case NFLG_BLT_EQUAL:
		    COFF_PCKG_CODE_EQUAL(ent);
		    break;

		case NFLG_BLT_TRUE:
		    COFF_PCKG_CODE_TRUE(ent);
		    break;

		default:
		    fatal_error(FE_IN_PCKG9, 0);
		    break;
	    }
	    break;		/* builtin cases */

	case NFLG_SINGLE:
	    COFF_PCKG_CODE_SINGLE(ent, procname);
	    break;

	case NFLG_MULTIPLE:
	case NFLG_SWITCH:
	    COFF_PCKG_CODE_MULTIPLE(ent, procname);
	    break;

	case NFLG_IMPORTED:
	    ic_install_resolve_ref(&tempent);
	    COFF_PCKG_RESOLVE_REF(&tempent);
	    break;

	case NFLG_LIBBREAK:
	    COFF_PCKG_LIBBREAK(ent);
	    break;

	case NFLG_UNDEFINED:
	    COFF_PCKG_RESOLVE_REF(ent);
	    break;

	default:
	    fatal_error(FE_IN_PCKG9, 0);
	    break;
    }

    return (1);
}






listasm_clause(ca, pckgname)
    long *ca;
    char *pckgname;
{
    ntbl_entry *ent;
    unsigned long clid;
    char *riptr;
    char *riend;
    char *cls_code;
    char *cls_dstart;
    char *cls_end;
    char *cp;
    char  procname[64];
    char  buf[64];

    currpckgname = pckgname;
    ent = w_nametable[(procIdx(ca) & 0xffff)];
    clid = clauseId(ca);
    sprintf(procname, "%s_%x", pckgname, ent);

#ifdef PACKAGE_DEBUG
    fprintf(stdout, "\nClause %x of %s:%s/%d",
	    clid, TOKNAME(ent->modid),
	    TOKNAME(MFUNCTOR_TOKID(ent->tokid_arity)),
	    MFUNCTOR_ARITY(ent->tokid_arity));
#endif /* PACKAGE_DEBUG */

    if (clsRInfo(ca) == 0) {
	fprintf(stderr,
	      "\nError: No relocation information on clause %x of %s:%s/%d",
		clid,
		TOKNAME(ent->modid),
		TOKNAME(MFUNCTOR_TOKID(ent->tokid_arity)),
		MFUNCTOR_ARITY(ent->tokid_arity));
	return (-1);
    }

    /*
     * Clause header
     */
    COFF_ALIGN4;

    sprintf(buf, "%s_%x", procname, clid);
    COFF_DECLARE_SYMBOL(buf);
    COFF_RAWDATA(ca, WCI_NEXTCLAUSEADDR * sizeof (long));
    if (nextClauseAddr(ca) != (long *) 0) {
	sprintf(buf, "%s_%x", procname, clauseId(nextClauseAddr(ca)));
	COFF_REFERENCE_SYMBOL(buf);
    }
    else
	COFF_LONG_RAWDATA(nextClauseAddr(ca));

    /*
     * Choice entry
     */
    /* When choiceCode and choiceEntry are different */
    if (choiceCode(ca) != choiceEntry(ca)) {
	COFF_RAWDATA((char *) (choiceCode(ca)),
		     (WCI_CHOICEENTRY - WCI_CHOICECODE) * sizeof (long));
    }

    sprintf(buf, "%s_%x_%s", procname, clid, "choice");
    COFF_DECLARE_SYMBOL(buf);
    if (nextClauseAddr(ca) != (long *) 0) {
	if (coff_pckg_retry_me(procname, ca, ent) < 0)
	    return (-1);
    }
    else if (coff_pckg_trust_me(ca, ent) < 0)
	return (-1);

    /*
     * Code and dstart entry
     *
     * Get the pointers into the begining and end of the relocation
     * information in the clause. Using the relocation information,
     * copy the rest of the clause into the coff file.
     */

    riptr = (char *) clsRInfoBuf(ca);
    riend = riptr + *(long *) riptr + sizeof (long);
    riptr += sizeof (long);

#ifdef PACKAGE_DEBUG
    {
	unsigned long *e;

	for (e = (unsigned long *) riptr; e < (unsigned long *) riend; e++)
	    fprintf(stderr, "\n%x :  %08lx  type=%x loc=%x val=%x ",
		    (long) e, *e, RELOC_TYPE(e), RELOC_LOC(e), RELOC_VAL(e));
    }
#endif /* PACKAGE_DEBUG */

    sprintf(buf, "%s_%x_%s", procname, clid, "code");
    COFF_DECLARE_SYMBOL(buf);
    cls_code = (char *) (clauseCode(ca));
    cls_dstart = cls_code + (dstartCode(ca) * sizeof (Code));
    cp = cls_code;

    if (code_into_coff_file(&cp, cls_dstart, &riptr, riend, cls_code) < 0)
	return (-1);

    sprintf(buf, "%s_%x_%s", procname, clid, "dstart");
    COFF_DECLARE_SYMBOL(buf);
    cls_end = (char *) (ca + sizeUsedBlock(ca));

    if (code_into_coff_file(&cp, cls_end, &riptr, riend, cls_code) < 0)
	return (-1);

    return (1);
}



#define NTBL_CALLENTRY_OFFSET  (NTBL_HEADERSIZE+NTBL_OVERFLOWSIZE)

#define NTBL_EXECENTRY_OFFSET  \
			(NTBL_HEADERSIZE+NTBL_OVERFLOWSIZE+NTBL_CALLENTRYSIZE)


/*
 * Copy the given clause code portion into the COFF file.
 * At the same time, copy relocation information of the clause
 * into the COFF file.
 */
code_into_coff_file(cpstart, cpend, ristart, riend, cls_code)
    char **cpstart;
    char *cpend;
    unsigned long **ristart;
    unsigned long *riend;
    char *cls_code;
{
    char *cp;
    unsigned long *ri;
    char  buf[64];
    char *riloc;
    Code *addr;
    long  offs;
    ntbl_entry *toent;

    cp = *cpstart;
    ri = *ristart;

    while (cp < cpend) {
	if ((ri < riend) && ((riloc = cls_code + RELOC_LOC(ri)) < cpend)) {

	    /*
	     * There is a relocation entry before the code end.
	     * First copy the code before this relocatable object,
	     * then copy that relocatable object.
	     */
	    if (cp < riloc) {
		COFF_RAWDATA(cp, riloc - cp)
		    cp = riloc;
	    }

	    switch (RELOC_TYPE(ri)) {
#ifdef arch_m88k
		    /* arch_m88k */
		case RELOC_GVAR_OFF26:
		    COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(RELOC_VAL(ri)), P_R_REL26, 0)
			COFF_LONG_RAWDATA(*(long *) cp & ~MASK26)
			cp += sizeof (long);
		    break;
		case RELOC_GVAR_LO16:
		    COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(RELOC_VAL(ri)), P_R_RELL16, 0)
			COFF_SHORT_RAWDATA(0)
			cp += sizeof (short);
		    break;
		case RELOC_GVAR_HI16:
		    COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(RELOC_VAL(ri)), P_R_RELH16, 0)
			COFF_SHORT_RAWDATA(0)
			cp += sizeof (short);
		    break;
		case RELOC_PROC_CALL_OFF26:
		    offs = (long) (*(long *) cp & MASK26);
		    if (offs & 0x02000000)
			offs |= ~MASK26;
		    addr = (Code *) (((long *) cp) + offs);
		    toent = (ntbl_entry *) (addr - NTBL_CALLENTRY_OFFSET);
		    ntbl_entry_name(toent, "call", buf);
		    COFF_RELOC_SYMBOL(buf, P_R_REL26, 0);
		    COFF_LONG_RAWDATA(*(long *) cp & ~MASK26)
			cp += sizeof (long);
		    break;
		case RELOC_PROC_EXEC_OFF26:
		    offs = (long) (*(long *) cp & MASK26);
		    if (offs & 0x02000000)
			offs |= ~MASK26;
		    addr = (Code *) (((long *) cp) + offs);
		    toent = (ntbl_entry *) (addr - NTBL_EXECENTRY_OFFSET);
		    ntbl_entry_name(toent, "exec", buf);
		    COFF_RELOC_SYMBOL(buf, P_R_REL26, 0);
		    COFF_LONG_RAWDATA(*(long *) cp & ~MASK26)
			cp += sizeof (long);
		    break;
#else  /* arch_m88k */
#ifdef arch_sparc
		    /* arch_sparc */
		case RELOC_GVAR_WDISP30:
		    COFF_RELOC_SYMBOL_IDX(
				 COFF_SYMIDX(RELOC_VAL(ri)), P_R_WDISP30, 0)
			COFF_LONG_RAWDATA(*(long *) cp & ~MASK30)
			cp += sizeof (long);
		    break;
		case RELOC_GVAR_WDISP22:
		    COFF_RELOC_SYMBOL_IDX(
				 COFF_SYMIDX(RELOC_VAL(ri)), P_R_WDISP22, 0)
			COFF_LONG_RAWDATA(*(long *) cp & ~MASK22)
			cp += sizeof (long);
		    break;
		case RELOC_GVAR_HI22:
		    COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(RELOC_VAL(ri)), P_R_HI22, 0)
			COFF_LONG_RAWDATA(*(long *) cp & ~MASK22)
			cp += sizeof (long);
		    break;
		case RELOC_GVAR_LO10:
		    COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(RELOC_VAL(ri)), P_R_LO10, 0)
			COFF_LONG_RAWDATA(*(long *) cp & ~MASK10)
			cp += sizeof (long);
		    break;
		case RELOC_PROC_CALL_WDISP30:
		    offs = (long) (*(long *) cp & MASK30);
		    if (offs & 0x020000000)
			offs |= ~MASK30;
		    addr = (Code *) (((long *) cp) + offs);
		    toent = (ntbl_entry *) (addr - NTBL_CALLENTRY_OFFSET);
		    ntbl_entry_name(toent, "call", buf);
		    COFF_RELOC_SYMBOL(buf, P_R_WDISP30, 0);
		    COFF_LONG_RAWDATA(*(long *) cp & ~MASK30)
			cp += sizeof (long);
		    break;
		case RELOC_PROC_EXEC_WDISP30:
		    offs = (long) (*(long *) cp & MASK30);
		    if (offs & 0x020000000)
			offs |= ~MASK30;
		    addr = (Code *) (((long *) cp) + offs);
		    toent = (ntbl_entry *) (addr - NTBL_EXECENTRY_OFFSET);
		    ntbl_entry_name(toent, "exec", buf);
		    COFF_RELOC_SYMBOL(buf, P_R_WDISP30, 0);
		    COFF_LONG_RAWDATA(*(long *) cp & ~MASK30)
			cp += sizeof (long);
		    break;
#else  /* arch_sparc */
		    /* arch_i386 and arch_m68k */
		case RELOC_GVAR:
		    COFF_RELOC_SYMBOL_IDX(COFF_SYMIDX(RELOC_VAL(ri)), P_R_VIR32, 0)
			COFF_LONG_RAWDATA(0)
			cp += sizeof (long);
		    break;
		case RELOC_PROC_CALL:
		    addr = (Code *) (*(long *) cp);
		    toent = (ntbl_entry *) (addr - NTBL_CALLENTRY_OFFSET);
		    ntbl_entry_name(toent, "call", buf);
		    COFF_RELOC_SYMBOL(buf, P_R_VIR32, 0)
			COFF_LONG_RAWDATA(0)
			cp += sizeof (long);
		    break;
		case RELOC_PROC_EXEC:
		    addr = (Code *) (*(long *) cp);
		    toent = (ntbl_entry *) (addr - NTBL_EXECENTRY_OFFSET);
		    ntbl_entry_name(toent, "exec", buf);
		    COFF_RELOC_SYMBOL(buf, P_R_VIR32, 0)
			COFF_LONG_RAWDATA(0)
			cp += sizeof (long);
		    break;
#endif /* arch_sparc */
#endif /* arch_m88k */
		default:
		    fatal_error(FE_IN_PCKG10, 0);
		    break;
	    }

	    ri++;
	}
	else {
	    /*
	     * There is no more relocatable object,
	     * Copy rest of the code.
	     */
	    COFF_RAWDATA(cp, cpend - cp)
		cp = cpend;
	}
    }

    *cpstart = cpend;
    *ristart = ri;

    return (1);
}

#endif /* PACKAGE */
