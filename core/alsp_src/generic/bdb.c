/*=============================================================*
 |		bdb.c   
 | Copyright (c) 1985 by Kevin A. Buettner
 | Copyright (c) 1986-95 by Applied Logic Systems
 |
 |		-- Database Prolog builtins defined in C.
 |
 | Program Author:  Kevin A. Buettner
 | Creation:  11/14/84
 | 06/28/85: K. Buettner  -- Conversion to wam and compiled prolog
 | 09/12/85: K. Buettner  -- arithmetic predicates moved to separate file.
 | 01/28/86: K. Buettner  -- IBM PC conversion
 | 05/04/89: K. Hughes, K. Buettner -- split off from built.c
 *=============================================================*/

/*-------------------------------------------
	Contents:
		pbi_abolish()
		pbi_abolish_clausegroup()
		pbi_asserta()
		pbi_assertz()
		pbi_addclause()
		pbi_execcommand()
		pbi_erase()
		pbi_icode()
		pbi_index_proc()
		pbi_massively_abolish_clausegroup()
		pbi_nextproc()
		pbi_procinfo()
		pbi_clauseinfo()
		pbi_dynamic()
		pbi_exported_proc()
		pbi_next_module()
		pbi_listasm_clause()
		pbi_listasm_ntblentry()
		pbi_firstargkey()
		pbi_resolve_module()
		pbi_push_clausegroup()
		pbi_pop_clausegroup()
		pbi_collectcode()
		pbi_libbreak()
 *------------------------------------------*/

#include "defs.h"
#include "wintcode.h"
#include "module.h"
#include "icodegen.h"
#include "cutmacro.h"
#include "compile.h"

int
pbi_abolish(void)
{				/* abolish(Module,Pred,Arity)   */
    PWord m, p, a;
    int   mt, pt, at;

    w_get_An(&m, &mt, 1);
    w_get_An(&p, &pt, 2);
    w_get_An(&a, &at, 3);

    if (xform_uia(&m, &mt) && xform_uia(&p, &pt) && at == WTP_INTEGER) {
	ntbl_entry *ent;

	if ( (ent = w_nameprobe(m, p, (int) a)) ) {
	    dbprot_t odbrs = w_dbprotect(DBRS_WRITABLE);
	    w_abolish(ent);
	    if (ent->flags & NMSK_DYNAMIC) {
			ic_install_fail(ent);
		}
	    (void) w_dbprotect(odbrs);
	}
	SUCCEED;
    }
    else
	FAIL;
}

int
pbi_abolish_clausegroup(void)
{				/* abolish_clausegroup(Module,Pred,Arity,CG) */
    PWord m, p, a, cg;
    int   mt, pt, at, cgt;

    w_get_An(&m, &mt, 1);
    w_get_An(&p, &pt, 2);
    w_get_An(&a, &at, 3);
    w_get_An(&cg, &cgt, 4);

    if (xform_uia(&m, &mt) && xform_uia(&p, &pt) && at == WTP_INTEGER
	&& cgt == WTP_INTEGER) {
	ntbl_entry *ent;

	if ( (ent = w_nameprobe(m, p, (int) a)) ) {
	    dbprot_t odbrs = w_dbprotect(DBRS_WRITABLE);
	    w_abolish_cg(ent, cg, -1);
	    (void) w_dbprotect(odbrs);
	}
	SUCCEED;
    }
    else
	FAIL;
}

extern long *aib_clause_addr;
static	int	doassert	( int, PWord, pword, int );

int
pbi_asserta(void)
{				/* asserta(Module,Clause,Ref,ReconsultFlag) */
    PWord v1, v2, v3, v4;
    int   t1, t2, t3, t4;
    PWord vdbref;
    int   tdbref;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);

    if (xform_uia(&v1, &t1) && t4 == WTP_INTEGER &&
	doassert(IC_ASSERTA, v1, cvt_term_to_rule(v2, t2), v4)) {
	if (aib_clause_addr != (long *) 0) {
	    make_dbref(aib_clause_addr, &vdbref, &tdbref);
	    if (w_unify(v3, t3, vdbref, tdbref))
		SUCCEED;
	}
    }

    FAIL;
}

int
pbi_assertz(void)
{				/* assertz(Module,Clause,Ref,ReconsultFlag) */
    PWord v1, v2, v3, v4;
    int   t1, t2, t3, t4;
    PWord vdbref;
    int   tdbref;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);

    if (xform_uia(&v1, &t1) && t4 == WTP_INTEGER &&
	doassert(IC_ASSERTZ, v1, cvt_term_to_rule(v2, t2), v4)) {
	if (aib_clause_addr != (long *) 0) {
	    make_dbref(aib_clause_addr, &vdbref, &tdbref);
	    if (w_unify(v3, t3, vdbref, tdbref))
		SUCCEED;
	}
    }

    FAIL;
}

int
pbi_addclause(void)
{				/* addclause(Module,Clause) */
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (xform_uia(&v1, &t1) &&
		doassert(IC_ADDCLAUSE, v1, cvt_term_to_rule(v2, t2), 0))
			SUCCEED;
    else
			FAIL;
}

int
pbi_execcommand(void)
{				/* execcommand(Goals) */
    PWord v1, vc, vh;
    int   t1, tc, th;

    w_get_An(&v1, &t1, 1);
    w_mk_term(&vc, &tc, TK_RIF, 2);
    w_mk_sym(&vh, &th, TK_RIF);
    w_install_argn(vc, 1, vh, th);
    w_install_argn(vc, 2, v1, t1);
    if (doassert(IC_EXECCOMMAND, cur_mod, cvt_term_to_rule(vc, tc), 0))
	SUCCEED;
    else
	FAIL;
}

static int
doassert(int which, PWord mod, pword rule, int reconsult_flag)
{
    int   status, popmod;
    unsigned long old_reconstamp = 0;
    dbprot_t odbrs;

    if (rule == NIL_VAL)
		return 0;

// Note: wintcode.h:extern	unsigned long w_reconstamp;
    if (reconsult_flag) {
		old_reconstamp = w_reconstamp;
		w_reconstamp = 0;
    }

    odbrs = w_dbprotect(DBRS_WRITABLE);

    icode(IC_INIT, 0, 0, 0, 0);
    if (mod == cur_mod)
		popmod = 0;
    else {
		popmod = 1;
		icode(IC_NEWMODULE, mod, 0, 0, 0);
    }

    if ( (status = compile_clause(rule, 0)) )
		icode(which, 0, 0, 0, 0);

    if (popmod)
		icode(IC_ENDMODULE, 0, 0, 0, 0);

    (void) w_dbprotect(odbrs);

    if (reconsult_flag) {
		w_reconstamp = old_reconstamp;
    }

    return status;

}

int
pbi_erase(void)
{				/* $erase(DBRef)        */
    PWord v;
    int   t;
    PWord ni;
    long *ca;
    dbprot_t odbrs;

    w_get_An(&v, &t, 1);
    odbrs = w_dbprotect(DBRS_WRITABLE);
    if ((ca = validate_dbref(v, t, &ni)) == 0 || w_erase((int) ni, ca) == 0) {
	(void) w_dbprotect(odbrs);
	FAIL;
    }
    else {
	(void) w_dbprotect(odbrs);
	SUCCEED;
    }
}

int
pbi_icode(void)
{				/* $icode(OpCode,W,X,Y,Z) */
    PWord v1, v2, v3, v4, v5;
    int   t1, t2, t3, t4, t5;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);
    w_get_An(&v5, &t5, 5);

    if (t1 == WTP_INTEGER && t3 == WTP_INTEGER && t5 == WTP_INTEGER &&
			(t4 == WTP_INTEGER || t4 == WTP_SYMBOL) &&
			(t2 == WTP_INTEGER || t2 == WTP_SYMBOL) )   {
		dbprot_t odbrs = w_dbprotect(DBRS_WRITABLE);
		icode(v1, v2, v3, v4, v5);
		(void) w_dbprotect(odbrs);
    }
    else
		FAIL;

    SUCCEED;
}

int
pbi_index_proc(void)
{
#ifdef Indexing
    PWord v1, v2, v3;
    int   t1, t2, t3;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (xform_uia(&v1, &t1) && xform_uia(&v2, &t2) && t3 == WTP_INTEGER) {
	dbprot_t odbrs = w_dbprotect(DBRS_WRITABLE);
	indexproc(v1, v2, (int) v3);
	(void) w_dbprotect(odbrs);
    }

#endif
    SUCCEED;
}

/*---------------------------------------*
 |	massively_abolish_clausegroup(CG) 
 *---------------------------------------*/

int
pbi_massively_abolish_clausegroup(void)
{				
    PWord v1;
    int   t1;
    int   i;
    ntbl_entry *ent;
    dbprot_t odbrs;

    w_get_An(&v1, &t1, 1);

    if (t1 != WTP_INTEGER)
	FAIL;

    odbrs = w_dbprotect(DBRS_WRITABLE);
    for (i = 0; i < NTBL_SIZE; i++)
	if ((ent = w_nametable[i]))
	    w_abolish_cg(ent, v1, -1);
    (void) w_dbprotect(odbrs);
    SUCCEED;
}

int
pbi_nextproc(void)
{				/* $nextproc(N,F,NN) */
    PWord v1, v2, v3;
    int   t1, t2, t3;
    PWord next;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

    if (t1 == WTP_INTEGER) {
	if (t2 != WTP_INTEGER)
	    v2 = 0;

	if ((next = nextproc(v1, (int) v2)) != (PWord) (-1) &&
	    w_unify(v3, t3, next, WTP_INTEGER))
	    SUCCEED;
    }

    FAIL;
}





/*-------------------------------------------------------------------------
 * pbi_procinfo is called from Prolog as $procinfo(N,M,P,A,DBRef,ProcType)
 *
 * If N is instantiated to a valid name index, the module, procedure name,
 * arity, and first clause are obtained and unified appropriately.  Otherwise,
 * M, P, and A must be instantiated.  The name index and first clause are
 * obtained from this information and (if valid) are unified.  This is an
 * enhanced version of Keiths original procedure to make clause and retract
 * (much) more efficient.
 *------------------------------------------------------------------------*/

#define PROLOG_PROC 		0
#define BUILTIN_PROC 		1
#define IMPORTED_PROC		2
#define UNDEFINED_PROC 		3
#define LIBBREAK_PROC       4
#define UNKNOWN_PROC_TYPE 	-1

int
pbi_procinfo(void)
{
    PWord v1, v2, v3, v4, v5, v6;
    int   t1, t2, t3, t4, t5, t6;
    PWord vdbref, vidx;
    PWord proc_type;
    int   tdbref;
    ntbl_entry *ent;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);
    w_get_An(&v5, &t5, 5);
    w_get_An(&v6, &t6, 6);

    if (t1 == WTP_INTEGER) {
	if (v1 < 0 || v1 >= NTBL_SIZE ||
	    (ent = w_nametable[v1]) == (ntbl_entry *) 0 ||
	    !w_unify(v2, t2, (PWord) ent->modid, WTP_SYMBOL) ||
	    !w_unify(v3, t3, (PWord) MFUNCTOR_TOKID(ent->tokid_arity), WTP_SYMBOL) ||
	    !w_unify(v4, t4, (PWord) MFUNCTOR_ARITY(ent->tokid_arity), WTP_INTEGER))
	    FAIL;
	vidx = v1;
    }
    else if (xform_uia(&v2, &t2) && xform_uia(&v3, &t3) && t4 == WTP_INTEGER) {
	if ((vidx = nameprobe(v2, v3, (int) v4)) == -1 ||
	    (ent = w_nametable[vidx]) == (ntbl_entry *) 0 ||
	    !w_unify(v1, t1, vidx, WTP_INTEGER))
	    FAIL;
    }
    else
	FAIL;

    make_dbref(first_clause((int) vidx), &vdbref, &tdbref);

    switch (ent->flags & NMSK_USAGE) {
	case NFLG_SINGLE:
	case NFLG_MULTIPLE:
	case NFLG_SWITCH:
	    proc_type = PROLOG_PROC;
	    break;
	case NFLG_BUILTIN:
	    proc_type = BUILTIN_PROC;
	    break;
	case NFLG_IMPORTED:
	    proc_type = IMPORTED_PROC;
	    break;
	case NFLG_UNDEFINED:
	    proc_type = UNDEFINED_PROC;
	    break;
	case NFLG_LIBBREAK:	/* 4/6/93 - raman */
	    proc_type = LIBBREAK_PROC;
	    break;
	default:
	    proc_type = UNKNOWN_PROC_TYPE;
    }

    if (w_unify(v5, t5, vdbref, tdbref) && w_unify(v6, t6, proc_type, WTP_INTEGER))
	SUCCEED;
    else
	FAIL;

}

int
pbi_clauseinfo(void)
{
    PWord v1, v2, v3, v4;
    int   t1, t2, t3, t4;
    PWord vdbref;
    int   tdbref;
    PWord procindex;
    long *curClause;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);

    if ((curClause = validate_dbref(v1, t1, &procindex)) != (long *) 0) {
	make_dbref(next_clause(curClause), &vdbref, &tdbref);

	if (w_unify(v2, t2, vdbref, tdbref) &&
	    w_unify(v3, t3, procindex, WTP_INTEGER) &&
	    w_unify(v4, t4, cgId(curClause), WTP_INTEGER))
	    SUCCEED;
    }

    FAIL;
}

int
pbi_dynamic(void)
{				/* $dynamic(Module,Pred,Arity)   */
    PWord m, p, a;
    int   mt, pt, at;

    w_get_An(&m, &mt, 1);
    w_get_An(&p, &pt, 2);
    w_get_An(&a, &at, 3);

    if (xform_uia(&m, &mt) && at == WTP_INTEGER && force_uia(&p, &pt)) {
		dbprot_t odbrs = w_dbprotect(DBRS_WRITABLE);
		w_dynamic(m,p,a);
		(void) w_dbprotect(odbrs);
		SUCCEED;
    }
    else
		FAIL;
}

int
pbi_exported_proc(void)
{				/* $exported_proc(Module,Pred,Arity)   */
    PWord m, p, a;
    int   mt, pt, at;
    ntbl_entry *ent;

    w_get_An(&m, &mt, 1);
    w_get_An(&p, &pt, 2);
    w_get_An(&a, &at, 3);

    if (xform_uia(&m, &mt) && xform_uia(&p, &pt) && at == WTP_INTEGER &&
	mt == WTP_SYMBOL && (ent = w_nameprobe(m, p, (int) a)) &&
	(ent->flags & NMSK_EXPORT))
	SUCCEED;
    else
	FAIL;
}

int
pbi_next_module(void)
{				/* $next_module(N,NN,Module,UseList) */
    PWord v1, v2, v3, v4;
    int   t1, t2, t3, t4;
    PWord n;
    PWord m, u;
    int   mt, ut;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);

    if (t1 == WTP_INTEGER &&
	(n = next_module((int) v1, &m, &mt, &u, &ut)) != -1 &&
	w_unify(v2, t2, n, WTP_INTEGER) &&
	w_unify(v3, t3, m, mt) &&
	w_unify(v4, t4, u, ut))
	SUCCEED;
    else
	FAIL;
}

/*-------------------------------------------------------------*
 |	pbi_cr_mod_close()
 |	$cr_mod_close(Mod,Pred,Arity,TProc)
 *-------------------------------------------------------------*/
int
pbi_cr_mod_close(void)
{				
    PWord v1, v2, v3, v4;
    int   t1, t2, t3, t4;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);

	if ((t1 != WTP_SYMBOL) || (t2 != WTP_SYMBOL) ||
		(t3 != WTP_INTEGER) || (t4 != WTP_SYMBOL))
		FAIL;

	if (createModCloseProc(v1, v2, v3, v4))
		SUCCEED;
	else
		FAIL;
}


int
pbi_listasm_clause(void)
{				/* $listasm_clause(DBRef) */
    PWord v1;
    int   t1;
    PWord na;
    long *ca;

    w_get_An(&v1, &t1, 1);

    if ((ca = validate_dbref(v1, t1, &na)) != 0) {
	list_asm(choiceCode(ca),
		     (int) ((sizeCode(ca) + (WCI_CLAUSECODE - WCI_CHOICECODE)) * 
					(sizeof (long) / sizeof (Code))));
	SUCCEED;
    }
    else
	FAIL;
}

int
pbi_listasm_ntblentry(void)
{				/* $listasm_ntblentry(Module,Pred,Arity)   */
    PWord m, p, a;
    int   mt, pt, at;
    ntbl_entry *ent;

    w_get_An(&m, &mt, 1);
    w_get_An(&p, &pt, 2);
    w_get_An(&a, &at, 3);

    if (xform_uia(&m, &mt) && xform_uia(&p, &pt) && at == WTP_INTEGER &&
	mt == WTP_SYMBOL) {
	if ( (ent = w_nameprobe(m, p, (int) a)) ) {
	    list_asm(ent->overflow, (int) (NTBL_OVERFLOWSIZE 
	                               + NTBL_CALLENTRYSIZE
				       + NTBL_EXECENTRYSIZE
				       + NTBL_CODESIZE));
	    SUCCEED;
	}
    }
    FAIL;
}

int
pbi_firstargkey(void)
{				/* $firstargkey(DBRef,FirstArgKey)  */
    PWord v1, v2;
    int   t1, t2;
    PWord a;
    int   at;
    PWord procindex;
    long *curClause;
    int   i;
    PWord firstargkey, tokid;
    int   arity;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if ((curClause = validate_dbref(v1, t1, &procindex)) != (long *) 0) {
	firstargkey = *(curClause + WCI_FIRSTARGKEY);
	if (MTP_TAG(firstargkey) == MTP_UNBOUND)
	    w_mk_unbound(&a, &at);
	else if (MTP_TAG(firstargkey) == MTP_LIST) {
	    w_mk_list(&a, &at);
	    w_install_unbound_car(a);
	    w_install_unbound_cdr(a);
	}
#ifdef MTP_CONSTTAG
	else if (MTP_CONSTTAG(firstargkey) == MTP_INT) {
	    a = MINTEGER(firstargkey);
	    at = WTP_INTEGER;
	}
#else  /* MTP_CONSTTAG */
	else if (MTP_TAG(firstargkey) == MTP_INT) {
	    a = MINTEGER(firstargkey);
	    at = WTP_INTEGER;
	}
#endif /* MTP_CONSTTAG */
	else {			/* A functor or a symbol */
	    tokid = MFUNCTOR_TOKID(firstargkey);
	    arity = MFUNCTOR_ARITY(firstargkey);
	    if (arity == 0) {	/* symbol */
		a = tokid;
		at = WTP_SYMBOL;
	    }
	    else {		/* structure */
		w_mk_term(&a, &at, tokid, arity);
		for (i = 1; i <= arity; i++)
		    w_install_unbound_argn(a, i);
	    }
	}
	if (w_unify(v2, t2, a, at))
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}

int
pbi_resolve_module(void)
{
    PWord v1, v2, v3, v4;
    int   t1, t2, t3, t4;
    ntbl_entry *ent;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);
    w_get_An(&v4, &t4, 4);

    if (xform_uia(&v1, &t1) && modprobe_id(v1) != -1 &&
	xform_uia(&v2, &t2) && t3 == WTP_INTEGER) {

	if ((ent = w_nameprobe(v1, v2, (int) v3)) &&
	    (ent->flags & NMSK_USAGE) != NFLG_UNDEFINED &&
	    (ent->flags & NMSK_USAGE) != NFLG_IMPORTED) ;	/* ok, we got
								 * it locally
								 */
	else if ((ent = resolve_ref(v1, v2, (int) v3)))	/* try looking for it
							 */
	    ;			/* Found it on module chain */
	else if (isCutMacro(v2, (int) v3 - 1)) {
	    /* Have a cut macro, try again by subtracting one from arity */
	    if ((ent = w_nameprobe(v1, v2, (int) v3 - 1)) &&
		(ent->flags & NMSK_USAGE) != NFLG_UNDEFINED &&
		(ent->flags & NMSK_USAGE) != NFLG_IMPORTED) ;	/* found
								 * cutmacro
								 * locally
								 */
	    else if (!(ent = resolve_ref(v1, v2, (int) v3 - 1)))
		FAIL;
	}
	else
	    FAIL;

	if (w_unify(v4, t4, (PWord) ent->modid, WTP_SYMBOL))
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}

int
pbi_push_clausegroup(void)
{				/* push_clausegroup(CG) */
    PWord v1;
    int   t1;

    w_get_An(&v1, &t1, 1);

    if (t1 != WTP_INTEGER)
	FAIL;

    push_clausegroup(v1);
    SUCCEED;
}

int
pbi_pop_clausegroup(void)
{				/* pop_clausegroup(CG) */
    PWord v1;
    int   t1;

    w_get_An(&v1, &t1, 1);

    if (w_unify(v1, t1, (PWord) pop_clausegroup(), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}

#ifdef PRIM_DBG
/* Test stuff (kev) */

int
pbi_collectcode(void)
{
#ifdef CodeGC
    dbprot_t odbrs = w_dbprotect(DBRS_WRITABLE);
    w_collect();
    (void) w_dbprotect(odbrs);
#endif
    SUCCEED;
}
/* End Test stuff (kev) */
#endif /* PRIM_DBG */

#ifdef LIBBRK

int
pbi_libbreak(void)
{				/* $libbreak(Module,Pred,Arity,BreakNum)   */
    PWord m, p, a, i;
    int   mt, pt, at, it;

    w_get_An(&m, &mt, 1);
    w_get_An(&p, &pt, 2);
    w_get_An(&a, &at, 3);
    w_get_An(&i, &it, 4);

    if (xform_uia(&m, &mt) && xform_uia(&p, &pt) && at == WTP_INTEGER &&
	it == WTP_INTEGER) {

	dbprot_t odbrs = w_dbprotect(DBRS_WRITABLE);
	w_libbreak(m, p, a, i);
	(void) w_dbprotect(odbrs);
	SUCCEED;
    }
    else
	FAIL;
}

#endif /* LIBBRK */

