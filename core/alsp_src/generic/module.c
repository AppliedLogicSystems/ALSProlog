/*=====================================================================*
 |			module.c                    
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-95 Applied Logic Systems, Inc.
 |
 |			-- module management code
 |
 | Author:  Kevin A. Buettner
 | Creation: 6/21/85
 | 01/15/85 - K.Buettner -- port to IBM-PC; split off from machine.c
 | 10/26/94, C. Houpt -- Various UCHAR* casts.
 |		-- Added pragmas to force some pointer returning functions
 |		   to use DO instead of A0 register under MetroWerks.
 *=====================================================================*/
#include "defs.h"
#include "module.h"
#include "wintcode.h"
#include "cutmacro.h"
#include "main.h"
#include "icodegen.h"

/*//int  *top_module;*/
/*//static int *module_stack;*/

/*//int  *top_clausegroup;*/
/*//static int *clausegroup_stack;*/

struct mtbl_entry {
    PWord modid;
    short nextuse;
}; 

/*//static struct mtbl_entry *module_table;*/

struct use_entry {
    PWord modid;
    short nextuse;
}; 

/*//static struct use_entry *use_table;*/

/*//static int nmods;*/
/*//static int nuses;*/

/*
 * resolve_reference is called by wm_resolve_ref with the name address. It
 * returns the address to continue execution at.
 *
 */
#ifdef POINTERS_IN_A0
#pragma pointers_in_D0
#endif

Code *
resolve_reference(ntbl_entry *entfrom)
    /* entfrom: entry from which we are coming from */
{
    ntbl_entry *entto;		/* entry where we'd like to go to */
    PWord functor;
    int   arity;

    functor = MFUNCTOR_TOKID(entfrom->tokid_arity);
    arity = MFUNCTOR_ARITY(entfrom->tokid_arity);

    entto = resolve_ref((PWord) entfrom->modid, (PWord) functor, arity);

    if (entto == (ntbl_entry *) 0) {
	wm_interrupt_caught = ALSSIG_UNDEFINED_PRED;
	wm_safety = wm_trigger;
	return entfrom->exec_entry;
    }
    else {
	int   usage = entto->flags & NMSK_USAGE;

	dbprot_t odbrs = w_dbprotect(DBRS_WRITABLE);
	if ((usage == NFLG_BUILTIN || usage == NFLG_IMPORTED) && entto->code[0] != W_FOREIGN_JUMP)
	    copy_code((long *) entto->code, (long *) entfrom->code,
		      (int)(NTBL_CODESIZE * sizeof (Code) / sizeof (long)));
	else
	    ic_install_reference(entfrom->code, (PWord) entto->code);

	entfrom->flags = (entfrom->flags & ~NMSK_USAGE) | NFLG_IMPORTED;
	(void) w_dbprotect(odbrs);

	return entfrom->code;
    }
}
#ifdef POINTERS_IN_A0
#pragma pointers_in_A0
#endif

ntbl_entry *
resolve_ref(PWord modid, PWord tokid, int arity)
{
    register int u;
    int   modidx;
    ntbl_entry *ent;

    if ((modidx = modprobe_id(modid)) != -1) {
	/* Scan through the use list, looking for a hit. */
	for (u = module_table[modidx].nextuse;
	     u != -1;
	     u = use_table[u].nextuse) {
	    ent = w_nameprobe(use_table[u].modid, tokid, arity);

	    if (ent != (ntbl_entry *) 0 &&
		(ent->flags & NMSK_EXPORT) &&
		(ent->flags & NMSK_USAGE) != NFLG_UNDEFINED)
		return ent;
	}
    }

    return (ntbl_entry *) 0;
}

/*
 * call_resolve_reference is called by wm_call to figure out where to jump
 * to.  It passes in the module id, token id, and arity of the thing
 * that it would like to call.  This procedure returns the address of
 * the code to execute.
 *
 * m and p will be tagged Prolog objects that will either be symbols or UIAs.
 * They have been dereferenced.
 *
 * The i argument is used if the call() decides to ignore the
 * overflow check. 0 means don't ignore it, while 1 means ignore it.
 * If it is ignored, make sure the calling routine sets the E pointer.
 */
#ifdef POINTERS_IN_A0
#pragma pointers_in_D0
#endif

Code *
call_resolve_reference(PWord m, PWord p, int a, int i)
{
    ntbl_entry *ent;
    dbprot_t odbrs;

    if (M_ISUIA(m))
	m = find_token((UCHAR *)(M_FIRSTUIAWORD(MUIA(m))));
    else
	m = MFUNCTOR_TOKID(m);

    if (M_ISUIA(p))
	p = find_token((UCHAR *) (M_FIRSTUIAWORD(MUIA(p))));
    else
	p = MFUNCTOR_TOKID(p);

    if ( (ent = w_nameprobe(m, p, a)) )
	return (i) ? ent->code : ent->exec_entry;
    else if ( (ent = resolve_ref(m, p, a)) )
	return (i) ? ent->code : ent->exec_entry;
    else if (isCutMacro(p, a - 1)) {
	if ( (ent = w_nameprobe(m, p, a - 1)) )
	    return (i) ? ent->code : ent->exec_entry;
	else if ( (ent = resolve_ref(m, p, a - 1)) )
	    return (i) ? ent->code : ent->exec_entry;
    }
    
    wm_interrupt_caught = ALSSIG_UNDEFINED_PRED;
    wm_safety = wm_trigger;
    /* FIXME: Find way to avoid creating an entry via w_namelookup */
    odbrs = w_dbprotect(DBRS_WRITABLE);
    ent = w_nametable[w_namelookup(m, p, a)];
    (void) w_dbprotect(odbrs);
    return ent->exec_entry;
}
#ifdef POINTERS_IN_A0
#pragma pointers_in_A0
#endif

#ifdef PACKAGE
void
set_curr_package_module(void)
{
    /* set current module to the module "builtin" */
    top_module = &module_stack[0];
    cur_mod = find_token("user");
}
#endif /* PACKAGE */


int
next_module(int n, PWord *m, int  *mt, PWord *u, int  *ut)
    /* m: module */
    /* mt: module type */
    /* u: use list */
    /* ut: use list type */
{
    PWord l, ll;		/* temporary lists */
    int   llt;			/* temporary list types */
    int   i;

    n++;			/* next module */

    if (n >= 0 && n < nmods) {	/* when n=-1, we get first module */
	/*
	 * get module name
	 */

	*m = module_table[n].modid;
	*mt = WTP_SYMBOL;

	/*
	 * get use list
	 */

	if ((i = module_table[n].nextuse) == -1) {
	    *u = TK_NIL;
	    *ut = WTP_SYMBOL;
	}
	else {
	    w_mk_list(u, ut);
	    l = *u;
	    w_install_car(l, use_table[i].modid, WTP_SYMBOL);
	    i = use_table[i].nextuse;

	    while (i != -1) {
		w_mk_list(&ll, &llt);
		w_install_cdr(l, ll, llt);
		w_install_car(ll, use_table[i].modid, WTP_SYMBOL);

		l = ll;
		i = use_table[i].nextuse;
	    }

	    w_install_cdr(l, (PWord) TK_NIL, WTP_SYMBOL);
	}

	return (n);
    }
    else
	return (-1);
}


/*
 * mod_id gets the index into the module table from the token id
 * given, inserting the module into the table if not there.
 */

int
mod_id(int tk)
{
    int   i;

    for (i = 0; i < nmods && module_table[i].modid != tk; i++) ;

    if (i == nmods) {

	if (nmods >= NMODULES)
	    fatal_error(FE_FULL_MODTBL, 0);

	module_table[i].modid = tk;
	module_table[i].nextuse = -1;
	nmods++;
    }

    return (i);
}

/*
 * modprobe_id gets the index into the module table from the token id
 * given, returning -1 if non-existent.
 */

int
modprobe_id(PWord tk)
{
    int   i;

    for (i = 0; i < nmods && module_table[i].modid != tk; i++) ;

    return ((i >= nmods) ? -1 : i);
}

/*
 * The parser calls new_mod with the token representing the module name.
 *
 * The following actions are taken:
 *      o  The token is assigned a module id. (or perhaps the module id is
 *              assigned to the token).
 *      o  The standard use definitions are made.
 *      o  The module is pushed onto the module stack
 *      o  The module is added to the global modules use list.
 *      o  Definitions for ; and , (and others) are inserted into the module.
 */


/*//static long *default_uses;*/
/*//static long default_usemax;*/

struct defprocs {
    PWord tokid;
    short arity;
};

/*//static struct defprocs *default_procs;*/
/*//static long default_procmax;*/

#define WARN_WHEN_REDEFINED_NUM 4


void
new_mod(PWord tk)
{
    register int i;
    int   modidx;
    int   ni;

    /* Advance the module stack pointer and check for stack overflow */
    if (++top_module == &module_stack[MAXMODNESTING])
	fatal_error(FE_OVER_MODSTK, 0);

    /* Set the current module (for the compiler and that ilk. */
    cur_mod = tk;

    modidx = mod_id(cur_mod);

    /* Make sure the module hasn't already been created. */
    if (module_table[modidx].nextuse != -1)
	return;

    /* Add in the auto-uses */
    for (i = 0; i < default_usemax; i++)
	adduse(modidx, default_uses[i]);

    /* Global module gets everything */
    mod_adduse(MODULE_GLOBAL, cur_mod);

    /* Add the default procedures */
    for (i = 0; i < default_procmax; i++) {
	ni = w_namelookup((PWord) cur_mod,
			  default_procs[i].tokid,
			  default_procs[i].arity);
	if (i < WARN_WHEN_REDEFINED_NUM) {
	    w_nametable[ni]->flags |= NMSK_PERMANENT;
	}
    }
}


/*
 * end_mod is used to pop the module stack.  If potential underflow is
 * detected, a warning is given, but no other action is taken.
 *
 */

void
end_mod(void)
{

    if (top_module == module_stack) {
	fprintf(stderr, "Warning: module stack underflow (too many endmods).\n");
    }
    else {
	top_module--;
    }
}

/*
 * push_clausegroup is called to push a new clause group
 */

void
push_clausegroup(int cg)
{
    if (++top_clausegroup == &clausegroup_stack[MAXMODNESTING])
	fatal_error(FE_OVER_CGSTK, 0);
    *top_clausegroup = cg;
}

/*
 * pop_clausegroup
 */

int
pop_clausegroup(void)
{
    int   cg;

    cg = *top_clausegroup;
    if (top_clausegroup != clausegroup_stack)
	top_clausegroup--;
    return cg;
}

/*
 * add_default_use is called by a function of a similar name in
 *      builtins.c  It adds the given module id to the default use
 *      list.
 */

void
add_default_use(int tokid)
{
    int   i;

    if (default_usemax >= MAXDEFUSES)
	fatal_error(FE_FULL_DEFUSES, 0);

    /*
     * Check to see if it is already there or not.
     */

    for (i = 0; i < default_usemax; i++)
	if (tokid == default_uses[i])
	    return;

    default_uses[default_usemax++] = tokid;

    /*
     * add use to all existing modules
     */

    for (i = nmods - 1; i >= 0; i--)
	adduse(i, tokid);
}

#ifdef PACKAGE
void
pckg_add_default_use(int tokid)
{
    int   i;

    if (default_usemax >= MAXDEFUSES)
	fatal_error(FE_FULL_DEFUSES, 0);

    /*
     * Check it whether it is already there or not.
     */
    for (i = 0; i < default_usemax; i++)
	if (tokid == default_uses[i])
	    return;

    default_uses[default_usemax++] = tokid;
}


/*
 * Get the default use module whose index in the default use
 * table is given.
 */

int
get_default_use(PWord n, PWord *m, PWord *mt)
    /* m, mt: default use module name */
{
    if (n >= 0 && n < default_usemax) {
	*m = (PWord) default_uses[n];
	*mt = WTP_SYMBOL;
	return (1);
    }
    else
	return (0);
}

#endif /* PACKAGE */


/*
 * add_default_proc adds to the list of things that names will be installed
 *      for when a module is created.
 *
 *      Q. Why is it important to install these names individually for each
 *         module?
 *      A. It is important to install individual names for all procedures which
 *         make use of the calling module.
 *
 *      Q. But won't these names entries be installed by the compiler if they
 *         are used in the program?
 *      A. Yes, this is usually the case.  But not when a construct like
 *         ...,call((p(X),q(X))),...   appears in the program.  It is important
 *         to call both p and q from the correct module.  This will not happen
 *         properly if ','/2 doesn't have a name entry in the calling module.
 *         Instead, the reference resolver would go to the defining module
 *         (of ','/2) and eventually use it to look up p and q in (which
 *         will probably be wrong).
 *
 */

void
add_default_proc(PWord tokid, int arity)
{
    int   i;

    if (default_procmax >= MAXDEFPROCS)
	fatal_error(FE_FULL_DEFPROC, 0);

    /*
     * Check whether it is there or not
     */

    for (i = 0; i < default_procmax; i++)
	if ((tokid == default_procs[i].tokid) &&
	    (arity == default_procs[i].arity))
	    return;

    default_procs[default_procmax].tokid = tokid;
    default_procs[default_procmax].arity = arity;
    default_procmax++;

    /*
     * install the name entry in everything that exists so far
     */

    for (i = nmods - 1; i >= 0; i--)
	w_namelookup(module_table[i].modid, tokid, arity);
}

#ifdef PACKAGE

/*
 * this is called from pckgload.c
 */
void
pckg_add_default_proc(PWord tokid, int arity)
{
    int   i;

    if (default_procmax >= MAXDEFPROCS)
	fatal_error(FE_FULL_DEFPROC);

    /*
     * Check whether it is already there or not.
     */
    for (i = 0; i < default_procmax; i++)
	if ((tokid == default_procs[i].tokid) &&
	    (arity == default_procs[i].arity))
	    return;

    default_procs[default_procmax].tokid = tokid;
    default_procs[default_procmax].arity = arity;
    default_procmax++;
}

/*
 * this is called from pckgload.c at package
 */

int
pckg_is_default_proc(PWord tokid, int arity)
{
    int   i;

    /*
     * Check whether it is there or not.
     */
    for (i = 0; i < default_procmax; i++)
	if ((tokid == default_procs[i].tokid) &&
	    (arity == default_procs[i].arity))
	    return (1);
    return (0);
}

/*
 * Get the default procedure whose index in the default procedure
 * table is given.
 */

int
get_default_proc(PWord n, PWord *p, PWord *pt, PWord *a, PWord *at)
    /* p, pt: default procedure name */
    /* a, at: default procedure arity */
{
    if (n >= 0 && n < default_procmax) {
	*p = (PWord) default_procs[n].tokid;
	*pt = WTP_SYMBOL;
	*a = (PWord) default_procs[n].arity;
	*at = WTP_INTEGER;
	return (1);
    }
    else
	return (0);
}


#endif /* PACKAGE */




/*
 * adduse adds a use definition to module u in module m.
 * m is assumed to be a module index, while u is a tokid.
 */

void
adduse(int m, int u)
{
    int   up;			/* use pointer */

    /*
     * Modules already use themselves without use declarations.
     */

    if (module_table[m].modid == u)
	return;

    /*
     * Walk the use list to see if module m is already using u.
     */

    up = module_table[m].nextuse;
    while (up != -1 && use_table[up].modid != u)
	up = use_table[up].nextuse;

    /* Is it used? */
    if (up != -1)
	return;

    /* Not used. Try and insert it. */

    if (nuses >= NUSEDEFS)
	fatal_error(FE_FULL_MODUSE, 0);

    use_table[nuses].modid = u;
    use_table[nuses].nextuse = module_table[m].nextuse;
    module_table[m].nextuse = nuses;
    nuses++;
}


void
export_pred(PWord mod, PWord tok, int arity)
    /* mod: module id */
    /* tok: token id  */
    /* arity: arity     */
{
    ntbl_entry *ent;

    ent = w_nametable[w_namelookup(mod, tok, arity)];
    ent->flags |= NMSK_EXPORT;
}


/*
 * createModuleClosureProcedure(name1,arity,name2)
 *
 *      Creates a procedure name1/arity which calls name2/arity+1 in
 *      which the arity+1'st argument is the module name of the caller.
 *
 *      Since this procedure is worthless without name1/arity being scattered
 *      throughout the rest of the modules, add_default_proc is automatically
 *      called.
 *
 */

void
createModuleClosureProcedure(PWord name1, int arity, PWord name2)
{
    ntbl_entry *ent1, *ent2;
    int   arity2;

    /* If one was a cutmacro, then up the arity. */
    if (isCutMacro(name1, arity))
		arity2 = arity + 2;
    else
		arity2 = arity + 1;

    ent1 = w_nametable[w_namelookup((PWord) cur_mod, name1, arity)];

    	/* Where we are going to */
    ent2 = w_nametable[w_namelookup((PWord) cur_mod, name2, arity2)];

    ent1->flags = NFLG_BUILTIN | NMSK_EXPORT | NFLG_BLT_MODCLOSURE;

    /* Don't want to do overflow code on destination, so go to ->code. */
    ic_install_module_closure(ent1, ent2->code);

    add_default_proc(name1, arity);
}

int
createModCloseProc(int tgtmod, PWord name1, int arity, PWord name2)
{
    ntbl_entry *ent1, *ent2;
    int   arity2;

    	/* If one was a cutmacro, then up the arity. */
    if (isCutMacro(name1, arity))
		arity2 = arity + 2;
    else
		arity2 = arity + 1;

		/* nametable entry for the "source" predicate: */
    ent1 = w_nametable[w_namelookup((PWord) tgtmod, name1, arity)];

		/* nametable entry to which we are mappying the "source" predicate: */
    ent2 = w_nametable[w_namelookup((PWord) tgtmod, name2, arity2)];

    ent1->flags = NFLG_BUILTIN | NMSK_EXPORT | NFLG_BLT_MODCLOSURE;


    /* Don't want to do overflow code on destination, so go to ->code. */
    ic_install_module_closure(ent1, ent2->code);

    add_default_proc(name1, arity);
    
    return 1;
}

#ifdef debugging

void
mod_stats(void)
{
    int   m, u;

    for (m = 0; m < nmods; m++) {
	PI_printf("Module: %s\n", TOKNAME(module_table[m].modid));
	u = module_table[m].nextuse;
	if (u == -1)
	    PI_printf("\tNo use declarations.\n");
	else {
	    PI_printf("\tHas use declarations for the following modules:\n");
	    while (u != -1) {
		PI_printf("\t\t%s\n", TOKNAME(use_table[u].modid));

		u = use_table[u].nextuse;
	    }
	}
    }
}

#endif /* debugging */


void
module_init(void)
{
    if (!module_stack) {
	module_stack = (int *)
	    ss_malloc(MAXMODNESTING * sizeof (int), FE_MODULE_INIT);
	clausegroup_stack = (int *)
	    ss_malloc(MAXMODNESTING * sizeof (int), FE_MODULE_INIT);
	module_table = (struct mtbl_entry *)
	    ss_malloc(NMODULES * sizeof (struct mtbl_entry), FE_MODULE_INIT);
	use_table = (struct use_entry *)
	    ss_malloc(NUSEDEFS * sizeof (struct use_entry), FE_MODULE_INIT);
	
	/* Allocate and initialize the default_uses structure */
	default_uses = (long *)
	    ss_malloc(MAXDEFUSES * sizeof (long), FE_MODULE_INIT);
	default_uses[0] = MODULE_GLOBAL;
	default_uses[1] = MODULE_BUILTINS;
	default_usemax = 2;

	/* Allocate and initialize the default_procs structure */
	default_procs = (struct defprocs *)
	    ss_malloc(MAXDEFPROCS * sizeof (struct defprocs), FE_MODULE_INIT);
	default_procs[0].tokid = TK_COMMA;
	default_procs[0].arity = 2;
	default_procs[1].tokid = TK_SCOLON;
	default_procs[1].arity = 2;
	default_procs[2].tokid = TK_IFARROW;
	default_procs[2].arity = 2;
	default_procs[3].tokid = TK_VBAR;
	default_procs[3].arity = 2;
	default_procs[4].tokid = TK_CALL;
	default_procs[4].arity = 1;
	default_procmax = 5;


	top_module = &module_stack[-1];
	top_clausegroup = &clausegroup_stack[0];
	*top_clausegroup = -1;

	/* create the user module (default) */
	new_mod((PWord) find_token((UCHAR *)"user"));

	/* create the builtins module */
	new_mod((PWord) find_token((UCHAR *)"builtins"));	

	/* pop builtins module off the stack */
	end_mod();

	/* leaving the user module as default */

	/* register the global variables in this module */
	ss_register_global((long *) &top_module);
	ss_register_global((long *) &module_stack);
	ss_register_global((long *) &top_clausegroup);
	ss_register_global((long *) &clausegroup_stack);
	ss_register_global((long *) &module_table);
	ss_register_global((long *) &use_table);
	ss_register_global((long *) &nmods);
	ss_register_global((long *) &nuses);
	ss_register_global((long *) &default_uses);
	ss_register_global((long *) &default_usemax);
	ss_register_global((long *) &default_procs);
	ss_register_global((long *) &default_procmax);
    }
}
