
/*
 * pckgload.c  -- load packaging code
 *
 * Copyright (c) 1989-1993, Applied Logic Systems, Inc.
 *
 * Author : Ilyas Cicekli
 * Date   : 3/24/89
 */

#include "defs.h"
#include "pckg.h"

#ifdef PACKAGE

/*
 * include this file if PACKAGE is defined.
 */

/*
 * #define PCKGLOAD_DEBUG                       1
 * #define PCKGLOAD_PRINT_PROCS         1
 */

#include <stdio.h>
#include <string.h>

#include "wintcode.h"
#include "module.h"


#define LongPtr(p)    (*(long **)&p)
#define ShortPtr(p)   (*(short **)&p)
#define CharPtr(p)    (*(char **)&p)



pckg_init(void)
{
    pckg_toktbl_init();
    pckg_modtbl_init();
    pckg_ntbl_init();
#if 0
    pckg_gvar_init();
    pckg_run_init_goal();
#endif
}



/*
 * Initialize the module table, the default use module table,
 * and the default procedure table from the package module table.
 */
pckg_modtbl_init(void)
{
    int   numofmods;		/* number of modulesin the package module
				 * table
				 */
    int   numofusemods;		/* number of modules in the use list of a
				 * module
				 */
    int   numofdefusemods;	/* number of package defult use modules */
    int   numofdefprocs;	/* number of default procedures */
    int   modtblidx, modid;	/* tokid and module table index of a module */
    int   tokid, arity;		/* tokid and arity of a procedure */
    char *modptr;		/* pointer to the package module table */

#ifdef PCKGLOAD_DEBUG
    printf("\n\nLoading Package Module Table ");
    fflush(stdout);
#endif /* PCKGLOAD_DEBUG */

    modptr = (char *) &pckg_modtbl;

    /*
     * Put modules in the package module table into
     * the module table.
     */
    numofmods = *(LongPtr(modptr))++;

#ifdef PCKGLOAD_DEBUG
    printf("\nNumber of Modules: %d", numofmods);
    fflush(stdout);
#endif /* PCKGLOAD_DEBUG */

    while (numofmods--) {

#ifdef PCKGLOAD_DEBUG
	printf("\nModule: %s ", ((char *) modptr));
	printf("\n    Use Modules: ");
	fflush(stdout);
#endif /* PCKGLOAD_DEBUG */

	modid = find_token((char *) modptr);
	modptr += strlen((char *) modptr) + 1;
	/*
	 * Fill the use list of the module with modules in the use list
	 * of the module in the package module table.
	 */
	modtblidx = mod_id(modid);
	modptr = (char *) (((long) modptr + 3) & 0xfffffffc);
	numofusemods = *(LongPtr(modptr))++;
	while (numofusemods--) {

#ifdef PCKGLOAD_DEBUG
	    printf(" %s ", ((char *) modptr));
	    fflush(stdout);
#endif /* PCKGLOAD_DEBUG */

	    adduse(modtblidx, find_token((char *) modptr));
	    modptr += strlen((char *) modptr) + 1;
	}
    }

    /*
     * Put default use modules in the package default use module table
     * into the default use module table.
     */
    modptr = (char *) (((long) modptr + 3) & 0xfffffffc);
    numofdefusemods = *(LongPtr(modptr))++;
    while (numofdefusemods--) {
	modid = find_token((char *) modptr);
	modptr += strlen((char *) modptr) + 1;
	pckg_add_default_use(modid);
    }

    /*
     * Put default procedures in the package default procedure table
     * into the default procedure table.
     */
    modptr = (char *) (((long) modptr + 3) & 0xfffffffc);
    numofdefprocs = *(LongPtr(modptr))++;
    while (numofdefprocs--) {
	tokid = find_token((char *) modptr);
	modptr += strlen((char *) modptr) + 1;
	modptr = (char *) (((long) modptr + 3) & 0xfffffffc);
	arity = *(LongPtr(modptr))++;
	pckg_add_default_proc(tokid, arity);
    }

    set_curr_package_module();

#ifdef PCKGLOAD_DEBUG
    printf("\nPackage Module Table Loaded \n");
    fflush(stdout);
#endif /* PCKGLOAD_DEBUG */

}



char  loaded_pckg_name[1024];	/* list of package names */

/*
 * Initialize the name table from name tables of packages.
 */
pckg_ntbl_init(void)
{
    long *pckg;			/* current package */
    long *pckg_array[MAX_PCKGS];	/* list of packages */
    int   numofpckgs;		/* number of packages */
    long *entptr;		/* pointer to package name table entry */
    ntbl_entry *ent;		/* package name table entry */
    int   numofprocs;		/* number of name table entries in a package */
    long *ca;			/* clause address */
    int   ntblidx, pckgidx;
    ntbl_entry *e;

#ifdef arch_m88k
    char *startaddr;
    char *endaddr;

#endif /* arch_m88k */

#ifdef PCKGLOAD_DEBUG
    printf("\n\nLoading Package Name Table ");
    fflush(stdout);
#endif /* PCKGLOAD_DEBUG */

    /*
     * Put all packages into the "pckg_array".
     */
    pckg = system_pckg;
    numofpckgs = -1;
    strcpy(loaded_pckg_name, "");
    while (!ENDOF_PCKGLIST(pckg)) {

#ifdef arch_m88k

	startaddr = (char *) (PCKG_CODE_START(pckg) & 0xfffff000);
	endaddr = (char *) (PCKG_CODE_END(pckg) & 0xfffff000);
#ifdef PCKGLOAD_DEBUG
	printf("\nPackage: %s  Code Start: %x (%x)  Code End: %x (%x) ",
	       PCKG_NAME(pckg), PCKG_CODE_START(pckg), startaddr, PCKG_CODE_END(pckg), endaddr);
	fflush(stdout);
#endif /* PCKGLOAD_DEBUG */
	if (pckg_addto_codeblocks(startaddr, (int) (endaddr - startaddr)) == -1) {
	    fprintf(stderr,
		    "Internal Error: Unable to add package code area to codeblocks.");
	    /* als_exit(1); */
	}

#endif /* arch_m88k */

	pckg_array[++numofpckgs] = pckg;
	strcat(loaded_pckg_name, PCKG_NAME(pckg));
	pckg = (long *) PCKG_PREVIOUS_PCKG(pckg);
	if (!ENDOF_PCKGLIST(pckg))
	    strcat(loaded_pckg_name, " ==> ");
    }

    /*
     * Put name table entries in packages into the name table
     * starting fom the oldest package.
     */
    for (pckgidx = numofpckgs; pckgidx >= 0; pckgidx--) {
	/*
	 * Get the address of the first name table entry
	 * in the current package, and the number of name table
	 * entries in that package.
	 */
	entptr = (long *) PCKG_NAMETBL(pckg_array[pckgidx]);
	numofprocs = *entptr++;

#ifdef PCKGLOAD_DEBUG
	printf("\n\n Loading Package: %s    Number of Procedures: %d ",
	       PCKG_NAME(pckg_array[pckgidx]), numofprocs);
	fflush(stdout);
#endif /* PCKGLOAD_DEBUG */

	/*
	 * Put the name table entries in the package name table into
	 * the name table.
	 */
	while (numofprocs--) {
	    /*
	     * Put the current name table entry in the package name table
	     * into the name table.
	     */
	    ent = (ntbl_entry *) (*entptr++);
	    ntblidx = nameprobe(ent->modid,
				MFUNCTOR_TOKID(ent->tokid_arity),
				MFUNCTOR_ARITY(ent->tokid_arity));

#ifdef PCKGLOAD_DEBUG
#ifdef PCKGLOAD_PRINT_PROCS
	    printf("\nProcedure: %s:%s/%d   ",
		   TOKNAME(ent->modid),
		   TOKNAME(MFUNCTOR_TOKID(ent->tokid_arity)),
		   MFUNCTOR_ARITY(ent->tokid_arity));
	    fflush(stdout);
#endif /* PCKGLOAD_PRINT_PROCS */
#endif /* PCKGLOAD_DEBUG */

	    if (ntblidx == -1)
		fatal_error(FE_PCKG_NTBL, 0);
	    else if (((e = w_nametable[ntblidx]) != (ntbl_entry *) 0) &&
		     ((e->flags & NMSK_USAGE) != NFLG_UNDEFINED) &&
		     ((e->flags & NMSK_USAGE) != NFLG_IMPORTED) &&
		     (((e->flags & NMSK_USAGE) != NFLG_BUILTIN) ||
		      !(pckg_is_default_proc(MFUNCTOR_TOKID(e->tokid_arity),
					MFUNCTOR_ARITY(e->tokid_arity))))) {
		fprintf(stderr, "Warning: Procedure %s:%s/%d is redefined. \n",
			TOKNAME(ent->modid),
			TOKNAME(MFUNCTOR_TOKID(ent->tokid_arity)),
			MFUNCTOR_ARITY(ent->tokid_arity));
		fprintf(stderr,
		     "         Its definition in the package %s is used.\n",
			PCKG_NAME(pckg_array[pckgidx]));
	    }

	    w_nametable[ntblidx] = ent;
	    /*
	     * Update the name table index (procIdx) areas in clauses of the
	     * current procedures with the new procedure index.
	     */
	    switch ((ent->flags) & NMSK_USAGE) {
		case NFLG_SINGLE:
		case NFLG_MULTIPLE:
		case NFLG_SWITCH:
		    ca = ent->first_clause;
		    while (ca != (long *) 0) {
#ifdef PCKGLOAD_PRINT_PROCS
			printf("+");
			printf(" %x", ca);
			fflush(stdout);
#endif /* PCKGLOAD_PRINT_PROCS */
			procIdx(ca) = (long) ntblidx;
			ca = nextClauseAddr(ca);
		    }
		    break;
		default:
		    break;
	    }
	}

#ifdef PCKGLOAD_DEBUG
	printf("\nPackage: %s  Loaded \n", PCKG_NAME(pckg_array[pckgidx]));
	fflush(stdout);
#endif /* PCKGLOAD_DEBUG */

    }

#ifdef PCKGLOAD_DEBUG
    printf("\nIndexing Package Procedures");
    fflush(stdout);
#endif /* PCKGLOAD_DEBUG */

    /*
     * Produce indexing for procedures.
     */
#ifdef Indexing
    gen_indexing();
#endif

    w_relinkall();		/* relink all procedures */

    /*
     * Increment w_timestamp so that abolishes in main.c works
     */
    w_timestamp++;

#ifdef PCKGLOAD_DEBUG
    printf("\nPackage Name Table Loaded \n");
    fflush(stdout);
#endif /* PCKGLOAD_DEBUG */

}

pckg_start(void)
{
    char *modptr, *goalptr;
    char  startpred[512];

    modptr = (char *) &pckg_start_pred;
    goalptr = modptr + strlen((char *) modptr) + 1;
    sprintf(startpred, "%s:%s", modptr, goalptr);

#ifdef PCKGLOAD_DEBUG
    printf("\n\nRunning Package Start Predicate: '%s' \n", startpred);
    fflush(stdout);
#endif /* PCKGLOAD_DEBUG */

    if (!exec_query_from_buf(startpred)) {
	fprintf(stderr, "\nError: Unable to run package start predicate %s",
		startpred);
    }
}

#endif /* PACKAGE */


/*
 * pckg_run_init_goal is used by the saved state mechanism too.
 */

void
pckg_run_init_goal(void)
{
    char  goal[1024];

    sprintf(goal, "builtins:'$initialize'");
    if (!exec_query_from_buf(goal)) {
	fprintf(stderr,
	  "\nError: Unable to run initialization predicate 'builtins:$initialize'");
    }
}
