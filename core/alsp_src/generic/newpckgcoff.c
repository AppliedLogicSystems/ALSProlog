
/*
 * pckgcoff.c   --      Creating coff files for application packaging
 *
 * Copyright (c) Applied Logic Systems, Inc.
 *
 * Author: Ilyas Cicekli
 * Date  : 6/4/1990
 */

// THREAD not used

#include "defs.h"

#ifdef PACKAGE


/*
 * Include this file only when PACKAGE is defined
 */

/*
 * #define PCKGCOFF_DEBUG  1
 */


#include <stdio.h>

#ifdef PCKG_OBJFORMAT_AOUT
#include <a.out.h>
#include <stab.h>
#endif /* PCKG_OBJFORMAT_AOUT */

#ifdef PCKG_OBJFORMAT_COFF
#include <sys/types.h>
#include <filehdr.h>
#include <scnhdr.h>
#include <reloc.h>
#include <linenum.h>
#include <syms.h>
#else  /* PCKG_OBJFORMAT_COFF */
#include "coffform.h"
#endif /* PCKG_OBJFORMAT_COFF */


#include "coerce.h"
#include "pckgcoff.h"
#include "rinfo.h"



/*
 * Symbol indexes for global variables which can appear
 * in Prolog code.
 */
long  coff_symidx_array[NUMOF_COFF_SYMIDX];


/********************************************************************
 *                                      COFF File Areas                                                                 *
 *******************************************************************/

/*
 * File Header
 */

static FILHDR file_header;


/*
 * Section Headers and Names
 */

#define NUMOF_SECTIONS 			3

static SCNHDR section_header[NUMOF_SECTIONS];

#define SEC_TEXT 	0
#define SEC_DATA 	1
#define SEC_BSS 	2

static char sec_names[NUMOF_SECTIONS][SYMNMLEN] =
{".text", ".data", ".bss"};
static short sec_types[NUMOF_SECTIONS] =
{STYP_TEXT, STYP_DATA, STYP_BSS};

#define SEC_TEXT_SYM_IDX 		2
#define SEC_DATA_SYM_IDX 		4
#define SEC_BSS_SYM_IDX 		6
#define FIRST_REGULAR_SYMIDX 	8


/*
 * Raw Data Area
 */

#define MAX_RAWDATA_SIZE 			0x800000	/* 8 Mbytes */
#define DEFAULT_RAWDATA_SIZE 		0x40000		/* 256 Kbytes */

static long rawdata_size;
static char *rawdata_start_addr;
static char *rawdata_end_addr;
static char *rawdata_addr;


/*
 * Relocation Entries
 */

#define MAX_NUMOF_RELOC_ENTRIES			0x40000		/* 256K
								 * entries
								 */
#define DEFAULT_NUMOF_RELOC_ENTRIES 	0x2000	/* 8192 entries */

static long numof_reloc_entries;
static RELOC *reloc_first_entry;
static RELOC *reloc_last_entry;
static RELOC *reloc_entry;


/*
 * Symbol Table
 */

#define MAX_NUMOF_SYMTBL_ENTRIES		0x40000		/* 256K
								 * entries
								 */
#define DEFAULT_NUMOF_SYMTBL_ENTRIES 	0x2000	/* 8192 entries */

static long numof_symtbl_entries;
static SYMENT *symtbl_first_entry;
static SYMENT *symtbl_last_entry;
static SYMENT *symtbl_entry;


/*
 * String Table
 */

#define MAX_STRTBL_SIZE 			0x400000	/* 4 Mbytes */
#define DEFAULT_STRTBL_SIZE 		0x20000		/* 128 Kbytes */

static long strtbl_size;
static char *strtbl_start_addr;
static char *strtbl_end_addr;
static char *strtbl_addr;


/*
 * Storage area size and address
 */

static long storage_size;
static char *storage_addr = (char *) -1;


/*
 * Hash Table (for Symbol Table) Pointers
 */

static long *hashtbl_start_addr;
static long *hashtbl_end_addr;
static long hashtbl_size;


#ifdef PCKGCOFF_DEBUG
static long hashtbl_request;
static long hashtbl_lookup;

#endif


#define INSERT_SYM(i,n) COFF_SYMIDX(i) = coff_insert_symbol(n,SYMDEF_REFERENCE);





/*
 * The procedure "coff_init_coff_file" allocates a storage area to store
 * raw data area, relocation table, symbol table and string table of
 * a COFF file. This procedures also initalizes these areas.
 */
coff_init_coff_file(fname, rawdatasize, reloctblsize, symtblsize, strtblsize)
    char *fname;		/* File name in the file header
				 *
				 */
    long  rawdatasize;		/* Size of raw data area (in Kbytes)
				 */
    long  reloctblsize;		/* Number of relocation entries
				 *
				 */
    long  symtblsize;		/* Number of symbols
				 *
				 */
    long  strtblsize;		/* Size of string table (in Kbytes)
				 */
{
#ifdef PCKGCOFF_DEBUG
    printf("\nInitializing COFF File areas\n");
#endif

    /*
     * Process arguments
     */

#ifndef RAWDATA_IN_FILE
    if (rawdatasize == -1)
	rawdata_size = DEFAULT_RAWDATA_SIZE;
    else if ((rawdatasize > 0) && (rawdatasize <= MAX_RAWDATA_SIZE))
	rawdata_size = rawdatasize * 1024;
    else {
	fprintf(stderr,
		"\nWarning: Illegal Raw Data Area Size. ");
	fprintf(stderr,
		"\n         Default Raw Data Area Size (%d Kbytes) used. ",
		(DEFAULT_RAWDATA_SIZE / 1024));
	fflush(stderr);
	rawdata_size = DEFAULT_RAWDATA_SIZE;
    }
#endif /* RAWDATA_IN_FILE */

    if (reloctblsize == -1)
	numof_reloc_entries = DEFAULT_NUMOF_RELOC_ENTRIES;
    else if ((reloctblsize > 0) && (reloctblsize <= MAX_NUMOF_RELOC_ENTRIES))
	numof_reloc_entries = reloctblsize * 1024;
    else {
	fprintf(stderr,
		"\nWarning: Illegal Relocation Table Size. ");
	fprintf(stderr,
	     "\n         Default Relocation Table Size (%d entries) used. ",
		DEFAULT_NUMOF_RELOC_ENTRIES);
	fflush(stderr);
	numof_reloc_entries = DEFAULT_NUMOF_RELOC_ENTRIES;
    }

    if (symtblsize == -1)
	numof_symtbl_entries = DEFAULT_NUMOF_SYMTBL_ENTRIES;
    else if ((symtblsize > 0) && (symtblsize <= MAX_NUMOF_SYMTBL_ENTRIES))
	numof_symtbl_entries = symtblsize * 1024;
    else {
	fprintf(stderr,
		"\nWarning: Illegal Symbol Table Size. ");
	fprintf(stderr,
		"\n         Default Symbol Table Size (%d symbols) used. ",
		DEFAULT_NUMOF_SYMTBL_ENTRIES);
	fflush(stderr);
	numof_symtbl_entries = DEFAULT_NUMOF_SYMTBL_ENTRIES;
    }

    if (strtblsize == -1)
	strtbl_size = DEFAULT_STRTBL_SIZE;
    else if ((strtblsize > 0) && (strtblsize <= MAX_STRTBL_SIZE))
	strtbl_size = strtblsize * 1024;
    else {
	fprintf(stderr,
		"\nWarning: Illegal String Table Size. ");
	fprintf(stderr,
		"\n         Default String Table Size (%d Kbytes) used. ",
		(DEFAULT_STRTBL_SIZE / 1024));
	fflush(stderr);
	strtbl_size = DEFAULT_STRTBL_SIZE;
    }

    /*
     * Allocate storage area to store raw data area, relocation entries,
     * symbol table, and string table.
     */
#ifndef RAWDATA_IN_FILE
    storage_size = rawdata_size +
#else  /* RAWDATA_IN_FILE */
    storage_size =
#endif /* RAWDATA_IN_FILE */
	(numof_reloc_entries * sizeof (RELOC)) +
	(numof_symtbl_entries * sizeof (SYMENT)) +
	strtbl_size +
	((numof_symtbl_entries + 1) * sizeof (long));

    /*
     * Deallocate the previous area
     */
    if (storage_addr != (char *) -1) {
	free((char *) storage_addr);
	storage_addr = (char *) -1;
    }

    if ((storage_addr = (char *) malloc(storage_size)) != (char *) NULL) {
#ifndef RAWDATA_IN_FILE
	/*
	 * Raw Data Area Pointers
	 */
	rawdata_start_addr = (char *) storage_addr;
	rawdata_end_addr = rawdata_start_addr + (rawdata_size - 1);
	rawdata_addr = rawdata_start_addr;
	/*
	 * Relocation Table Pointers
	 */
	reloc_first_entry = (RELOC *) (rawdata_end_addr + 1);
#else  /* RAWDATA_IN_FILE */
	reloc_first_entry = (RELOC *) storage_addr;
#endif /* RAWDATA_IN_FILE */
	reloc_last_entry = reloc_first_entry + (numof_reloc_entries - 1);
	reloc_entry = reloc_first_entry;
	/*
	 * Symbol Table Pointers
	 */
	symtbl_first_entry = (SYMENT *) (reloc_last_entry + 1);
	symtbl_last_entry = symtbl_first_entry + (numof_symtbl_entries - 1);
	symtbl_entry = symtbl_first_entry;
	/*
	 * String Table Pointers
	 */
	strtbl_start_addr = (char *) (symtbl_last_entry + 1);
	strtbl_end_addr = strtbl_start_addr + (strtbl_size - 1);
	strtbl_addr = strtbl_start_addr;
	/*
	 * Hash Table Pointers
	 */
	hashtbl_start_addr = (long *) (strtbl_end_addr + 1);
	hashtbl_size = (numof_symtbl_entries + 1);
	hashtbl_end_addr = (long *) hashtbl_start_addr + (hashtbl_size - 1);
    }
    else {
	fprintf(stderr,
	     "\nWarning: Unable to allocate temporary area for COFF file.");
	fflush(stderr);
	storage_addr = (char *) -1;
	return (-1);
    }

#ifdef 	RAWDATA_IN_FILE
    /*
     * Rawdata will be in a file.
     * This file will be .o file when we are creating COFF or
     * AOUT files. But it will be a temporary file when we
     * are creating OMF86 files.
     */

#ifdef PCKG_OBJFORMAT_OMF86
    if (open_tmp_pckg_file())
	!=-1) {
#else  /* PCKG_OBJFORMAT_OMF86 */
    if (open_pckg_file(fname))
	!=-1) {			/* } for vi */
#endif /* PCKG_OBJFORMAT_OMF86 */
	rawdata_start_addr = 0;
	rawdata_addr = rawdata_start_addr;
	}
    else {
	free((char *) storage_addr);
	storage_addr = (char *) -1;
	return (-1);
    }
#endif /* RAWDATA_IN_FILE */

    /*
     * Initialize Symbol Table
     */
    coff_init_symtbl(fname);

    /*
     * Initialize String Table
     */
    *LongPtr(strtbl_addr)++ = sizeof (long);

    /*
     * Initialize Hash Table
     */
    {
	register long *p;

	for (p = hashtbl_start_addr; p <= hashtbl_end_addr; p++)
	    *p = 0;
    }

    /*
     * Initialize global symbols
     */
#ifdef arch_i386
    INSERT_SYM(symidx_wm_safety, "wm_safety")
	INSERT_SYM(symidx_wm_b_reg, "wm_b_reg")
	INSERT_SYM(symidx_wm_heapbase, "wm_heapbase")
	INSERT_SYM(symidx_wm_docut, "wm_docut")
	INSERT_SYM(symidx_OverflowPtr, "OverflowPtr")
	INSERT_SYM(symidx_UnifyPtr, "UnifyPtr")
	INSERT_SYM(symidx_wm_try_me, "wm_try_me")
	INSERT_SYM(symidx_wm_retry_me, "wm_retry_me")
	INSERT_SYM(symidx_wm_trust_me, "wm_trust_me")
	INSERT_SYM(symidx_wm_g_sym, "wm_g_sym")
	INSERT_SYM(symidx_wm_g_uia, "wm_g_uia")
	INSERT_SYM(symidx_wm_p_uia, "wm_p_uia")
	INSERT_SYM(symidx_wm_u_sym, "wm_u_sym")
	INSERT_SYM(symidx_wm_resolve_ref, "wm_resolve_ref")
	INSERT_SYM(symidx_call_mod_closure, "call_mod_closure")
	INSERT_SYM(symidx_wm_execute_builtin, "wm_execute_builtin")
#endif /* arch_i386 */

#ifdef arch_m68k
	INSERT_SYM(symidx_wm_heapbase, "_wm_heapbase")
	INSERT_SYM(symidx_wm_docut, "_wm_docut")
	INSERT_SYM(symidx_wm_g_sym, "_wm_g_sym")
	INSERT_SYM(symidx_wm_g_uia, "_wm_g_uia")
	INSERT_SYM(symidx_wm_p_uia, "_wm_p_uia")
	INSERT_SYM(symidx_wm_resolve_ref, "_wm_resolve_ref")
	INSERT_SYM(symidx_wm_execute_builtin, "_wm_execute_builtin")
	INSERT_SYM(symidx_wm_unify, "_wm_unify")
	INSERT_SYM(symidx_wm_overflow, "_wm_overflow")
	INSERT_SYM(symidx_wm_g_int, "_wm_g_int")
	INSERT_SYM(symidx_cmp_sym_uia, "_cmp_sym_uia")
#endif /* arch_m68k */

#ifdef DOS
    {
	/*
	 * THE ONLY REASON THIS TEST IS HERE THAT STUPID MetaWare HighC
	 * COMPILER CANNOT COMPILE PROPERLY CODE ABOVE(INSERT_SYM)
	 * IN SOME CASES.
	 *
	 * Ilyas -- 5/7/1991
	 */
	int   i;

	for (i = 0; i < NUMOF_COFF_SYMIDX; i++) {
	    if (COFF_SYMIDX(i) != (i + FIRST_REGULAR_SYMIDX))
		fprintf(stderr, "Error: Stupid MetaWare HighC Compiler Error");
	}
    }
#endif /* DOS */

#ifdef PCKGCOFF_DEBUG
    printf("\nC_I_FILE: fname=%s ", fname);
    printf("\nC_I_FILE: SYMESZ=%d %d  RELSZ=%d %d",
	   SYMESZ, sizeof (SYMENT), RELSZ, sizeof (RELOC));
    printf("\nC_I_FILE: RawDataArea: %x %x %x   %x",
	   rawdata_start_addr, rawdata_end_addr, rawdata_addr, rawdata_size);
    printf("\nC_I_FILE: RelocTbl   : %x %x %x   %x",
     reloc_first_entry, reloc_last_entry, reloc_entry, numof_reloc_entries);
    printf("\nC_I_FILE: SymTbl     : %x %x %x   %x",
	   symtbl_first_entry, symtbl_last_entry, symtbl_entry, numof_symtbl_entries);
    printf("\nC_I_FILE: StrTbl     : %x %x %x   %x",
	   strtbl_start_addr, strtbl_end_addr, strtbl_addr, strtbl_size);
    printf("\nC_I_FILE: HashTbl    : %x %x    %x",
	   hashtbl_start_addr, hashtbl_end_addr, hashtbl_size);
    hashtbl_request = 0;
    hashtbl_lookup = 0;
#endif

    return (1);
}




/*
 * Initialize the symbol table by putting first four special symbols
 * ".file", ".text", ".data" and ".bss".
 */
coff_init_symtbl(fname)
    char *fname;		/* File Name */
{
    int   i;
    char *chptr;
    AUXENT *auxs;

    /*
     * Put the symbol ".file" into the symbol table
     */
    strncpy(symtbl_entry->n_name, ".file", SYMNMLEN);
    symtbl_entry->n_value = (long) 0;
    symtbl_entry->n_scnum = (short) N_DEBUG;
    symtbl_entry->n_type = (short) T_NULL;
    symtbl_entry->n_sclass = (char) C_FILE;
    symtbl_entry->n_numaux = (char) 1;
    symtbl_entry++;
    /* Auxiliary entry of the symbol ".file" */
    auxs = (AUXENT *) symtbl_entry;
    strncpy(auxs->x_file.x_fname, fname, FILNMLEN);
    for (i = FILNMLEN, chptr = (char *) (CharPtr(auxs) + FILNMLEN);
	 i < sizeof (SYMENT); i++, chptr++)
	*chptr = 0;
    symtbl_entry++;

    /*
     * Put the special section symbols ".text", ".data" and ".bss"
     * into the symbol table
     */
    for (i = 1; i <= NUMOF_SECTIONS; i++) {
	/* Put the section name into the symbol table */
	strncpy(symtbl_entry->n_name, sec_names[(i - 1)], SYMNMLEN);
	symtbl_entry->n_value = (long) 0;
	symtbl_entry->n_scnum = (short) i;
	symtbl_entry->n_type = (short) T_NULL;
	symtbl_entry->n_sclass = (char) C_STAT;
	symtbl_entry->n_numaux = (char) 1;
	symtbl_entry++;
	/* Auxiliary entry of the section symbol */
	symtbl_entry->n_zeroes = (long) 0;
	symtbl_entry->n_offset = (long) 0;
	symtbl_entry->n_value = (long) 0;
	symtbl_entry->n_scnum = (short) 0;
	symtbl_entry->n_type = (short) 0;
	symtbl_entry->n_sclass = (char) 0;
	symtbl_entry->n_numaux = (char) 0;
	symtbl_entry++;
    }

    return (1);
}



/*
 * The procedure "coff_find_symbol" tries to find the given symbol
 * in the symbol table. If the symbol is in the symbol table,
 * the symbol table entry of that symbol is returned. Otherwise,
 * this procedure returns 0.
 */
SYMENT *coff_find_symbol(sym_name, sym_len)
    char *sym_name;
    long  sym_len;
{
    register SYMENT *sym;	/* a symbol table entry pointer */
    register unsigned long idx;
    register unsigned long hash_incr;
    unsigned long start;
    char *strptr;

#ifdef PCKGCOFF_DEBUG
    hashtbl_request++;
    hashtbl_lookup++;
    if ((hashtbl_request & (long) 0x02FF) == 0) {
	printf("\nFIND_SYM:   numof_syms: %d   request=%d   lookup=%d ",
	       (unsigned long) (symtbl_entry - symtbl_first_entry),
	       hashtbl_request, hashtbl_lookup);
    }
#endif

    {
	register unsigned char *s;
	register long l;

	s = (unsigned char *) sym_name;
	l = sizeof (long);
	idx = (unsigned long) sym_len;
	while (l <= sym_len) {
	    idx += *(unsigned long *) s;
	    l += sizeof (long);
	    s += sizeof (long);
	}
	for (; *s; idx += (*s++)) ;
    }
    idx *= sym_len;
    hash_incr = (idx % (hashtbl_size / sym_len)) + 1;
    idx = (idx % hashtbl_size);
    start = idx;

    for (; ((sym = (SYMENT *) * (hashtbl_start_addr + idx)) != (SYMENT *) 0);) {
	if (sym_len > SYMNMLEN) {
	    if (sym->n_zeroes == 0) {
		strptr = (char *) strtbl_start_addr + sym->n_offset;
		if (strcmp(sym_name, strptr) == 0)
		    return (sym);
	    }
	}
	else {
	    if (sym->n_zeroes != 0 &&
		strncmp(sym_name, sym->n_name, SYMNMLEN) == 0)
		return (sym);
	}

	idx = (idx + hash_incr) % hashtbl_size;
	if (idx == start) {
	    fprintf(stderr,
		    "\nError: COFF hash table full.");
	    fflush(stderr);
	    als_exit(1);
	}
#ifdef PCKGCOFF_DEBUG
	hashtbl_lookup++;
#endif
    }

    /* Not Found */
    *(hashtbl_start_addr + idx) = (long) symtbl_entry;
    return ((SYMENT *) 0);

}



/*
 * The procedure "coff_insert_symbol" inserts the given symbol into
 * the symbol table. If it is successful, it returns symbol index.
 * Otherwise, it gives a warning message and returns -1.
 */
coff_insert_symbol(sym_name, sym_def_flag)
    char *sym_name;		/* symbol name
				 *
				 */
    long  sym_def_flag;		/* type of symbol definition
				 */
{
    long  sym_len;		/* length of symbol (not including null
				 * character)
				 */
    SYMENT *sym;		/* a symbol table entry pointer */

    sym_len = strlen(sym_name);

    /*
     * Allocate symbol table entry (if it is required)
     */
    if ((sym = (SYMENT *) coff_find_symbol(sym_name, sym_len)) == (SYMENT *) 0) {
	if (symtbl_entry > symtbl_last_entry) {
	    fprintf(stderr,
		    "\nWarning: Symbol Table of COFF file is full.");
	    fflush(stderr);
	    return (-1);
	}
	sym = symtbl_entry++;
	/*
	 * Copy the symbol name into its location in the symbol table entry,
	 * or copy it into the string table and put the offset into
	 * its location in the symbol table entry.
	 */
	if (sym_len <= SYMNMLEN)
	    strncpy(sym->n_name, sym_name, SYMNMLEN);
	else {
	    if (strtbl_addr + sym_len + 1 > strtbl_end_addr) {
		fprintf(stderr,
			"\nWarning: String Table of COFF file is full.");
		fflush(stderr);
		return (-1);
	    }
	    sym->n_zeroes = (long) 0;
	    sym->n_offset = (long) (strtbl_addr - strtbl_start_addr);
	    strcpy(strtbl_addr, sym_name);
	    strtbl_addr += (sym_len + 1);
	}
	/*
	 * Initialize the symbol
	 */
	sym->n_value = (long) 0;
	sym->n_scnum = (short) 0;
	sym->n_type = (short) T_NULL;
	sym->n_sclass = (char) C_NULL;
	sym->n_numaux = (char) 0;
    }
    /*
     * Check the symbol definition flag and
     * update the symbol table entry properly.
     */
    switch (sym_def_flag) {
	case SYMDEF_DECLARE:
	    sym->n_value = (long) (rawdata_addr - rawdata_start_addr);
	    sym->n_scnum = (short) (SEC_DATA + 1);
	    if (sym->n_sclass == (char) C_NULL)
		sym->n_sclass = (char) C_STAT;
	    break;
	case SYMDEF_DECLARE_GLOBAL:
	    sym->n_value = (long) (rawdata_addr - rawdata_start_addr);
	    sym->n_scnum = (short) (SEC_DATA + 1);
	    sym->n_sclass = (char) C_EXT;
	    break;
	case SYMDEF_REFERENCE:
	    break;
defult:
	    break;
    }
    /*
     * Return the symbol table index
     */
    return ((int) (sym - symtbl_first_entry));
}



/*
 * The procedure "coff_insert_rawdata" copies the given code block
 * whose boundaries are given into the raw data area of the COFF file.
 * If it is unable to copy that code block, it returns -1.
 * If it is successfule, it returns 1.
 */
coff_insert_rawdata(from, to)
    register char *from;
    register char *to;
{
#ifndef 	RAWDATA_IN_FILE
    register char *p;
    register char *tominus3;

    if ((rawdata_addr + (int) (to - from)) > rawdata_end_addr) {
	fprintf(stderr,
		"\nWarning: Raw Data Area of COFF file is full.");
	fflush(stderr);
	return (-1);
    }

    tominus3 = to - 3;
    for (p = rawdata_addr; from < tominus3;) {
	*(long *) p = *(long *) from;
	p += sizeof (long);
	from += sizeof (long);
    }

    for (; from < to; p++, from++)
	*p = *from;

    rawdata_addr = p;

#else  /* RAWDATA_IN_FILE */
    fwrite((void *) from, (int) (to - from), 1, rawdata_fd);
    rawdata_addr += (int) (to - from);
#endif /* RAWDATA_IN_FILE */

    return (1);
}


coff_insert_string_rawdata(val)
    char *val;
{
    long  n;

    n = strlen(val) + 1;
    return (coff_insert_rawdata(val, (val + n)));
}


coff_insert_char_rawdata(val)
    char  val;
{
#ifndef 	RAWDATA_IN_FILE
    if ((rawdata_addr + sizeof (char)) > rawdata_end_addr) {
	fprintf(stderr,
		"\nWarning: Raw Data Area of COFF file is full.");
	fflush(stderr);
	return (-1);
    }
    *CharPtr(rawdata_addr)++ = val;
#else  /* RAWDATA_IN_FILE */
    fwrite((void *) &val, sizeof (char), 1, rawdata_fd);
    rawdata_addr += sizeof (char);
#endif /* RAWDATA_IN_FILE */
    return (1);
}


coff_insert_short_rawdata(val)
    short val;
{
#ifndef 	RAWDATA_IN_FILE
    if ((rawdata_addr + sizeof (short)) > rawdata_end_addr) {
	fprintf(stderr,
		"\nWarning: Raw Data Area of COFF file is full.");
	fflush(stderr);
	return (-1);
    }
    *ShortPtr(rawdata_addr)++ = val;
#else  /* RAWDATA_IN_FILE */
    fwrite((void *) &val, sizeof (short), 1, rawdata_fd);
    rawdata_addr += sizeof (short);
#endif /* RAWDATA_IN_FILE */
    return (1);
}


coff_insert_long_rawdata(val)
    long  val;
{
#ifndef 	RAWDATA_IN_FILE
    if ((rawdata_addr + sizeof (long)) > rawdata_end_addr) {
	fprintf(stderr,
		"\nWarning: Raw Data Area of COFF file is full.");
	fflush(stderr);
	return (-1);
    }
    *LongPtr(rawdata_addr)++ = val;
#else  /* RAWDATA_IN_FILE */
    fwrite((void *) &val, sizeof (long), 1, rawdata_fd);
    rawdata_addr += sizeof (long);
#endif /* RAWDATA_IN_FILE */
    return (1);
}


coff_align4_rawdata() {
    while ((LongVal(rawdata_addr) & 0x03) != 0) {
	if (coff_insert_char_rawdata(0) == -1)
	    return (-1);
    }
    return (1);
}


/*
 * The procedure "coff_insert_reloc" inserts a relocation entry
 * into the relocation table of the COFF file. If it is unable
 * to insert an entry, it prints a warning message and reutrns -1.
 * Otherwise it inserts the entry and returns 1.
 */
coff_insert_reloc(sym_name)
    char *sym_name;
{
    long  sym_idx;

    if (reloc_entry > reloc_last_entry) {
	fprintf(stderr,
		"\nWarning: Relocation Table of COFF file is full.");
	fflush(stderr);
	return (-1);
    }

    if ((sym_idx = coff_insert_symbol(sym_name, SYMDEF_REFERENCE)) != -1) {
	reloc_entry->r_vaddr = (long) (rawdata_addr - rawdata_start_addr);
	reloc_entry->r_symndx = (long) sym_idx;
	reloc_entry->r_type = (short) R_DIR32;
	reloc_entry++;
    }
    else
	return (-1);

    return (1);
}


coff_insert_reloc_symidx(sym_idx)
    long  sym_idx;
{
    if (reloc_entry > reloc_last_entry) {
	fprintf(stderr,
		"\nWarning: Relocation Table of COFF file is full.");
	fflush(stderr);
	return (-1);
    }

    reloc_entry->r_vaddr = (long) (rawdata_addr - rawdata_start_addr);
    reloc_entry->r_symndx = (long) sym_idx;
    reloc_entry->r_type = (short) R_DIR32;
    reloc_entry++;

    return (1);
}





#ifdef PCKG_OBJFORMAT_COFF

/****************************************************************************
 *                                                      Create COFF File                                                                *
 ****************************************************************************/


open_pckg_file(fname))
    char *fname;
{
    if ((rawdata_fd = fopen(fname, "w+")) == NULL) {
	fprintf(stderr,
		"\nWarning: Unable to open file %s for write access.",
		fname);
	fflush(stderr);
	free((char *) storage_addr);	/* Free the malloced area */
	storage_addr = (char *) -1;
	return (-1);
    }
}



coff_create_coff_file(fname)
    char *fname;
{
    FILE *fd;
    long  i;
    register RELOC *r;
    register SYMENT *s;
    AUXENT *auxs;

    /*
     * Long Word Boundary
     */
    while (((long) rawdata_addr & 0x03) != 0)
	coff_insert_char_rawdata(0);

    /*
     * Initialize File Header
     */
    file_header.f_magic = (short) I386MAGIC;
    file_header.f_nscns = (short) NUMOF_SECTIONS;
    file_header.f_timdat = (long) time(0);
    file_header.f_symptr = (long) (FILHSZ + (NUMOF_SECTIONS * SCNHSZ) +
		     ((rawdata_addr - rawdata_start_addr) * sizeof (char)) +
			       ((reloc_entry - reloc_first_entry) * RELSZ));
    file_header.f_nsyms = (long) (symtbl_entry - symtbl_first_entry);
    file_header.f_opthdr = 0;
    file_header.f_flags = (short) (F_LNNO | F_AR32WR);

    /*
     * Initilaze Section Headers
     */
    for (i = 0; i < NUMOF_SECTIONS; i++) {
	strncpy(section_header[i].s_name, sec_names[i], SYMNMLEN);
	section_header[i].s_paddr = 0;
	section_header[i].s_vaddr = 0;
	section_header[i].s_size = 0;
	section_header[i].s_scnptr = 0;
	section_header[i].s_relptr = 0;
	section_header[i].s_lnnoptr = 0;
	section_header[i].s_nreloc = 0;
	section_header[i].s_nlnno = 0;
	section_header[i].s_flags = (short) sec_types[i];
    }

    /*
     * Fill Section Header of Section ".data"
     */
    section_header[SEC_DATA].s_size = (long)
	((rawdata_addr - rawdata_start_addr) * sizeof (char));
    section_header[SEC_DATA].s_scnptr = (long) (FILHSZ + NUMOF_SECTIONS * SCNHSZ);
    section_header[SEC_DATA].s_relptr = (long) (FILHSZ + NUMOF_SECTIONS * SCNHSZ +
		     ((rawdata_addr - rawdata_start_addr) * sizeof (char)));
    section_header[SEC_DATA].s_nreloc = (short) (reloc_entry - reloc_first_entry);
    /*
     * Update auxiliary symbol table entry of symbol ".data"
     */
    auxs = (AUXENT *) (symtbl_first_entry + (SEC_DATA_SYM_IDX + 1));
    auxs->x_scn.x_scnlen = (long) section_header[SEC_DATA].s_size;
    auxs->x_scn.x_nreloc = (short) section_header[SEC_DATA].s_nreloc;
    auxs->x_scn.x_nlinno = (short) section_header[SEC_DATA].s_nlnno;


    /*
     * Fill Section Header of Section ".bss"
     */
    section_header[SEC_BSS].s_paddr = section_header[SEC_DATA].s_size;
    section_header[SEC_BSS].s_vaddr = section_header[SEC_DATA].s_size;
    /*
     * Update symbol table entry of symbol ".bss"
     */
    s = (symtbl_first_entry + SEC_BSS_SYM_IDX);
    s->n_value = section_header[SEC_DATA].s_size;

    /*
     * Update relocation entries for local variables
     */
    for (r = reloc_first_entry; r < reloc_entry; r++) {
	s = (SYMENT *) (symtbl_first_entry + r->r_symndx);
	if (s->n_scnum) {
	    *(long *) (rawdata_start_addr + r->r_vaddr) = s->n_value;
	    r->r_symndx = SEC_DATA_SYM_IDX;
	}
    }

    /*
     * Update Symbol Table for External Symbols
     */
    for (s = symtbl_first_entry; s < symtbl_entry; s++) {
	if (!(s->n_scnum))
	    s->n_sclass = (char) C_EXT;
	s += s->n_numaux;
    }

    /*
     * Update String Table Size
     */
    *LongPtr(strtbl_start_addr) = (long) (strtbl_addr - strtbl_start_addr);

    /*
     * Create the real COFF file
     */
    {
	unsigned long rawdata_sz, reltbl_sz, symtbl_sz, strtbl_sz;

	rawdata_sz = (unsigned long) (rawdata_addr - rawdata_start_addr);
	reltbl_sz = (unsigned long) (reloc_entry - reloc_first_entry);
	symtbl_sz = (unsigned long) (symtbl_entry - symtbl_first_entry);
	strtbl_sz = (unsigned long) (strtbl_addr - strtbl_start_addr);

#ifdef PCKGCOFF_DEBUG
	printf("\nC_C_C_F: rawdata_sz=%x  reltbl_sz=%x  symtbl_sz=%x  strtbl_sz=%x",
	       rawdata_sz, reltbl_sz, symtbl_sz, strtbl_sz);
	printf("\nC_C_C_F: hashtbl_request=%d  hashtbl_lookup=%d ",
	       hashtbl_request, hashtbl_lookup);
#endif

	if ((fd = fopen(fname, "w")) != NULL) {
	    /*
	     * Write File Header
	     */
	    fwrite((void *) &file_header, FILHSZ, 1, fd);
	    /*
	     * Write Section Headers
	     */
	    for (i = 0; i < NUMOF_SECTIONS; i++)
		fwrite((void *) &section_header[i], SCNHSZ, 1, fd);
	    /*
	     * Write Raw Data Area
	     */
	    fwrite((void *) rawdata_start_addr, rawdata_sz, 1, fd);
	    /*
	     * Write Relocation Entries
	     */
	    for (r = reloc_first_entry; r < reloc_entry; r++)
		fwrite((void *) r, RELSZ, 1, fd);
	    /*
	     * Write Symbol Table Entries
	     */
	    for (s = symtbl_first_entry; s < symtbl_entry; s++)
		fwrite((void *) s, SYMESZ, 1, fd);
	    /*
	     * Write String Table
	     */
	    fwrite((void *) strtbl_start_addr, strtbl_sz, 1, fd);
	}
	else {
	    fprintf(stderr,
		    "\nWarning: Unable to open file %s for write access.",
		    fname);
	    fflush(stderr);
	    free((char *) storage_addr);	/* Free the malloced area */
	    storage_addr = (char *) -1;
	    return (-1);
	}
    }

    /*
     * Close the file and free the malloced area
     */
    fclose(fd);
    free((char *) storage_addr);
    storage_addr = (char *) -1;

    return (1);
}

#endif /* PCKG_OBJFORMAT_COFF */



#ifdef PCKG_OBJFORMAT_OMF86

/****************************************************************************
 *                                                      Create OMF86 File                                                               *
 ****************************************************************************/


/*
 * An OMF86 Record
 */
#define MAX_OMF86_REC_SIZE 		0x0900

static unsigned char omf86_record[MAX_OMF86_REC_SIZE];


/*
 * OMF86 Object Record Types
 */
#define RT_THEADR 	0x80
#define RT_COMENT	0x88
#define RT_MODEND 	0x8A
#define RT_EXTDEF	0x8C
#define RT_TYPDEF 	0x8E
#define RT_PUBDEF 	0x90
#define RT_LINNUM 	0x94
#define RT_LNAMES	0x96
#define RT_SEGDEF	0x98
#define RT_GRPDEF	0x9A
#define RT_FIXUPP	0x9C
#define RT_LEDATA	0xA0
#define RT_LIDATA	0xA2
#define	RT_COMDEF	0xB0


/*
 * COMENT Record Macros
 */
#define COMENT_ATTRIB 			0x80
#define COMENT_CLASS 			0xAA
#define COMENT_STRING 			"80386"


/*
 * LNAMES Record Macros
 */
#define LNAMES_NULL_IDX 		1
#define LNAMES_SEG_DATA_IDX 	2
#define LNAMES_CLS_DATA_IDX 	3
#define LNAMES_SEG_BSS_IDX 		4
#define LNAMES_CLS_BSS_IDX 		5
#define LNAMES_SEG_CONST_IDX 	6
#define LNAMES_GRP_DGROUP_IDX	7
#define LNAMES_CLS_CONST_IDX 	8
#define LNAMES_SEG_TEXT_IDX 	9
#define LNAMES_CLS_CODE_IDX 	10

#define NUMOF_LNAMES 			10
#define SIZEOF_LNAMES 			8

static char LNAMES_NAMES[NUMOF_LNAMES][SIZEOF_LNAMES] =
{
    "", "_DATA", "DATA", "_BSS", "BSS", "CONST", "DGROUP", "CONST", "_TEXT", "CODE"
};


/*
 * SEGDEF Record Macros
 */
#define NUMOF_SEGS				4

#define SEGDEF_ATTRIB 			0xA8

#define SEGDEF_DATASEG_IDX 		1
#define SEGDEF_BSSSEG_IDX 		2
#define SEGDEF_CONSTSEG_IDX 	3
#define SEGDEF_TEXTSEG_IDX 		4

static unsigned char SEGDEF_SEG_IDX[NUMOF_SEGS] = {
    LNAMES_SEG_DATA_IDX,
	LNAMES_SEG_BSS_IDX,
	LNAMES_SEG_CONST_IDX,
	LNAMES_SEG_TEXT_IDX
};

static unsigned char SEGDEF_CLS_IDX[NUMOF_SEGS] = {
    LNAMES_CLS_DATA_IDX,
	LNAMES_CLS_BSS_IDX,
	LNAMES_CLS_CONST_IDX,
	LNAMES_CLS_CODE_IDX
};

static unsigned char SEGDEF_OVL_IDX[NUMOF_SEGS] = {
    LNAMES_NULL_IDX,
	LNAMES_NULL_IDX,
	LNAMES_NULL_IDX,
	LNAMES_NULL_IDX
};


/*
 * TYPDEF Record Macros
 */
#define TYPDEF_NOTYPE_IDX 			0


/*
 * GRPDEF Record Macros
 */
#define GRPDEF_NOGROUP_IDX			0
#define GRPDEF_DGROUP_IDX			1

#define GRPDEF_COMP_DESC_TYPE 		0xFF


/*
 * EXTDEF Record Macros
 */
#define EXTDEF_NOTYPEDEF 			0

#define MAX_NUMOF_EXTDEFS			0xFFF

/*
 * FIXUPP Record Macros
 */
#define FIXUPP_LOCAT_LOB(off) 	((unsigned char)(0xd4 | (unsigned char)		\
									((((unsigned long)(off)) >> 8) & 3)))
#define FIXUPP_LOCAT_HOB(off) 	((unsigned char)((off) & 0xFF))
#define FIXUPP_LOCAL_FIXDAT 	0x50
#define FIXUPP_EXTERNAL_FIXDAT 	0x56


/*
 * MODEND Record Macros
 */
#define MODEND_MODTYPE 			0


/*
 * Following two macros intialize beginning and end of an object record.
 */
#define OMF86_REC_ENTRY(rectype) { 											\
		recptr = &omf86_record[0];											\
		*recptr++ = rectype;												\
		reclenaddr  = (unsigned short *) recptr;							\
		recptr += sizeof(short);											\
		}

#define OMF86_REC_EXIT { 													\
		*reclenaddr  = (unsigned short) 									\
			((((unsigned long)recptr)-((unsigned long)reclenaddr)) - 1);	\
if (*reclenaddr > MAX_OMF86_REC_SIZE) 	\
	printf("\n   Record Overflow: size=%x",*reclenaddr); 	\
		chksum = 0;															\
		for (p=&omf86_record[0]; p < recptr; p++)							\
			chksum += *p;													\
		*recptr++ = (unsigned char) (256 - chksum);							\
		fwrite((void *)&omf86_record[0], (long)(*reclenaddr+3), 1, fd);		\
		}



coff_create_omf86_file(fname)
    char *fname;
{
    register unsigned char *recptr;
    register unsigned char chksum;
    register unsigned char *p;
    register unsigned char ch;
    unsigned char *namelenaddr;
    unsigned short *reclenaddr;
    FILE *fd;
    long  i, j;

#ifdef RESOLVEDEBUG
    resolve_debug_results();
#endif

#ifdef PCKGCOFF_DEBUG
    unsigned long rawdata_sz, reltbl_sz, symtbl_sz, strtbl_sz;

    rawdata_sz = (unsigned long) (rawdata_addr - rawdata_start_addr);
    reltbl_sz = (unsigned long) (reloc_entry - reloc_first_entry);
    symtbl_sz = (unsigned long) (symtbl_entry - symtbl_first_entry);
    strtbl_sz = (unsigned long) (strtbl_addr - strtbl_start_addr);
    printf("\nC_C_C_F: rawdata_sz=%x  reltbl_sz=%x  symtbl_sz=%x  strtbl_sz=%x",
	   rawdata_sz, reltbl_sz, symtbl_sz, strtbl_sz);
    printf("\nC_C_C_F: hashtbl_request=%d  hashtbl_lookup=%d ",
	   hashtbl_request, hashtbl_lookup);
#endif

/*
 * fprintf(stderr,"\nCREATE_OMF86_FILE:");      fflush(stderr);
 */
    /*
     * Long Word Boundary
     */
    while (((long) rawdata_addr & 0x03) != 0)
	coff_insert_char_rawdata(0);

    /*
     * Open the file for write access
     */
    if ((fd = fopen(fname, "wb")) == NULL) {
	fprintf(stderr,
		"\nWarning: Unable to open file %s for write access.",
		fname);
	fflush(stderr);
	free((char *) storage_addr);	/* Free the malloced area */
	storage_addr = (char *) -1;
	return (-1);
    }

    /*
     * Create THEADR Translator Header Record
     */
/*
 * fprintf(stderr,"\n   THEADR Record ");       fflush(stderr);
 */
    {
	AUXENT *auxs;

	OMF86_REC_ENTRY(RT_THEADR);
	namelenaddr = recptr++;
	auxs = (AUXENT *) (symtbl_first_entry + 1);
	p = (unsigned char *) (auxs->x_file.x_fname);
	for (i = 0; (ch = *p) && (i < FILNMLEN); i++, p++)
	    *recptr++ = ch;
	*namelenaddr = (unsigned char) i;
	OMF86_REC_EXIT;
    }

    /*
     * Create COMENT Comment Record
     */
/*
 * fprintf(stderr,"\n   COMENT Record ");       fflush(stderr);
 */
    {
	OMF86_REC_ENTRY(RT_COMENT);
	*recptr++ = COMENT_ATTRIB;
	*recptr++ = COMENT_CLASS;
	for (p = (unsigned char *) COMENT_STRING; ch = *p; p++)
	    *recptr++ = ch;
	OMF86_REC_EXIT;
    }

    /*
     * Create LNAMES List of Names Record
     */
/*
 * fprintf(stderr,"\n   LNAMES Record ");       fflush(stderr);
 */
    {
	OMF86_REC_ENTRY(RT_LNAMES);
	for (j = 0; j < NUMOF_LNAMES; j++) {
	    namelenaddr = recptr++;
	    for (i = 0, p = (unsigned char *) LNAMES_NAMES[j]; ch = *p; i++, p++)
		*recptr++ = ch;
	    *namelenaddr = (unsigned char) i;
	}
	OMF86_REC_EXIT;
    }

    /*
     * Create SEGDEF Segment Definition Records
     */
/*
 * fprintf(stderr,"\n   SEGDEF Records ");      fflush(stderr);
 */
    {
	for (j = 0; j < NUMOF_SEGS; j++) {
	    OMF86_REC_ENTRY(RT_SEGDEF);
	    *recptr++ = SEGDEF_ATTRIB;
	    if ((j + 1) != SEGDEF_TEXTSEG_IDX)
		*(long *) recptr = 0;
	    else
		*(long *) recptr = (long) (rawdata_addr - rawdata_start_addr);
	    recptr += sizeof (long);
	    *recptr++ = SEGDEF_SEG_IDX[j];
	    *recptr++ = SEGDEF_CLS_IDX[j];
	    *recptr++ = SEGDEF_OVL_IDX[j];
	    OMF86_REC_EXIT;
	}
    }

    /*
     * Create GRPDEF Group Definition Record
     */
/*
 * fprintf(stderr,"\n   GRPDEF Record ");       fflush(stderr);
 */
    {
	OMF86_REC_ENTRY(RT_GRPDEF);
	*recptr++ = LNAMES_GRP_DGROUP_IDX;
	*recptr++ = GRPDEF_COMP_DESC_TYPE;
	*recptr++ = SEGDEF_CONSTSEG_IDX;
	*recptr++ = GRPDEF_COMP_DESC_TYPE;
	*recptr++ = SEGDEF_BSSSEG_IDX;
	*recptr++ = GRPDEF_COMP_DESC_TYPE;
	*recptr++ = SEGDEF_DATASEG_IDX;
	OMF86_REC_EXIT;
    }

    /*
     * Create PUBDEF Public Names Definition Records
     * and EXTDEF External Names Definition Records
     *
     *      Steps:
     *              1.      Traverse the symbol table to create PUBDEF records for
     *                      public symbols. At the same time, collect external symbols.
     *              2.      Create EXTDEF records.
     */
    {
	SYMENT *sym, *first_sym;
	SYMENT **extsym;
	SYMENT **e;
	long  extsym_idx;

	/* Use the hash table area to store external symbol addresses */
	extsym = (SYMENT **) hashtbl_start_addr;
	extsym_idx = 0;

	/* First regular symbol in the symbol table (first 8 entries  belong
	 * to special symbols
	 */
	first_sym = symtbl_first_entry + 8;

/*
 * fprintf(stderr,"\n   PUBDEF Records ");      fflush(stderr);
 */
	for (sym = first_sym; sym < symtbl_entry; sym++) {
	    if (!(sym->n_scnum)) {	/* External Symbol */
		*extsym++ = sym;
		sym->n_value = ++extsym_idx;
	    }
	    else if (sym->n_sclass == C_EXT) {	/* Public Symbol */
		OMF86_REC_ENTRY(RT_PUBDEF);
		*recptr++ = GRPDEF_NOGROUP_IDX;
		*recptr++ = SEGDEF_TEXTSEG_IDX;
		namelenaddr = recptr++;
		if (sym->n_zeroes == 0) {
		    p = (unsigned char *) (strtbl_start_addr + sym->n_offset);
		    for (i = 0; ch = *p; i++, p++)
			*recptr++ = ch;
		}
		else {
		    p = (unsigned char *) (sym->n_name);
		    for (i = 0; (ch = *p) && (i < SYMNMLEN); i++, p++)
			*recptr++ = ch;
		}
		*namelenaddr = (unsigned char) i;
		*(long *) recptr = sym->n_value;
		recptr += sizeof (long);
		*recptr++ = TYPDEF_NOTYPE_IDX;
		OMF86_REC_EXIT;
	    }
	}

	/*
	 * Create EXTDEF Records
	 */
	if (extsym_idx > MAX_NUMOF_EXTDEFS) {
	    fprintf(stderr, "Error: Too many external symbols.");
	    fflush(stderr);
	    free((char *) storage_addr);	/* Free the malloced area */
	    storage_addr = (char *) -1;
	    return (-1);
	}

/*
 * fprintf(stderr,"\n   EXTDEF Records ");      fflush(stderr);
 */
	for (e = (SYMENT **) hashtbl_start_addr; e < extsym; e++) {
	    OMF86_REC_ENTRY(RT_EXTDEF);
	    sym = *e;
	    namelenaddr = recptr++;
	    if (sym->n_zeroes == 0) {
		p = (unsigned char *) (strtbl_start_addr + sym->n_offset);
		for (i = 0; ch = *p; i++, p++)
		    *recptr++ = ch;
	    }
	    else {
		p = (unsigned char *) sym->n_name;
		for (i = 0; (ch = *p) && (i < SYMNMLEN); i++, p++)
		    *recptr++ = ch;
	    }
	    *namelenaddr = (unsigned char) i;
	    *recptr++ = EXTDEF_NOTYPEDEF;
	    OMF86_REC_EXIT;
	}
    }

    /*
     * Create LEDATA Logical Enumerated Data Records and
     * FIXUPP Fixup Records
     */
    {
	char *rwptr;
	RELOC *rs;
	RELOC *re;
	SYMENT *sym;
	long  blkstart;
	long  blkend;
	long  ledatasz;
	long  off;

/*
 * fprintf(stderr,"\n   LEDATA & FIXUPP Records  ");    fflush(stderr);
 */
	rs = reloc_first_entry;
	rwptr = rawdata_start_addr;
	for (; rwptr < rawdata_addr;) {
	    /*
	     * Create LEDATA Record
	     */
/*
 * fprintf(stderr,"+");         fflush(stderr);
 */
	    OMF86_REC_ENTRY(RT_LEDATA);
	    *recptr++ = SEGDEF_TEXTSEG_IDX;

	    /* Data Offset of Block */
	    blkstart = (long) (rwptr - rawdata_start_addr);
	    *(long *) recptr = (long) blkstart;
	    recptr += sizeof (long);

	    /* Find size of data block and last relocation entry belongs to
	     * that data block
	     */
	    if (((long) (rawdata_addr - rwptr)) <= 0x0400) {
		ledatasz = (long) (rawdata_addr - rwptr);
		re = reloc_entry - 1;
	    }
	    else {
		ledatasz = 0x0400;
		blkend = (long) (blkstart + ledatasz);
		for (re = rs; (re < reloc_entry) && (re->r_vaddr < blkend); re++) ;
		re--;
		if ((rs <= re) && ((re->r_vaddr + sizeof (long)) > blkend)) {
		    ledatasz = (((long) re->r_vaddr) - ((long) blkstart));
		    re--;
		}
	    }

	    /* Copy data block into LEDATA record */
	    for (; ledatasz > 0;) {
		*(long *) recptr = *(long *) rwptr;
		recptr += sizeof (long);
		rwptr += sizeof (long);
		ledatasz -= sizeof (long);
	    }
	    recptr += ledatasz;	/* ledatasz is between 0 and -3 */
	    rwptr += ledatasz;
	    OMF86_REC_EXIT;

	    /*
	     * Create FIXUPP Record
	     */
	    if (rs <= re) {
/*
 * fprintf(stderr,"-");         fflush(stderr);
 */
		OMF86_REC_ENTRY(RT_FIXUPP);

		/* Put Fixup information into FIXUPP record for each
		 * relocation entry belongs to previous LEADATA record
		 */
		for (; rs <= re; rs++) {
		    sym = (symtbl_first_entry + rs->r_symndx);
		    off = (long) (((long) rs->r_vaddr) - ((long) blkstart));
		    *recptr++ = FIXUPP_LOCAT_LOB(off);
		    *recptr++ = FIXUPP_LOCAT_HOB(off);
		    if (sym->n_scnum) {		/* Local Symbol */
			*recptr++ = FIXUPP_LOCAL_FIXDAT;
			*recptr++ = SEGDEF_TEXTSEG_IDX;
			*(long *) recptr = sym->n_value;
			recptr += sizeof (long);
		    }
		    else {	/* External Symbol */
			*recptr++ = FIXUPP_EXTERNAL_FIXDAT;
			if (sym->n_value >= 0x80) {
			    *recptr++ = (unsigned char)
				(0x80 | (sym->n_value / 256));
			    *recptr++ = (unsigned char) (sym->n_value % 256);
			}
			else {
			    *recptr++ = (unsigned char) sym->n_value;
			}
		    }
		}
		OMF86_REC_EXIT;
	    }
	}			/* end of for loop */
    }

    /*
     * Create MODEND Module End Record
     */
/*
 * fprintf(stderr,"\n   MODEND Record ");       fflush(stderr);
 */
    {
	OMF86_REC_ENTRY(RT_MODEND);
	*recptr++ = MODEND_MODTYPE;
	OMF86_REC_EXIT;
    }

    /*
     * Close the file and free the malloced area
     */
    fclose(fd);
    free((char *) storage_addr);
    storage_addr = (char *) -1;
    return (1);
}

#endif /* PCKG_OBJFORMAT_OMF86 */



#ifdef PCKG_OBJFORMAT_AOUT

/****************************************************************************
 *                                                      Create A.OUT File                                                               *
 ****************************************************************************/

copy_name_to_strtbl(name)
    register char *name;
{
    register char *s;
    register int sz;
    int   nidx;

    if (strtbl_addr + SYMNMLEN + 1 > strtbl_end_addr) {
	fprintf(stderr,
		"\nWarning: String Table of COFF file is full.");
	fflush(stderr);
	return (-1);
    }

    for (s = strtbl_addr, sz = 0; (*name != 0 && sz <= SYMNMLEN); sz++)
	*s++ = *name++;
    *s++ = 0;

    nidx = strtbl_addr - strtbl_start_addr;
    strtbl_addr = s;
    return (nidx);
}


coff_create_aout_file(fname)
    char *fname;
{
    FILE *aout_fd;
    struct exec aout_filehdr;
    struct nlist aout_syment;
    struct reloc_info_68k aout_reloc;

    register RELOC *r;
    register SYMENT *s;

    /*
     * Long Word Boundary
     */
    while (((long) rawdata_addr & 0x03) != 0)
	coff_insert_char_rawdata(0);

    /*
     * Update relocation entries for local variables
     */
    for (r = reloc_first_entry; r < reloc_entry; r++) {
	s = (SYMENT *) (symtbl_first_entry + r->r_symndx);
	if (s->n_scnum) {
	    *(long *) (rawdata_start_addr + r->r_vaddr) = s->n_value;
	    r->r_symndx = SEC_DATA_SYM_IDX;
	}
    }

    /*
     * Update Symbol Table for External Symbols
     */
    for (s = symtbl_first_entry; s < symtbl_entry; s++) {
	if (!(s->n_scnum))
	    s->n_sclass = (char) C_EXT;
	s += s->n_numaux;
    }

    /*
     * Update String Table Size
     */
    *LongPtr(strtbl_start_addr) = (long) (strtbl_addr - strtbl_start_addr);

    /*
     * Create the real COFF file
     */
    {
	unsigned long rawdata_sz, reltbl_sz, symtbl_sz, strtbl_sz;

	rawdata_sz = (unsigned long) (rawdata_addr - rawdata_start_addr);
	reltbl_sz = (unsigned long) (reloc_entry - reloc_first_entry);
	symtbl_sz = (unsigned long) (symtbl_entry - symtbl_first_entry) -
	    (long) FIRST_REGULAR_SYMIDX;

	if ((aout_fd = fopen(fname, "w")) == NULL) {
	    fprintf(stderr,
		    "\nWarning: Unable to open file '%s' for write access.",
		    fname);
	    fflush(stderr);
	    free((char *) storage_addr);	/* Free the malloced area */
	    storage_addr = (char *) -1;
	    return (-1);
	}

	/*
	 * Write File Header
	 */
	aout_filehdr.a_dynamic = 0;
	aout_filehdr.a_toolversion = 0;
	aout_filehdr.a_machtype = M_68020;
	aout_filehdr.a_magic = OMAGIC;
	aout_filehdr.a_text = rawdata_sz;
	aout_filehdr.a_data = 0;
	aout_filehdr.a_bss = 0;
	aout_filehdr.a_syms = symtbl_sz * sizeof (aout_syment);
	aout_filehdr.a_entry = 0;
	aout_filehdr.a_trsize = reltbl_sz * sizeof (aout_reloc);
	aout_filehdr.a_drsize = 0;

	fwrite((void *) &aout_filehdr, sizeof (aout_filehdr), 1, aout_fd);

	/*
	 * Write Text Section
	 */
	fwrite((void *) rawdata_start_addr, rawdata_sz, 1, aout_fd);

	/*
	 * Write Relocation Entries
	 */
	/* Fix values for all relocation entries */
	aout_reloc.r_pcrel = 0;
	aout_reloc.r_length = 2;
	aout_reloc.r_baserel = 0;
	aout_reloc.r_jmptable = 0;
	aout_reloc.r_relative = 0;

	for (r = reloc_first_entry; r < reloc_entry; r++) {
	    aout_reloc.r_address = r->r_vaddr;
	    if (r->r_symndx == SEC_DATA_SYM_IDX) {
		aout_reloc.r_symbolnum = N_TEXT;
		aout_reloc.r_extern = 0;
	    }
	    else {
		aout_reloc.r_symbolnum = r->r_symndx - FIRST_REGULAR_SYMIDX;
		aout_reloc.r_extern = 1;
	    }
	    fwrite((void *) &aout_reloc, sizeof (aout_reloc), 1, aout_fd);
	}

	/*
	 * Write Symbol Table Entries
	 */
	/* Fix values for symbol table entries */
	aout_syment.n_other = 0;
	aout_syment.n_desc = 0;

	for (s = symtbl_first_entry + FIRST_REGULAR_SYMIDX; s < symtbl_entry; s++) {
	    if (s->n_zeroes == 0)
		aout_syment.n_un.n_strx = s->n_offset;
	    else
		aout_syment.n_un.n_strx = copy_name_to_strtbl(s->n_name);
	    if (s->n_scnum)
		aout_syment.n_type = N_TEXT;
	    else
		aout_syment.n_type = 0;
	    if (s->n_sclass == C_EXT)
		aout_syment.n_type |= N_EXT;
	    aout_syment.n_value = s->n_value;

	    fwrite((void *) &aout_syment, sizeof (aout_syment), 1, aout_fd);
	}

	/*
	 * Write String Table
	 */
	strtbl_sz = (unsigned long) (strtbl_addr - strtbl_start_addr);
	*LongPtr(strtbl_start_addr) = (long) strtbl_sz;

	fwrite((void *) strtbl_start_addr, strtbl_sz, 1, aout_fd);
    }

    /*
     * Close the file and free the malloced area
     */
    fclose(aout_fd);
    free((char *) storage_addr);
    storage_addr = (char *) -1;

    return (1);
}

#endif /* PCKG_OBJFORMAT_AOUT */


#endif /* PACKAGE  */
