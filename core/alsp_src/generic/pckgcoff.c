/*
 * pckgcoff.c   --      Creating coff files for application packaging
 *
 * Copyright (c) 1990-1993, Applied Logic Systems, Inc.
 *
 * Author: Ilyas Cicekli
 * Date  : 6/4/1990
 */

#include "defs.h"

#ifdef PACKAGE

/*
 * #define PCKGCOFF_DEBUG  1
 */

#include <stdio.h>

#include "coerce.h"
#include "pckgcoff.h"
#include "rinfo.h"
#include "pckgmake.h"


/*
 * Symbol indexes for global variables which can appear
 * in Prolog code.
 */
long  coff_symidx_array[NUMOF_COFF_SYMIDX];


#ifdef arch_m88k

long  ovtab_symidx[] =
{
    symidx_wm_overflow0,
    symidx_wm_overflow1,
    symidx_wm_overflow2,
    symidx_wm_overflow3
};

long  ebtab_symidx[] =
{
    symidx_wm_exec_builtin0,
    symidx_wm_exec_builtin1,
    symidx_wm_exec_builtin2,
    symidx_wm_exec_builtin3
};

long  rrtab_symidx[] =
{
    symidx_wm_resolve_ref0,
    symidx_wm_resolve_ref1,
    symidx_wm_resolve_ref2,
    symidx_wm_resolve_ref3
};

long  trytab_symidx[] =
{
    symidx_wm_try0,
    symidx_wm_try1,
    symidx_wm_try2,
    symidx_wm_try3
};

long  retry_tab_symidx[] =
{
    symidx_wm_retry0,
    symidx_wm_retry1,
    symidx_wm_retry2,
    symidx_wm_retry3
};

long  retry_u_tab_symidx[] =
{
    symidx_wm_retry_u0,
    symidx_wm_retry_u1,
    symidx_wm_retry_u2,
    symidx_wm_retry_u3
};

long  trust_tab_symidx[] =
{
    symidx_wm_trust0,
    symidx_wm_trust1,
    symidx_wm_trust2,
    symidx_wm_trust3
};

long  trust_u_tab_symidx[] =
{
    symidx_wm_trust_u0,
    symidx_wm_trust_u1,
    symidx_wm_trust_u2,
    symidx_wm_trust_u3
};

#endif /* arch_m88k */



/********************************************************************
 *                     Package Areas                                *
 *******************************************************************/

static char rawdata_fname[1024];
static FILE *rawdata_fp;
static char *rawdata_start_addr;
static char *rawdata_addr;

/*
 * Relocation Entries
 */

static long numof_reloc_entries;
static PRELOC *reloc_first_entry;
static PRELOC *reloc_last_entry;
static PRELOC *reloc_entry;


/*
 * Symbol Table
 */

static long numof_symtbl_entries;
static PSYMENT *symtbl_first_entry;
static PSYMENT *symtbl_last_entry;
static PSYMENT *symtbl_entry;


/*
 * String Table
 */

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

static long hashtbl_sizes[64] =
{
    4603,			/* Symbol Table Size <=   4 Kbytes */
    9209,			/* Symbol Table Size <=   8 Kbytes */
    13807,			/* Symbol Table Size <=  12 Kbytes */
    18427,			/* Symbol Table Size <=  16 Kbytes */
    23039,			/* Symbol Table Size <=  20 Kbytes */
    27647,			/* Symbol Table Size <=  24 Kbytes */
    32251,			/* Symbol Table Size <=  28 Kbytes */
    36857,			/* Symbol Table Size <=  32 Kbytes */
    41467,			/* Symbol Table Size <=  36 Kbytes */
    46073,			/* Symbol Table Size <=  40 Kbytes */
    50683,			/* Symbol Table Size <=  44 Kbytes */
    55291,			/* Symbol Table Size <=  48 Kbytes */
    59887,			/* Symbol Table Size <=  52 Kbytes */
    64499,			/* Symbol Table Size <=  56 Kbytes */
    69119,			/* Symbol Table Size <=  60 Kbytes */
    73727,			/* Symbol Table Size <=  64 Kbytes */
    78317,			/* Symbol Table Size <=  68 Kbytes */
    82939,			/* Symbol Table Size <=  72 Kbytes */
    87547,			/* Symbol Table Size <=  76 Kbytes */
    92153,			/* Symbol Table Size <=  80 Kbytes */
    96763,			/* Symbol Table Size <=  84 Kbytes */
    101363,			/* Symbol Table Size <=  88 Kbytes */
    105983,			/* Symbol Table Size <=  92 Kbytes */
    110587,			/* Symbol Table Size <=  96 Kbytes */
    115183,			/* Symbol Table Size <= 100 Kbytes */
    119797,			/* Symbol Table Size <= 104 Kbytes */
    124367,			/* Symbol Table Size <= 108 Kbytes */
    129023,			/* Symbol Table Size <= 112 Kbytes */
    133631,			/* Symbol Table Size <= 116 Kbytes */
    138239,			/* Symbol Table Size <= 120 Kbytes */
    142841,			/* Symbol Table Size <= 124 Kbytes */
    147451,			/* Symbol Table Size <= 128 Kbytes */
    152063,			/* Symbol Table Size <= 132 Kbytes */
    156671,			/* Symbol Table Size <= 136 Kbytes */
    161267,			/* Symbol Table Size <= 140 Kbytes */
    165887,			/* Symbol Table Size <= 144 Kbytes */
    170483,			/* Symbol Table Size <= 148 Kbytes */
    175103,			/* Symbol Table Size <= 152 Kbytes */
    179693,			/* Symbol Table Size <= 156 Kbytes */
    184309,			/* Symbol Table Size <= 160 Kbytes */
    188927,			/* Symbol Table Size <= 164 Kbytes */
    193513,			/* Symbol Table Size <= 168 Kbytes */
    198139,			/* Symbol Table Size <= 172 Kbytes */
    202751,			/* Symbol Table Size <= 176 Kbytes */
    207343,			/* Symbol Table Size <= 180 Kbytes */
    211949,			/* Symbol Table Size <= 184 Kbytes */
    216571,			/* Symbol Table Size <= 188 Kbytes */
    221173,			/* Symbol Table Size <= 192 Kbytes */
    225781,			/* Symbol Table Size <= 196 Kbytes */
    230393,			/* Symbol Table Size <= 200 Kbytes */
    235007,			/* Symbol Table Size <= 204 Kbytes */
    239611,			/* Symbol Table Size <= 208 Kbytes */
    244219,			/* Symbol Table Size <= 212 Kbytes */
    248827,			/* Symbol Table Size <= 216 Kbytes */
    253439,			/* Symbol Table Size <= 220 Kbytes */
    258031,			/* Symbol Table Size <= 224 Kbytes */
    262651,			/* Symbol Table Size <= 228 Kbytes */
    267259,			/* Symbol Table Size <= 232 Kbytes */
    271867,			/* Symbol Table Size <= 236 Kbytes */
    276467,			/* Symbol Table Size <= 240 Kbytes */
    281081,			/* Symbol Table Size <= 244 Kbytes */
    285673,			/* Symbol Table Size <= 248 Kbytes */
    290249,			/* Symbol Table Size <= 252 Kbytes */
    294911			/* Symbol Table Size <= 256 Kbytes */
};

#ifdef PCKGCOFF_DEBUG
static long hashtbl_request;
static long hashtbl_lookup;

#endif


/*
 * The procedure "coff_init_package" allocates a storage area to store
 * raw data area, relocation table, symbol table and string table.
 * This procedure also initalizes these areas.
 */
int
coff_init_package(reloctblsize, symtblsize, strtblsize)
    long  reloctblsize;		/* Number of relocation entries */
    long  symtblsize;		/* Number of symbols */
    long  strtblsize;		/* Size of string table (in Kbytes) */
{
    extern int pckg_error;

    pckg_error = 0;

#ifdef PCKGCOFF_DEBUG
    printf("\nInitializing COFF File areas\n");
#endif

    /*
     * Process arguments
     */

    numof_reloc_entries = reloctblsize * 1024;
    numof_symtbl_entries = symtblsize * 1024;
    strtbl_size = strtblsize * 1024;

    /*
     * Allocate storage area to store raw data area, relocation entries,
     * symbol table, string table and hash table for symbol table.
     */
    hashtbl_size = hashtbl_sizes[((numof_symtbl_entries - 1) >> 12)];

    storage_size =
	numof_reloc_entries * sizeof (PRELOC) +
	numof_symtbl_entries * sizeof (PSYMENT) +
	strtbl_size + hashtbl_size * sizeof (long);


#ifdef PCKGCOFF_DEBUG
    fprintf(stderr, "\nnumof_symtbl_entries=%d, hashtbl_size=%d\n",
	    numof_symtbl_entries, hashtbl_size);
    fprintf(stderr, "numof_reloc_entries=%d, sizeof(PRELOC)=%d\n",
	    numof_reloc_entries, sizeof (PRELOC));
    fprintf(stderr, "sizeof(PSYMENT)=%d, strtbl_size=%d\n",
	    sizeof (PSYMENT), strtbl_size);
    fprintf(stderr, "Total =%d bytes\n", storage_size);
#endif

    /*
     * Deallocate the previous area
     */
    if (storage_addr != (char *) -1) {
	free((char *) storage_addr);
	storage_addr = (char *) -1;
	/* reset raw data file */
	fseek(rawdata_fp, (long) rawdata_start_addr, 0);
	rawdata_addr = rawdata_start_addr;
    }

    if ((storage_addr = (char *) malloc(storage_size)) != (char *) NULL) {

	/*
	 * Relocation Table Pointers
	 */
	reloc_first_entry = (PRELOC *) storage_addr;
	reloc_last_entry = reloc_first_entry + (numof_reloc_entries - 1);
	reloc_entry = reloc_first_entry;
	/*
	 * Symbol Table Pointers
	 */
	symtbl_first_entry = (PSYMENT *) (reloc_last_entry + 1);
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
	hashtbl_end_addr = (long *) hashtbl_start_addr + (hashtbl_size - 1);
    }
    else {
	storage_addr = (char *) -1;
	return (PCKG_MALLOC_FAIL);
    }

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
     * Initialize Symbol Table
     */
    coff_init_symtbl();

#ifdef PCKGCOFF_DEBUG
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
 * Initialize the symbol table by putting initial global variables
 */

coff_init_symtbl()
{
    coff_init_icode1();
    coff_init_icode2();
    coff_init_math();

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
	    if (COFF_SYMIDX(i) != i)
		fprintf(stderr, "Error: Stupid MetaWare HighC Compiler Error");
	}
    }
#endif /* DOS */

    return (1);
}


/* *INDENT-OFF* */

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

#ifdef arch_m88k
INSERT_SYM(symidx_wm_unify, "_wm_unify")
INSERT_SYM(symidx_wm_docut, "_wm_docut")
INSERT_SYM(symidx_wm_g_sym, "_wm_g_sym")
INSERT_SYM(symidx_wm_g_uia, "_wm_g_uia")
INSERT_SYM(symidx_wm_g_dbl, "_wm_g_dbl")
INSERT_SYM(symidx_wm_p_uia, "_wm_p_uia")
INSERT_SYM(symidx_wm_p_unsafe, "_wm_p_unsafe")
INSERT_SYM(symidx_wm_u_sym, "_wm_u_sym")
INSERT_SYM(symidx_wm_u_int, "_wm_u_int")
INSERT_SYM(symidx_wm_u_lval, "_wm_u_lval")
INSERT_SYM(symidx_mth_base, "_mth_base")
INSERT_SYM(symidx_mth_stk, "_mth_stk")
INSERT_SYM(symidx_mth_bot, "_mth_bot")
INSERT_SYM(symidx_mth_aftercall, "_mth_aftercall")
INSERT_SYM(symidx_mth_pushdbl0, "_mth_pushdbl0")
INSERT_SYM(symidx_wm_overflow0, "_wm_overflow0")
INSERT_SYM(symidx_wm_overflow1, "_wm_overflow1")
INSERT_SYM(symidx_wm_overflow2, "_wm_overflow2")
INSERT_SYM(symidx_wm_overflow3, "_wm_overflow3")
INSERT_SYM(symidx_wm_exec_builtin0, "_wm_exec_builtin0")
INSERT_SYM(symidx_wm_exec_builtin1, "_wm_exec_builtin1")
INSERT_SYM(symidx_wm_exec_builtin2, "_wm_exec_builtin2")
INSERT_SYM(symidx_wm_exec_builtin3, "_wm_exec_builtin3")
INSERT_SYM(symidx_wm_resolve_ref0, "_wm_resolve_ref0")
INSERT_SYM(symidx_wm_resolve_ref1, "_wm_resolve_ref1")
INSERT_SYM(symidx_wm_resolve_ref2, "_wm_resolve_ref2")
INSERT_SYM(symidx_wm_resolve_ref3, "_wm_resolve_ref3")
INSERT_SYM(symidx_wm_try0, "_wm_try0")
INSERT_SYM(symidx_wm_try1, "_wm_try1")
INSERT_SYM(symidx_wm_try2, "_wm_try2")
INSERT_SYM(symidx_wm_try3, "_wm_try3")
INSERT_SYM(symidx_wm_retry0, "_wm_retry0")
INSERT_SYM(symidx_wm_retry1, "_wm_retry1")
INSERT_SYM(symidx_wm_retry2, "_wm_retry2")
INSERT_SYM(symidx_wm_retry3, "_wm_retry3")
INSERT_SYM(symidx_wm_retry_u0, "_wm_retry_u0")
INSERT_SYM(symidx_wm_retry_u1, "_wm_retry_u1")
INSERT_SYM(symidx_wm_retry_u2, "_wm_retry_u2")
INSERT_SYM(symidx_wm_retry_u3, "_wm_retry_u3")
INSERT_SYM(symidx_wm_trust0, "_wm_trust0")
INSERT_SYM(symidx_wm_trust1, "_wm_trust1")
INSERT_SYM(symidx_wm_trust2, "_wm_trust2")
INSERT_SYM(symidx_wm_trust3, "_wm_trust3")
INSERT_SYM(symidx_wm_trust_u0, "_wm_trust_u0")
INSERT_SYM(symidx_wm_trust_u1, "_wm_trust_u1")
INSERT_SYM(symidx_wm_trust_u2, "_wm_trust_u2")
INSERT_SYM(symidx_wm_trust_u3, "_wm_trust_u3")
#endif /* arch_m88k */

/* *INDENT-ON* */




/*
 * The procedure "coff_find_symbol" tries to find the given symbol
 * in the symbol table. If the symbol is in the symbol table,
 * the symbol table entry of that symbol is returned. Otherwise,
 * this procedure returns 0.
 */
static PSYMENT *
coff_find_symbol(sym_name, sym_len)
    unsigned char *sym_name;
    long  sym_len;
{
    register unsigned char *s = sym_name;
    register unsigned long idx;
    register unsigned long shift;
    register unsigned int n;
    register PSYMENT *sym;	/* a symbol table entry pointer */
    unsigned long start;
    char *strptr;

#ifdef PCKGCOFF_DEBUG
    hashtbl_request++;
    hashtbl_lookup++;
    if ((hashtbl_request & (long) 0x03FF) == 0) {
	printf("\nFIND_SYM:   numof_syms: %d   request=%d   lookup=%d ",
	       (unsigned long) (symtbl_entry - symtbl_first_entry),
	       hashtbl_request, hashtbl_lookup);
    }
#endif

    shift = (*s & 0x0f);
    idx = *s;
    if (idx) {
	idx ^= (*(s + 1));
    }
    for (n = 0; *s; n++, shift += 4) {
	idx += ((*s++) << shift);
	if (shift >= 17)
	    shift -= 17;
    }

    shift = ((idx >> 1) % (hashtbl_size - 1)) + 1;
    /* Range will be 1..ts_prime-1 */
    idx = (idx + (idx >> 9) + n) % hashtbl_size;


    start = idx;

    while ((sym = (PSYMENT *) * (hashtbl_start_addr + idx)) != (PSYMENT *) 0) {

	strptr = (char *) strtbl_start_addr + sym->pn_offset;
	if (strcmp(sym_name, strptr) == 0)
	    return (sym);

	idx = (idx + shift) % hashtbl_size;
	if (idx == start)
	    fatal_error(FE_PCKG_HTBL, 0);

#ifdef PCKGCOFF_DEBUG
	hashtbl_lookup++;
#endif

    }

    /* symbol Not Found */

    *(hashtbl_start_addr + idx) = (long) symtbl_entry;
    return ((PSYMENT *) 0);

}



/*
 * The procedure "coff_insert_symbol" inserts the given symbol into
 * the symbol table. If it is successful, it returns symbol index.
 * Otherwise, it gives a warning message and returns -1.
 */
coff_insert_symbol(sym_name, sym_def_flag)
    char *sym_name;		/* symbol name  */
    long  sym_def_flag;		/* type of symbol definition */
{
    long  sym_len;		/* length of symbol (not including null
				 * character)
				 */
    PSYMENT *sym;		/* a symbol table entry pointer */
    char  savech;		/* save character       */

    sym_len = strlen(sym_name);
    if (sym_len > P_S_MAXSYMLEN) {
	savech = *(sym_name + P_S_MAXSYMLEN);
	*(sym_name + P_S_MAXSYMLEN) = 0;
	sym_len = P_S_MAXSYMLEN;
    }
    else
	savech = 0;

    /*
     * Allocate symbol table entry (if it is required)
     */
    sym = (PSYMENT *) coff_find_symbol(sym_name, sym_len);
    if (sym == (PSYMENT *) 0) {
	if (symtbl_entry > symtbl_last_entry) {
	    return (PCKG_SYMTBL_FULL);
	}
	sym = symtbl_entry++;
	/*
	 * Copy the symbol name into the string table and put the offset into
	 * its location in the symbol table entry.
	 */
	if (strtbl_addr + sym_len + 1 > strtbl_end_addr) {
	    return (PCKG_STRTBL_FULL);
	}
	sym->pn_offset = (unsigned long) (strtbl_addr - strtbl_start_addr);
	strcpy(strtbl_addr, sym_name);
	strtbl_addr += (sym_len + 1);
	/*
	 * Initialize the symbol
	 */
	sym->pn_value = (unsigned long) 0;
	sym->pn_len = (unsigned char) sym_len;
	sym->pn_type = (unsigned char) P_S_UNDEFINED;
    }

    if (savech != 0)
	*(sym_name + P_S_MAXSYMLEN) = savech;

    /*
     * Check the symbol definition flag and
     * update the symbol table entry properly.
     */
    switch (sym_def_flag) {
	case SYMDEF_DECLARE:
	    sym->pn_value = (unsigned long) (rawdata_addr - rawdata_start_addr);
	    sym->pn_type = (unsigned char) P_S_LOCAL;
	    break;
	case SYMDEF_DECLARE_GLOBAL:
	    sym->pn_value = (unsigned long) (rawdata_addr - rawdata_start_addr);
	    sym->pn_type = (unsigned char) P_S_GLOBAL;
	    break;
	case SYMDEF_REFERENCE:
	    break;
    }

    /*
     * Return the symbol table index
     */
    return ((int) (sym - symtbl_first_entry));
}


/*
 * The procedure "coff_insert_reloc" inserts a relocation entry
 * into the relocation table of the COFF file. If it is unable
 * to insert an entry, it prints a warning message and returns -1.
 * Otherwise it inserts the entry and returns 1.
 */
coff_insert_reloc(sym_name, rtype, offset)
    char *sym_name;
    unsigned short rtype;
    int   offset;
{
    long  sym_idx;

    if (reloc_entry > reloc_last_entry) {
	return (PCKG_RELTBL_FULL);
    }

    sym_idx = coff_insert_symbol(sym_name, SYMDEF_REFERENCE);

    if (sym_idx < 0)
	return (sym_idx);

    reloc_entry->pr_vaddr = (unsigned long) ((rawdata_addr - rawdata_start_addr) + offset);
    reloc_entry->pr_symndx = (unsigned long) sym_idx;
    reloc_entry->pr_type = (unsigned short) rtype;
    reloc_entry++;

    return (1);
}


coff_insert_reloc_symidx(sym_idx, rtype, offset)
    long  sym_idx;
    unsigned short rtype;
    int   offset;
{
    if (reloc_entry > reloc_last_entry) {
	return (PCKG_RELTBL_FULL);
    }

    reloc_entry->pr_vaddr = (unsigned long) ((rawdata_addr - rawdata_start_addr) + offset);
    reloc_entry->pr_symndx = (unsigned long) sym_idx;
    reloc_entry->pr_type = (unsigned short) rtype;
    reloc_entry++;

    return (1);
}


/*
 * The procedure "coff_insert_rawdata" copies "size" bytes from
 * the data area whose address is in "from" into the COFF file.
 */

coff_insert_rawdata(from, size)
    char *from;
    int   size;
{
    rawdata_addr += size;
    fwrite(from, size, 1, rawdata_fp);
    return (1);
}


coff_insert_string_rawdata(val)
    char *val;
{
    long  n;

    n = strlen(val) + 1;

    rawdata_addr += n;
    fwrite(val, n, 1, rawdata_fp);
    return (1);
}


coff_insert_char_rawdata(val)
    char  val;
{
    rawdata_addr += sizeof (char);
    fputc(val, rawdata_fp);
    return (1);
}


coff_insert_short_rawdata(val)
    short val;
{
    rawdata_addr += sizeof (short);
    fwrite(&val, sizeof (short), 1, rawdata_fp);
    return (1);
}


coff_insert_long_rawdata(val)
    long  val;
{
    rawdata_addr += sizeof (long);
    fwrite(&val, sizeof (long), 1, rawdata_fp);
    return (1);
}

static long zeroes[2] =
{0, 0};

coff_align4_rawdata()
{
    int   i;

    if ((LongVal(rawdata_addr) & 0x03) != 0) {
	i = 4 - (LongVal(rawdata_addr) & 0x03);
	fwrite((void *) zeroes, i, 1, rawdata_fp);
	rawdata_addr += i;
    }
    return (1);
}

coff_align8_rawdata()
{
    int   i;

    if ((LongVal(rawdata_addr) & 0x07) != 0) {
	i = 8 - (LongVal(rawdata_addr) & 0x07);
	fwrite((void *) zeroes, i, 1, rawdata_fp);
	rawdata_addr += i;
    }
    return (1);
}


#ifdef PCKG_OBJFORMAT_COFF

#include <sys/types.h>
#include <filehdr.h>
#include <scnhdr.h>
#include <reloc.h>
#include <linenum.h>
#include <syms.h>

/*
 * Section Headers and Names
 */

#define NUMOF_SECTIONS 			3

#define SEC_TEXT 				0
#define SEC_DATA 				1
#define SEC_BSS 				2

#define SEC_TEXT_SYM_IDX 		2
#define SEC_DATA_SYM_IDX 		4
#define SEC_BSS_SYM_IDX 		6
#define FIRST_REGULAR_SYMIDX 	8

static char sec_names[NUMOF_SECTIONS][SYMNMLEN] =
{".text", ".data", ".bss"};
static short sec_types[NUMOF_SECTIONS] =
{STYP_TEXT, STYP_DATA, STYP_BSS};



/****************************************************************************
 *                      Create SysV COFF File                               *
 ****************************************************************************/

coff_open_rawdata_file(fname)
    char *fname;
{
    FILHDR coff_filehdr;
    SCNHDR coff_scnhdr[NUMOF_SECTIONS];

    if ((rawdata_fp = fopen(fname, "w+b")) == NULL) {
	return (PCKG_FOPEN_FAIL);
    }

    /* save the file name for later use */

    strcpy(rawdata_fname, fname);

    /* write dummy file header */

    fwrite((void *) &coff_filehdr, FILHSZ, 1, rawdata_fp);

    /* write dummy section headers */

    fwrite((void *) &coff_scnhdr[0], SCNHSZ, NUMOF_SECTIONS, rawdata_fp);

    rawdata_addr = rawdata_start_addr = (char *) ftell(rawdata_fp);
    return (1);
}


coff_create_obj_file(fname)
    char *fname;
{
    long  i;
    char *chptr;
    unsigned long rawdata_sz, reltbl_sz, symtbl_sz, strtbl_sz;
    register PRELOC *r;
    register PSYMENT *s;
    FILHDR coff_filehdr;
    SCNHDR coff_scnhdr[NUMOF_SECTIONS];
    RELOC coff_reloc;
    SYMENT coff_syment;
    AUXENT *auxs;
    FILE *fp;

    /*
     * Long Word Boundary
     */

    coff_align4_rawdata();

    /*
     * Sizes of package areas
     */
    rawdata_sz = (unsigned long) (rawdata_addr - rawdata_start_addr);
    reltbl_sz = (unsigned long) (reloc_entry - reloc_first_entry);
    symtbl_sz = (unsigned long) (symtbl_entry - symtbl_first_entry);
    strtbl_sz = (unsigned long) (strtbl_addr - strtbl_start_addr);

#ifdef PCKGCOFF_DEBUG
    printf("\nC_C_O_F: rawdata_sz=%x  reltbl_sz=%x  symtbl_sz=%x  strtbl_sz=%x",
	   rawdata_sz, reltbl_sz, symtbl_sz, strtbl_sz);
    printf("\nC_C_O_F: hashtbl_request=%d  hashtbl_lookup=%d ",
	   hashtbl_request, hashtbl_lookup);
#endif

    /*
     * Update rawdata area for local symbols
     */
    for (r = reloc_first_entry; r < reloc_entry; r++) {
	s = (PSYMENT *) (symtbl_first_entry + r->pr_symndx);
	if (s->pn_type != P_S_UNDEFINED) {
	    fseek(rawdata_fp, (long) (rawdata_start_addr + r->pr_vaddr), 0);
	    fwrite((void *) &(s->pn_value), sizeof (long), 1, rawdata_fp);
	}
    }

    fp = rawdata_fp;

    /*
     * Write File Header
     */
    coff_filehdr.f_magic = (unsigned short) I386MAGIC;
    coff_filehdr.f_nscns = (unsigned short) NUMOF_SECTIONS;
    coff_filehdr.f_timdat = (long) time(0);
    coff_filehdr.f_symptr = (long)
	(FILHSZ + NUMOF_SECTIONS * SCNHSZ +
	 rawdata_sz * sizeof (char) + reltbl_sz * RELSZ);
    coff_filehdr.f_nsyms = (long) (symtbl_sz + FIRST_REGULAR_SYMIDX);
    coff_filehdr.f_opthdr = (unsigned short) 0;
    coff_filehdr.f_flags = (unsigned short) (F_LNNO | F_AR32WR);

    fseek(fp, 0L, 0);
    fwrite((void *) &coff_filehdr, FILHSZ, 1, fp);

    /*
     * Write Section Headers
     */
    for (i = 0; i < NUMOF_SECTIONS; i++) {
	strncpy(coff_scnhdr[i].s_name, sec_names[i], SYMNMLEN);
	coff_scnhdr[i].s_paddr = 0;
	coff_scnhdr[i].s_vaddr = 0;
	coff_scnhdr[i].s_size = 0;
	coff_scnhdr[i].s_scnptr = 0;
	coff_scnhdr[i].s_relptr = 0;
	coff_scnhdr[i].s_lnnoptr = 0;
	coff_scnhdr[i].s_nreloc = 0;
	coff_scnhdr[i].s_nlnno = 0;
	coff_scnhdr[i].s_flags = (short) sec_types[i];
    }

    coff_scnhdr[SEC_DATA].s_size = (long) (rawdata_sz * sizeof (char));
    coff_scnhdr[SEC_DATA].s_scnptr = (long) (FILHSZ + NUMOF_SECTIONS * SCNHSZ);
    coff_scnhdr[SEC_DATA].s_relptr = (long)
	(FILHSZ + NUMOF_SECTIONS * SCNHSZ +
	 rawdata_sz * sizeof (char));
    coff_scnhdr[SEC_DATA].s_nreloc = (unsigned short) reltbl_sz;

    coff_scnhdr[SEC_BSS].s_paddr = coff_scnhdr[SEC_DATA].s_size;
    coff_scnhdr[SEC_BSS].s_vaddr = coff_scnhdr[SEC_DATA].s_size;

    fwrite((void *) &coff_scnhdr[0], SCNHSZ, NUMOF_SECTIONS, fp);

    /*
     * Raw Data Area already in the right place
     */

    fseek(fp, (long) rawdata_addr, 0);

    /*
     * Write Relocation Entries
     */
    coff_reloc.r_type = (unsigned short) R_DIR32;
    for (r = reloc_first_entry; r < reloc_entry; r++) {
	coff_reloc.r_vaddr = (long) (r->pr_vaddr);
	s = (PSYMENT *) (symtbl_first_entry + r->pr_symndx);
	if (s->pn_type != P_S_UNDEFINED)
	    coff_reloc.r_symndx = (long) SEC_DATA_SYM_IDX;
	else
	    coff_reloc.r_symndx = (long) (r->pr_symndx + FIRST_REGULAR_SYMIDX);
	fwrite((void *) &coff_reloc, RELSZ, 1, fp);
    }

    /*
     * Write Special Symbol Table Entries
     */

    /*
     * Write symbol ".file"
     */

    strncpy(coff_syment.n_name, ".file", SYMNMLEN);
    coff_syment.n_value = (long) 0;
    coff_syment.n_scnum = (short) N_DEBUG;
    coff_syment.n_type = (unsigned short) T_NULL;
    coff_syment.n_sclass = (char) C_FILE;
    coff_syment.n_numaux = (char) 1;
    fwrite((void *) &coff_syment, SYMESZ, 1, fp);

    /* Auxiliary entry of the symbol ".file" */
    auxs = (AUXENT *) (&coff_syment);
    strncpy(auxs->x_file.x_fname, fname, FILNMLEN);
    chptr = (char *) (CharPtr(auxs) + FILNMLEN);
    i = FILNMLEN;
    for (; i < sizeof (SYMENT); i++, chptr++)
	*chptr = 0;
    fwrite((void *) &coff_syment, SYMESZ, 1, fp);

    /*
     * Write special section symbols ".text", ".data" and ".bss"
     */
    for (i = 1; i <= NUMOF_SECTIONS; i++) {
	/* Put the section name into the symbol table */
	strncpy(coff_syment.n_name, sec_names[(i - 1)], SYMNMLEN);
	coff_syment.n_value = (long) 0;
	coff_syment.n_scnum = (short) i;
	coff_syment.n_type = (unsigned short) T_NULL;
	coff_syment.n_sclass = (char) C_STAT;
	coff_syment.n_numaux = (char) 1;
	if ((i - 1) == SEC_BSS) {
	    /*
	     * Update symbol table entry of symbol ".bss"
	     */
	    coff_syment.n_value = coff_scnhdr[SEC_DATA].s_size;
	}
	fwrite((void *) &coff_syment, SYMESZ, 1, fp);
	/* Auxiliary entry of the section symbol */
	coff_syment.n_zeroes = (long) 0;
	coff_syment.n_offset = (long) 0;
	coff_syment.n_value = (long) 0;
	coff_syment.n_scnum = (short) 0;
	coff_syment.n_type = (unsigned short) 0;
	coff_syment.n_sclass = (char) 0;
	coff_syment.n_numaux = (char) 0;
	if ((i - 1) == SEC_DATA) {
	    /*
	     * Update auxiliary symbol table entry of symbol ".data"
	     */
	    auxs = (AUXENT *) (&coff_syment);
	    auxs->x_scn.x_scnlen = (long) coff_scnhdr[SEC_DATA].s_size;
	    auxs->x_scn.x_nreloc = (short) coff_scnhdr[SEC_DATA].s_nreloc;
	    auxs->x_scn.x_nlinno = (short) coff_scnhdr[SEC_DATA].s_nlnno;
	}
	fwrite((void *) &coff_syment, SYMESZ, 1, fp);
    }

    /*
     * Write Regular Symbol Table Entries
     */
    coff_syment.n_type = (unsigned short) T_NULL;
    coff_syment.n_numaux = (char) 0;
    strtbl_sz = sizeof (long);
    for (s = symtbl_first_entry; s < symtbl_entry; s++) {
	if (s->pn_len <= SYMNMLEN)
	    strncpy(coff_syment.n_name,
		    strtbl_start_addr + s->pn_offset,
		    SYMNMLEN);
	else {
	    coff_syment.n_zeroes = (long) 0;
	    coff_syment.n_offset = (long) strtbl_sz;
	    strtbl_sz += ((unsigned int) s->pn_len + 1);
	}
	coff_syment.n_value = (long) s->pn_value;
	if (s->pn_type == P_S_UNDEFINED) {
	    coff_syment.n_scnum = (short) N_UNDEF;
	    coff_syment.n_sclass = (char) C_EXT;
	}
	else if (s->pn_type == P_S_LOCAL) {
	    coff_syment.n_scnum = (short) (SEC_DATA + 1);
	    coff_syment.n_sclass = (char) C_STAT;
	}
	else if (s->pn_type == P_S_GLOBAL) {
	    coff_syment.n_scnum = (short) (SEC_DATA + 1);
	    coff_syment.n_sclass = (char) C_EXT;
	}
	else
	    fatal_error(FE_IN_PCKG4, 0);

	fwrite((void *) &coff_syment, SYMESZ, 1, fp);
    }

    /*
     * Write String Table
     */
    fwrite((void *) &strtbl_sz, sizeof (long), 1, fp);
    for (s = symtbl_first_entry; s < symtbl_entry; s++) {
	if (s->pn_len > SYMNMLEN)
	    fwrite((void *) (strtbl_start_addr + s->pn_offset),
		   ((unsigned int) s->pn_len + 1), 1, fp);
    }

    /*
     * Close the file and free the malloced area
     */
    fclose(fp);
    rename(rawdata_fname, fname);
    free((char *) storage_addr);
    storage_addr = (char *) -1;

    return (1);
}

#endif /* PCKG_OBJFORMAT_COFF */





#ifdef PCKG_OBJFORMAT_OMF86

/****************************************************************************
 *			Create OMF86 File                                   *
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
{"", "_DATA", "DATA", "_BSS", "BSS", "CONST", "DGROUP", "CONST", "_TEXT", "CODE"};


/*
 * SEGDEF Record Macros
 */
#define NUMOF_SEGS				4

#define SEGDEF_ATTRIB 			0xA8

#define SEGDEF_DATASEG_IDX 		1
#define SEGDEF_BSSSEG_IDX 		2
#define SEGDEF_CONSTSEG_IDX 	3
#define SEGDEF_TEXTSEG_IDX 		4

static unsigned char SEGDEF_SEG_IDX[NUMOF_SEGS] =
{
    LNAMES_SEG_DATA_IDX,
    LNAMES_SEG_BSS_IDX,
    LNAMES_SEG_CONST_IDX,
    LNAMES_SEG_TEXT_IDX};

static unsigned char SEGDEF_CLS_IDX[NUMOF_SEGS] =
{
    LNAMES_CLS_DATA_IDX,
    LNAMES_CLS_BSS_IDX,
    LNAMES_CLS_CONST_IDX,
    LNAMES_CLS_CODE_IDX};

static unsigned char SEGDEF_OVL_IDX[NUMOF_SEGS] =
{
    LNAMES_NULL_IDX,
    LNAMES_NULL_IDX,
    LNAMES_NULL_IDX,
    LNAMES_NULL_IDX};


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
		fwrite((void *)&omf86_record[0], (long)(*reclenaddr+3), 1, fp);		\
		}




coff_open_rawdata_file(fname)
    char *fname;
{

    if ((rawdata_fp = fopen(fname, "w+b")) == NULL)
	return (PCKG_FOPEN_FAIL);

    rawdata_addr = rawdata_start_addr = 0;
    return (1);
}


coff_create_obj_file(fname)
    char *fname;
{
    register unsigned char *recptr;
    register unsigned char chksum;
    register unsigned char *p;
    register unsigned char ch;
    unsigned char *namelenaddr;
    unsigned short *reclenaddr;
    FILE *fp;
    long  i, j;
    unsigned long rawdata_sz, reltbl_sz, symtbl_sz, strtbl_sz;

    /*
     * Long Word Boundary for raw data
     */

    coff_align4_rawdata();

    /*
     * Sizes of package areas
     */
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

    /*
     * Open the file for write access
     */
    if ((fp = fopen(fname, "wb")) == NULL) {
	free((char *) storage_addr);	/* Free the malloced area */
	storage_addr = (char *) -1;
	return (PCKG_FOPEN_FAIL);
    }

    /*
     * Create THEADR Translator Header Record
     */

    {
	OMF86_REC_ENTRY(RT_THEADR);
	namelenaddr = recptr++;
	p = (unsigned char *) (fname);
	for (i = 0; (ch = *p) && (i < 255); i++, p++)
	    *recptr++ = ch;
	*namelenaddr = (unsigned char) i;
	OMF86_REC_EXIT;
    }

    /*
     * Create COMENT Comment Record
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

    {
	for (j = 0; j < NUMOF_SEGS; j++) {
	    OMF86_REC_ENTRY(RT_SEGDEF);
	    *recptr++ = SEGDEF_ATTRIB;
	    if ((j + 1) != SEGDEF_TEXTSEG_IDX)
		*(long *) recptr = 0;
	    else
		*(long *) recptr = (long) (rawdata_sz);
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
     *    Steps:
     *            1.      Traverse the symbol table to create PUBDEF records for
     *                    public symbols. At the same time, collect external symbols.
     *            2.      Create EXTDEF records.
     */
    {
	PSYMENT *sym;
	PSYMENT **extsym;
	PSYMENT **e;
	long  extsym_idx;

	/* Use the hash table area to store external symbol addresses */
	extsym = (PSYMENT **) hashtbl_start_addr;
	extsym_idx = 0;

	for (sym = symtbl_first_entry; sym < symtbl_entry; sym++) {
	    if (sym->pn_type == P_S_UNDEFINED) {	/* External Symbol */
		*extsym++ = sym;
		sym->pn_value = ++extsym_idx;
	    }
	    else if (sym->pn_type == P_S_GLOBAL) {	/* Public Symbol */
		OMF86_REC_ENTRY(RT_PUBDEF);
		*recptr++ = GRPDEF_NOGROUP_IDX;
		*recptr++ = SEGDEF_TEXTSEG_IDX;
		*recptr++ = (unsigned char) sym->pn_len;
		p = (unsigned char *) (strtbl_start_addr + sym->pn_offset);
		for (i = 0; ch = *p; i++, p++)
		    *recptr++ = ch;
		*(long *) recptr = sym->pn_value;
		recptr += sizeof (long);
		*recptr++ = TYPDEF_NOTYPE_IDX;
		OMF86_REC_EXIT;
	    }
	}

	/*
	 * Create EXTDEF Records
	 */
	if (extsym_idx > MAX_NUMOF_EXTDEFS) {
	    free((char *) storage_addr);	/* Free the malloced area */
	    storage_addr = (char *) -1;
	    return (PCKG_EXTS_FULL);
	}

	for (e = (PSYMENT **) hashtbl_start_addr; e < extsym; e++) {
	    OMF86_REC_ENTRY(RT_EXTDEF);
	    sym = *e;
	    *recptr++ = (unsigned char) sym->pn_len;
	    p = (unsigned char *) (strtbl_start_addr + sym->pn_offset);
	    for (i = 0; ch = *p; i++, p++)
		*recptr++ = ch;
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
	PRELOC *rs;
	PRELOC *re;
	PSYMENT *sym;
	long  blkstart;
	long  blkend;
	long  ledatasz;
	long  ledatabuf[0x100];
	long  off;

	rs = reloc_first_entry;
	rwptr = rawdata_start_addr;

	fseek(rawdata_fp, (long) rwptr, 0);

	for (; rwptr < rawdata_addr;) {
	    /*
	     * Create LEDATA Record
	     */
	    /*
	     * fprintf(stderr,"+");       fflush(stderr);
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
		for (re = rs; (re < reloc_entry) && (re->pr_vaddr < blkend); re++) ;
		re--;
		if ((rs <= re) && ((re->pr_vaddr + sizeof (long)) > blkend)) {
		    ledatasz = (((long) re->pr_vaddr) - ((long) blkstart));
		    re--;
		}
	    }

	    /* Copy data block into LEDATA record */

	    fread(ledatabuf, ledatasz, 1, rawdata_fp);
	    memcpy(recptr, ledatabuf, ledatasz);

	    recptr += ledatasz;
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
		    sym = (symtbl_first_entry + rs->pr_symndx);
		    off = (long) (((long) rs->pr_vaddr) - ((long) blkstart));
		    *recptr++ = FIXUPP_LOCAT_LOB(off);
		    *recptr++ = FIXUPP_LOCAT_HOB(off);
		    if (sym->pn_type != P_S_UNDEFINED) {	/* Local
								 * Symbol
								 */
			*recptr++ = FIXUPP_LOCAL_FIXDAT;
			*recptr++ = SEGDEF_TEXTSEG_IDX;
			*(long *) recptr = sym->pn_value;
			recptr += sizeof (long);
		    }
		    else {	/* External Symbol */
			*recptr++ = FIXUPP_EXTERNAL_FIXDAT;
			if (sym->pn_value >= 0x80) {
			    *recptr++ = (unsigned char)
				(0x80 | (sym->pn_value / 256));
			    *recptr++ = (unsigned char) (sym->pn_value % 256);
			}
			else {
			    *recptr++ = (unsigned char) sym->pn_value;
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

    {
	OMF86_REC_ENTRY(RT_MODEND);
	*recptr++ = MODEND_MODTYPE;
	OMF86_REC_EXIT;
    }

    /*
     * Close the file and free the malloced area
     */

    fclose(rawdata_fp);

    fclose(fp);
    free((char *) storage_addr);
    storage_addr = (char *) -1;
    return (1);
}

#endif /* PCKG_OBJFORMAT_OMF86 */





#ifdef PCKG_OBJFORMAT_AOUT

#include <a.out.h>
#include <stab.h>

/****************************************************************************
 *                         Create A.OUT File                                *
 ****************************************************************************/


coff_open_rawdata_file(fname)
    char *fname;
{
    struct exec aout_filehdr;

    if ((rawdata_fp = fopen(fname, "w+b")) == NULL) {
	return (PCKG_FOPEN_FAIL);
    }

    /* save the file name for later use */

    strcpy(rawdata_fname, fname);

    /* write dummy file header */

    fwrite((void *) &aout_filehdr, sizeof (aout_filehdr), 1, rawdata_fp);

    rawdata_addr = rawdata_start_addr = (char *) ftell(rawdata_fp);

    return (1);
}


coff_create_obj_file(fname)
    char *fname;
{
    struct exec aout_filehdr;
    struct nlist aout_syment;
    struct reloc_info_68k aout_reloc;
    register PRELOC *r;
    register PSYMENT *s;
    unsigned long rawdata_sz, reltbl_sz, symtbl_sz, strtbl_sz;
    FILE *fp;

    /*
     * Long Word Boundary
     */

    coff_align4_rawdata();


    /*
     * Sizes of package areas
     */
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

    /*
     * Update rawdata area for relocatable local variables
     */
    for (r = reloc_first_entry; r < reloc_entry; r++) {
	s = (PSYMENT *) (symtbl_first_entry + r->pr_symndx);
	if (s->pn_type != P_S_UNDEFINED) {
	    fseek(rawdata_fp, rawdata_start_addr + r->pr_vaddr, 0);
	    fwrite((void *) &(s->pn_value), sizeof (long), 1, rawdata_fp);
	}
    }

    fp = rawdata_fp;

    /*
     * Write File Header
     */
    aout_filehdr.a_dynamic = 0;
    aout_filehdr.a_toolversion = 0;
    aout_filehdr.a_machtype = M_68020;
    aout_filehdr.a_magic = OMAGIC;
    aout_filehdr.a_text = 0;
    aout_filehdr.a_data = rawdata_sz;
    aout_filehdr.a_bss = 0;
    aout_filehdr.a_syms = symtbl_sz * sizeof (aout_syment);
    aout_filehdr.a_entry = 0;
    aout_filehdr.a_trsize = 0;
    aout_filehdr.a_drsize = reltbl_sz * sizeof (aout_reloc);

    fseek(fp, 0L, 0);

    fwrite((void *) &aout_filehdr, sizeof (aout_filehdr), 1, fp);

    /*
     * Write Data Section
     */

    fseek(fp, rawdata_addr, 0);

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
	aout_reloc.r_address = r->pr_vaddr;
	s = (PSYMENT *) (symtbl_first_entry + r->pr_symndx);
	if (s->pn_type != P_S_UNDEFINED) {
	    aout_reloc.r_symbolnum = N_DATA;
	    aout_reloc.r_extern = 0;
	}
	else {
	    aout_reloc.r_symbolnum = r->pr_symndx;
	    aout_reloc.r_extern = 1;
	}
	fwrite((void *) &aout_reloc, sizeof (aout_reloc), 1, fp);
    }

    /*
     * Write Symbol Table Entries
     */
    /* Fix values for symbol table entries */
    aout_syment.n_other = 0;
    aout_syment.n_desc = 0;

    for (s = symtbl_first_entry; s < symtbl_entry; s++) {
	aout_syment.n_un.n_strx = s->pn_offset;
	if (s->pn_type != P_S_UNDEFINED)
	    aout_syment.n_type = N_DATA;
	else
	    aout_syment.n_type = 0;
	if (s->pn_type != P_S_LOCAL)
	    aout_syment.n_type |= N_EXT;
	aout_syment.n_value = s->pn_value;

	fwrite((void *) &aout_syment, sizeof (aout_syment), 1, fp);
    }

    /*
     * Write String Table
     */
    *LongPtr(strtbl_start_addr) = (long) strtbl_sz;
    fwrite((void *) strtbl_start_addr, strtbl_sz, 1, fp);

    /*
     * Close the file and free the malloced area
     */
    fclose(fp);
    rename(rawdata_fname, fname);
    free((char *) storage_addr);
    storage_addr = (char *) -1;

    return (1);
}

#endif /* PCKG_OBJFORMAT_AOUT */




#ifdef PCKG_OBJFORMAT_SPARC_AOUT

#include <a.out.h>
#include <stab.h>

/****************************************************************************
 *                      Create Sparc A.OUT File                             *
 ****************************************************************************/

coff_open_rawdata_file(fname)
    char *fname;
{
    struct exec aout_filehdr;

    if ((rawdata_fp = fopen(fname, "w+b")) == NULL) {
	return (PCKG_FOPEN_FAIL);
    }

    /* save the file name for later use */

    strcpy(rawdata_fname, fname);

    /* write dummy file header */

    fwrite((void *) &aout_filehdr, sizeof (aout_filehdr), 1, rawdata_fp);

    rawdata_addr = rawdata_start_addr = (char *) ftell(rawdata_fp);

    return (1);
}

coff_create_obj_file(fname)
    char *fname;
{
    struct exec aout_filehdr;
    struct nlist aout_syment;
    struct reloc_info_sparc aout_reloc;
    register PRELOC *r;
    register PSYMENT *s;
    unsigned long rawdata_sz, reltbl_sz, symtbl_sz, strtbl_sz;
    long  disp, val;
    FILE *fp;

    /*
     * Long Word Boundary
     */

    coff_align4_rawdata();

    /*
     * Sizes of package areas
     */
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

    /*
     * Update rawdata area for relocatable local variables
     * when they are displacements. Alsos, mark those relocation
     * entries as deleted.
     */
    for (r = reloc_first_entry; r < reloc_entry; r++) {
	s = (PSYMENT *) (symtbl_first_entry + r->pr_symndx);
	if (s->pn_type != P_S_UNDEFINED) {
	    if (r->pr_type == P_R_WDISP30) {
		disp = (long) ((long *) (s->pn_value) - (long *) (r->pr_vaddr));
		fseek(rawdata_fp, rawdata_start_addr + r->pr_vaddr, 0);
		fread(&val, sizeof (long), 1, rawdata_fp);
		fseek(rawdata_fp, rawdata_start_addr + r->pr_vaddr, 0);
		val |= (disp & MASK30);
		fwrite(&val, sizeof (long), 1, rawdata_fp);
		r->pr_type = P_R_DELETED;
		reltbl_sz--;
	    }
	    else if (r->pr_type == P_R_WDISP22) {
		disp = (long) ((long *) (s->pn_value) - (long *) (r->pr_vaddr));
		fseek(rawdata_fp, rawdata_start_addr + r->pr_vaddr, 0);
		fread(&val, sizeof (long), 1, rawdata_fp);
		fseek(rawdata_fp, rawdata_start_addr + r->pr_vaddr, 0);
		val |= (disp & MASK22);
		fwrite(&val, sizeof (long), 1, rawdata_fp);
		r->pr_type = P_R_DELETED;
		reltbl_sz--;
	    }
	}
    }

    fp = rawdata_fp;

    /*
     * Write File Header
     */
    aout_filehdr.a_dynamic = 0;
    aout_filehdr.a_toolversion = 1;
    aout_filehdr.a_machtype = M_SPARC;
    aout_filehdr.a_magic = OMAGIC;
    aout_filehdr.a_text = 0;
    aout_filehdr.a_data = rawdata_sz;
    aout_filehdr.a_bss = 0;
    aout_filehdr.a_syms = symtbl_sz * sizeof (aout_syment);
    aout_filehdr.a_entry = 0;
    aout_filehdr.a_trsize = 0;
    aout_filehdr.a_drsize = reltbl_sz * sizeof (aout_reloc);

    fseek(rawdata_fp, 0L, 0);

    fwrite((void *) &aout_filehdr, sizeof (aout_filehdr), 1, fp);

    /*
     * Write Data Section
     */

    fseek(fp, rawdata_addr, 0);

    /*
     * Write Relocation Entries
     */
    for (r = reloc_first_entry; r < reloc_entry; r++) {
	if (r->pr_type != P_R_DELETED) {
	    aout_reloc.r_address = r->pr_vaddr;
	    s = (PSYMENT *) (symtbl_first_entry + r->pr_symndx);
	    if (s->pn_type != P_S_UNDEFINED) {
		aout_reloc.r_index = N_DATA;
		aout_reloc.r_extern = 0;
		aout_reloc.r_addend = s->pn_value;
		switch (r->pr_type) {
		    case P_R_VIR32:
			aout_reloc.r_type = RELOC_32;
			break;
		    case P_R_HI22:
			aout_reloc.r_type = RELOC_HI22;
			break;
		    case P_R_LO10:
			aout_reloc.r_type = RELOC_LO10;
			break;
		    default:
			fatal_error(FE_IN_PCKG5, 0);
			break;
		}
	    }
	    else {
		aout_reloc.r_index = r->pr_symndx;
		aout_reloc.r_extern = 1;
		switch (r->pr_type) {
		    case P_R_VIR32:
			aout_reloc.r_type = RELOC_32;
			aout_reloc.r_addend = s->pn_value;
			break;
		    case P_R_WDISP30:
			aout_reloc.r_type = RELOC_WDISP30;
			aout_reloc.r_addend = -(long) (r->pr_vaddr);
			break;
		    case P_R_WDISP22:
			aout_reloc.r_type = RELOC_WDISP22;
			aout_reloc.r_addend = -(long) (r->pr_vaddr);
			break;
		    case P_R_HI22:
			aout_reloc.r_type = RELOC_HI22;
			aout_reloc.r_addend = s->pn_value;
			break;
		    case P_R_LO10:
			aout_reloc.r_type = RELOC_LO10;
			aout_reloc.r_addend = s->pn_value;
			break;
		    default:
			fatal_error(FE_IN_PCKG6, 0);
			break;
		}
	    }
	    fwrite((void *) &aout_reloc, sizeof (aout_reloc), 1, fp);
	}
    }

    /*
     * Write Symbol Table Entries
     */
    /* Fix values for symbol table entries */
    aout_syment.n_other = 0;
    aout_syment.n_desc = 0;

    for (s = symtbl_first_entry; s < symtbl_entry; s++) {
	aout_syment.n_un.n_strx = s->pn_offset;
	if (s->pn_type != P_S_UNDEFINED)
	    aout_syment.n_type = N_DATA;
	else
	    aout_syment.n_type = 0;
	if (s->pn_type != P_S_LOCAL)
	    aout_syment.n_type |= N_EXT;
	aout_syment.n_value = s->pn_value;

	fwrite((void *) &aout_syment, sizeof (aout_syment), 1, fp);
    }

    /*
     * Write String Table
     */
    *LongPtr(strtbl_start_addr) = (long) strtbl_sz;
    fwrite((void *) strtbl_start_addr, strtbl_sz, 1, fp);

    /*
     * Close the file and free the malloced area
     */
    fclose(fp);
    rename(rawdata_fname, fname);
    free((char *) storage_addr);
    storage_addr = (char *) -1;

    return (1);
}

#endif /* PCKG_OBJFORMAT_SPARC_AOUT */




#ifdef PCKG_OBJFORMAT_88KBCS

#include <sys/types.h>
#include <filehdr.h>
#include <scnhdr.h>
#include <reloc.h>
#include <linenum.h>
#include <syms.h>

/*
 * Section Headers and Names
 */

#define NUMOF_SECTIONS 			2

#define SEC_TEXT 			0
#define SEC_DATA 			1

#define SEC_TEXT_SYM_IDX 		2
#define SEC_DATA_SYM_IDX 		4
#define FIRST_REGULAR_SYMIDX 		6

static char sec_names[NUMOF_SECTIONS][SYMNMLEN] =
{".text", ".data"};
static short sec_types[NUMOF_SECTIONS] =
{STYP_TEXT, STYP_DATA};



/****************************************************************************
 *                     Create 88OpenBCS COFF File                           *
 ****************************************************************************/


coff_open_rawdata_file(fname)
    char *fname;
{
    FILHDR bcs_filehdr;
    SCNHDR bcs_scnhdr[NUMOF_SECTIONS];
    long  rawdata_alignment;
    char  rawdata_alignment_buf[8] =
    {0, 0, 0, 0, 0, 0, 0, 0};

    if ((rawdata_fp = fopen(fname, "w+b")) == NULL) {
	return (PCKG_FOPEN_FAIL);
    }

    /* save the file name for later use */

    strcpy(rawdata_fname, fname);

    /* write dummy file header */

    fwrite((void *) &bcs_filehdr, FILHSZ, 1, rawdata_fp);

    /* write dummy section headers */

    fwrite((void *) &bcs_scnhdr[0], SCNHSZ, NUMOF_SECTIONS, rawdata_fp);

    /* write raw data alignment zeroes */

    rawdata_alignment = 8 - ((FILHSZ + (NUMOF_SECTIONS * SCNHSZ)) & 0x07);
    if (rawdata_alignment != 8)
	fwrite((void *) &rawdata_alignment_buf[0], rawdata_alignment, 1, rawdata_fp);

    rawdata_addr = rawdata_start_addr = (char *) ftell(rawdata_fp);
    return (1);
}

coff_create_obj_file(fname)
    char *fname;
{
    register PRELOC *r;
    register PSYMENT *s;
    long  i;
    char *chptr;
    long  rawdata_alignment;
    char  rawdata_alignment_buf[8] =
    {0, 0, 0, 0, 0, 0, 0, 0};
    unsigned long rawdata_sz, reltbl_sz, symtbl_sz, strtbl_sz;
    FILHDR bcs_filehdr;
    SCNHDR bcs_scnhdr[NUMOF_SECTIONS];
    RELOC bcs_reloc;
    SYMENT bcs_syment;
    AUXENT *auxs;
    FILE *fp;

    /*
     * Raw data for sections is a multiple of eight bytes in length
     * and must be aligned to an 8-byte boundary.
     */
    if (((long) rawdata_addr & 0x07) != 0) {
	i = 8 - ((long) rawdata_addr & 0x07);
	fwrite((void *) rawdata_alignment_buf, i, 1, rawdata_fp);
	rawdata_addr += i;
    }

    rawdata_alignment = 8 - ((FILHSZ + (NUMOF_SECTIONS * SCNHSZ)) & 0x07);
    if (rawdata_alignment == 8)
	rawdata_alignment = 0;

    /*
     * Sizes of package areas
     */
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


    /*
     * Write File Header
     */
    bcs_filehdr.f_magic = (unsigned short) MC88MAGIC;
    bcs_filehdr.f_nscns = (unsigned short) NUMOF_SECTIONS;
    bcs_filehdr.f_timdat = (long) time(0);
    bcs_filehdr.f_symptr = (long)
	(FILHSZ +
	 (NUMOF_SECTIONS * SCNHSZ) +
	 rawdata_alignment +
	 (rawdata_sz * sizeof (char)) +
	 (reltbl_sz * RELSZ));
    bcs_filehdr.f_nsyms = (long) (symtbl_sz + FIRST_REGULAR_SYMIDX);
    bcs_filehdr.f_opthdr = (unsigned short) 0;
    bcs_filehdr.f_flags = (unsigned short) (F_LNNO | F_AR32W);

    /*
     * Create the real COFF file
     */

    fp = rawdata_fp;
    fseek(fp, 0L, 0);

    fwrite((void *) &bcs_filehdr, FILHSZ, 1, fp);

    /*
     * Write Section Headers
     */
    for (i = 0; i < NUMOF_SECTIONS; i++) {
	strncpy(bcs_scnhdr[i].s_name, sec_names[i], SYMNMLEN);
	bcs_scnhdr[i].s_paddr = 0;
	bcs_scnhdr[i].s_vaddr = 0;
	bcs_scnhdr[i].s_size = 0;
	bcs_scnhdr[i].s_scnptr = 0;
	bcs_scnhdr[i].s_relptr = 0;
	bcs_scnhdr[i].s_lnnoptr = 0;
	bcs_scnhdr[i].s_nreloc = 0;
	bcs_scnhdr[i].s_nlnno = 0;
	bcs_scnhdr[i].s_flags = (long) sec_types[i];
    }

    bcs_scnhdr[SEC_DATA].s_size = (long) (rawdata_sz * sizeof (char));
    bcs_scnhdr[SEC_DATA].s_scnptr = (long)
	(FILHSZ +
	 NUMOF_SECTIONS * SCNHSZ +
	 rawdata_alignment);
    bcs_scnhdr[SEC_DATA].s_relptr = (long)
	(FILHSZ +
	 NUMOF_SECTIONS * SCNHSZ +
	 rawdata_alignment +
	 (rawdata_sz * sizeof (char)));
    bcs_scnhdr[SEC_DATA].s_nreloc = (unsigned long) reltbl_sz;

    fwrite((void *) &bcs_scnhdr[0], SCNHSZ, NUMOF_SECTIONS, fp);

    /*
     * Write Raw Data Area
     */
    fseek(fp, (long) rawdata_addr, 0);

    /*
     * Write Relocation Entries
     */
    bcs_reloc.r_offset = (unsigned short) 0;
    for (r = reloc_first_entry; r < reloc_entry; r++) {
	bcs_reloc.r_vaddr = (long) r->pr_vaddr;
	bcs_reloc.r_symndx = (long) (r->pr_symndx + FIRST_REGULAR_SYMIDX);
	switch (r->pr_type) {
	    case P_R_VIR32:
		bcs_reloc.r_type = (unsigned short) R_VRT32;
		break;
	    case P_R_REL26:
		bcs_reloc.r_type = (unsigned short) R_PCR26L;
		break;
	    case P_R_RELH16:
		bcs_reloc.r_type = (unsigned short) R_HVRT16;
		break;
	    case P_R_RELL16:
		bcs_reloc.r_type = (unsigned short) R_LVRT16;
		break;
	    default:
		fatal_error(FE_IN_PCKG7, 0);
		break;
	}
	fwrite((void *) &bcs_reloc, RELSZ, 1, fp);
    }


    /*
     * Write Special Symbol Table Entries
     */

    /*
     * Write symbol ".file"
     */
    strncpy(bcs_syment.n_name, ".file", SYMNMLEN);
    bcs_syment.n_value = (long) 0;
    bcs_syment.n_scnum = (short) N_DEBUG;
    bcs_syment.n_type = (unsigned short) T_NULL;
    bcs_syment.n_sclass = (char) C_FILE;
    bcs_syment.n_numaux = (char) 1;
#ifdef MOTO_BCS
    bcs_syment.n_pad = (short) 0;
#else
    bcs_syment.n_pad1 = (char) 0;
    bcs_syment.n_pad2 = (char) 0;
#endif
    fwrite((void *) &bcs_syment, SYMESZ, 1, fp);
    /* Auxiliary entry of the symbol ".file" */
    auxs = (AUXENT *) (&bcs_syment);
#if defined(MOTO_BCS) || defined(SysVR4)
    strncpy(auxs->x_file.x_fname, fname, FILNMLEN);
#else
    strncpy(auxs->x_name, fname, FILNMLEN);
#endif
    chptr = (char *) (CharPtr(auxs) + FILNMLEN);
    i = FILNMLEN;
    for (; i < sizeof (SYMENT); i++, chptr++)
	*chptr = 0;
    fwrite((void *) &bcs_syment, SYMESZ, 1, fp);

    /*
     * Write special section symbols ".text" and  ".data"
     */
    for (i = 1; i <= NUMOF_SECTIONS; i++) {
	/* Put the section name into the symbol table */
	strncpy(bcs_syment.n_name, sec_names[(i - 1)], SYMNMLEN);
	bcs_syment.n_value = (long) 0;
	bcs_syment.n_scnum = (short) i;
	bcs_syment.n_type = (unsigned short) T_NULL;
	bcs_syment.n_sclass = (char) C_STAT;
	bcs_syment.n_numaux = (char) 1;
#ifdef MOTO_BCS
	bcs_syment.n_pad = (short) 0;
#else
	bcs_syment.n_pad1 = (char) 0;
	bcs_syment.n_pad2 = (char) 0;
#endif
	fwrite((void *) &bcs_syment, SYMESZ, 1, fp);
	/* Auxiliary entry of the section symbol */
	bcs_syment.n_zeroes = (long) 0;
	bcs_syment.n_offset = (long) 0;
	bcs_syment.n_value = (long) 0;
	bcs_syment.n_scnum = (short) 0;
	bcs_syment.n_type = (unsigned short) 0;
	bcs_syment.n_sclass = (char) 0;
	bcs_syment.n_numaux = (char) 0;
#ifdef MOTO_BCS
	bcs_syment.n_pad = (short) 0;
#else
	bcs_syment.n_pad1 = (char) 0;
	bcs_syment.n_pad2 = (char) 0;
#endif
	if ((i - 1) == SEC_DATA) {
	    /*
	     * Update auxiliary symbol table entry of symbol ".data"
	     */
	    auxs = (AUXENT *) (&bcs_syment);
	    auxs->x_scn.x_scnlen = (long) bcs_scnhdr[SEC_DATA].s_size;
	    auxs->x_scn.x_nreloc = (short) bcs_scnhdr[SEC_DATA].s_nreloc;
	    auxs->x_scn.x_nlinno = (short) bcs_scnhdr[SEC_DATA].s_nlnno;
	}
	fwrite((void *) &bcs_syment, SYMESZ, 1, fp);
    }


    /*
     * Write Regular Symbol Table Entries
     */
    bcs_syment.n_type = (unsigned short) T_NULL;
    bcs_syment.n_numaux = (char) 0;
#ifdef MOTO_BCS
    bcs_syment.n_pad = (short) 0;
#else
    bcs_syment.n_pad1 = (char) 0;
    bcs_syment.n_pad2 = (char) 0;
#endif
    strtbl_sz = sizeof (long);
    for (s = symtbl_first_entry; s < symtbl_entry; s++) {
	if (s->pn_len <= SYMNMLEN)
	    strncpy(bcs_syment.n_name,
		    strtbl_start_addr + (s->pn_offset),
		    SYMNMLEN);
	else {
	    bcs_syment.n_zeroes = (long) 0;
	    bcs_syment.n_offset = (long) strtbl_sz;
	    strtbl_sz += ((unsigned int) s->pn_len + 1);
	}
	bcs_syment.n_value = (long) s->pn_value;
	if (s->pn_type == P_S_UNDEFINED) {
	    bcs_syment.n_scnum = (short) N_UNDEF;
	    bcs_syment.n_sclass = (char) C_EXT;
	}
	else if (s->pn_type == P_S_LOCAL) {
	    bcs_syment.n_scnum = (short) (SEC_DATA + 1);
	    bcs_syment.n_sclass = (char) C_STAT;
	}
	else if (s->pn_type == P_S_GLOBAL) {
	    bcs_syment.n_scnum = (short) (SEC_DATA + 1);
	    bcs_syment.n_sclass = (char) C_EXT;
	}
	else
	    fatal_error(FE_IN_PCKG8, 0);
	fwrite((void *) &bcs_syment, SYMESZ, 1, fp);
    }

    /*
     * Write String Table
     */
    fwrite((void *) &strtbl_sz, sizeof (long), 1, fp);
    for (s = symtbl_first_entry; s < symtbl_entry; s++) {
	if (s->pn_len > SYMNMLEN)
	    fwrite((void *) (strtbl_start_addr + (s->pn_offset)),
		   ((unsigned int) s->pn_len + 1), 1, fp);
    }

    /*
     * Close the file and free the malloced area
     */
    fclose(fp);
    rename(rawdata_fname, fname);
    free((char *) storage_addr);
    storage_addr = (char *) -1;

    return (1);
}

#endif /* PCKG_OBJFORMAT_88KBCS */


#endif /* PACKAGE  */
