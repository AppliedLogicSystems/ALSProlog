/*===================================================================*
 |			symtab.c     
 |		Copyright (c) 1990-1995 Applied Logic Systems, Inc.
 |
 |			-- symbol table routines for ALS-Prolog
 |
 | Author: Kevin A. Buettner
 | Created: 11/28/90
 | 10/26/94 - C. Houpt -- Removed + 1 increment of string constants in the 
 |		initialization of initial_table because it is incompatable with 
 |		MetroWerks C. Also, various char* and UCHAR* casts.
 *===================================================================*/
#include "defs.h"

#ifdef DEBUG
/*
 * The following variable is for debugging purposes.
 */

static int collisions;
static int nsearches;

#endif

#include "pckg.h"
#include "pckgcoff.h"

/*
 * tablessizes gives the tables sizes to use.  The first component of each
 * triple is the size to allocate.  This will be a power of two and arranged
 * so that each successive triple will require twice as much space.  The
 * second component of the triple is the actual table size.  The third component
 * is the cutoff point.  This number is about 84% of the table size.
 *
 * On the 88k implementation (6 bit tags, 10 bit arities), there is only 16 bits
 * of symbol to work with, so the maximum table size is 65521.  On the other
 * implementations (4 bit constant tags, 8 bit arities), it is possible to
 * get symbol tables with slightly over a million entries.
 */


struct ts {
    long  allocation;
    long  size;
    long  cutoff;
} tablesizes[] = {
/*   allocation, size,    cutoff  */
	{4096,       4093,    3438},
	{8192,       8191,    6880},
	{16384,      16381,   13760},
	{32768,      32749,   27509},
	{65536,      65521,   55038},
	{131072,     131071,  110100},
	{262144,     262139,  220197},
	{524288,     524287,  440401},
	{1048576,    1048573, 880801}
};


/*//static int ts_allocidx = 0;*/	/* index to look at in doing next allocation */

#ifdef arch_m88k
#define TS_LASTIDX 4		/* Last valid index in table */
#else
#define TS_LASTIDX 8
#endif

/*
 * The initial token table will have 2048 entries allocated to it, although
 * it will only support 2039 actual entries.  The cutoff point will be 1712
 * at which point a larger table will be allocated (if possible).
 */

/*//static long ts_allocated = 2048;*/	/* number of entries allocated */
/*//static unsigned long ts_prime = 2039;*/		/* actual size (the prime number) */
/*//static unsigned long ts_cutoff = 1712;*/		/* the cutoff point */
/*//static unsigned long ts_next = TK_EOF + 1;*/	/* the next token index */

int tok_table_size(void)
{
	return(ts_prime);
}


/*
 * The structure below is only used for initialization.  After initialization
 * the space could be used for something else, but I am not sure how to
 * arrange this right now.  Prior to 10/7/93, this area was the real initial
 * table.  We have the saved state mechanism now which requires that large
 * areas like the token table be put into a common piece of memory.
 */

#define TK(ppsym, name) {sizeof(name)-1, (UCHAR *) name, 0, 0}
#define OP(ppsym, name, a, b) {sizeof(name)-1, (UCHAR *) name, a, b}
static tkentry initial_table[] =
{
    {0, (UCHAR *) "used up entry", 0, 0},
#include "newtokini.h"            /* initial definitions of tokens */
};
#undef TK
#undef OP

/*-@[5.1]@-------------------------------------------------------------
 * toktable is accessible to the world.  It is a pointer to the array of
 * tokens.  At times it may be necessary to make this array bigger, so more
 * space is malloc'd, the tokens copied over and the entries rehashed.
 *
 *---------------------------------------------------------------------*/

/*//tkentry *toktable = (tkentry *) 0;*/

/*---------------------------------------------------------------------
 * hashtable is accessible only to this module.  It points to an array of
 * pointers to the tokens.
 *---------------------------------------------------------------------*/

/*//static tkentry **hashtable;*/

/*---------------------------------------------------------------------
 * Variable strings points to the next character position which may be
 * allocated for a token string.
 *---------------------------------------------------------------------*/

/*//static UCHAR *strings;*/

/*---------------------------------------------------------------------
 * strings_last points to the last character in the area in which strings
 * points to.
 *---------------------------------------------------------------------*/

/*//static UCHAR *strings_last;*/

/*
 * strings_next is a pointer to future areas to be used by strings.  The first
 * longword (32 bits) of the area indicates the size of the current block.
 * The next longword is a pointer to the next block to use.  As the hashtables
 * are discarded due to the token table getting larger, these blocks will be
 * added to this list.  The strings area will definitely need more space as
 * the number of potential tokens grows.  If the average number of characters
 * used by each token is eight (actually seven, since we need a null byte
 * terminator), then the growths of the token area and the growth of the
 * string area will keep in step.  If the average number of characters per
 * token is greater than seven, then small regions will be malloc'd as needed
 * to keep up with this growth.
 */

/*//static long *strings_next = 0;*/

/*
 * char_to_tok_map is an array of which maps characters to their
 * corresponding tokens.
 */

/*//long *char_to_tok_map;*/

/* Prototypes */
static	tkentry ** lookup		( UCHAR *, size_t * );
static	void	new_string_space	( size_t );
static	void	increase_table_size	( void );



/*
 * lookup is called by both find_token and probe_token.  It will hash the
 * string and at the same time determine the length of the string for
 * possible insertion by find_token.  It will then examine the hash table and
 * return either the first free hash address if the string is not in the
 * table or the hash address at which the string is located.  If the table is
 * full, a fatal error will occur.
 */

static tkentry **
lookup(name, len)
    UCHAR *name;		/* character string to hash */
    size_t *len;		/* length of string */
{
    register UCHAR *s =  name;
    register unsigned int n, shift;
    register unsigned long idx;

#ifdef DEBUG
    nsearches++;
#endif

    /*
     * Compute initial hash index and shift value.  Also find length
     * of string.
     */

    shift = (*s & 0x0f);
    idx = *s;
    if (idx) {
	idx ^= (*(s + 1));
    }
    for (n = 0; *s; n++, shift += 4) {
	idx = idx + ((*s++) << shift);
	if (shift >= 17)
	    shift -= 17;
    }

    /* If caller passed in non-nil pointer then set return length */
    if (len)
	*len = n;

    shift = ((idx >> 1) % (ts_prime - 1)) + 1;
    /* Range will be 1..ts_prime-1 */
    idx = (idx + (idx >> 9) + n) % ts_prime;

    /*
     * At this point idx contains the initial probe index.  shift contains
     * the value to add in case of collision.  We will reuse n to keep track
     * of the starting point.
     */

    n = idx;

    while 
		(hashtable[idx] && 
			strcmp((char *)name, (char *)hashtable[idx]->tkname) != 0) {
	idx += shift;
	if (idx >= ts_prime)
	    idx -= ts_prime;
	if (idx == n)
	    fatal_error(FE_SYMSPACE, 0);
#ifdef DEBUG
	collisions++;
#endif
    }

    return hashtable + idx;

}

/*
 * probe_token(s) attempts to look the given string up in the token table
 * returning the token index if the token is in the table, 0 otherwise.
 * (Index zero is not used by any token).
 */

long
probe_token(s)
    UCHAR *s;			/* string to find */
{
    tkentry **pos;

    pos = lookup(s, 0);		/* perform lookup */
    if (*pos)
	return *pos - toktable;	/* token is present */
    else
	return 0;		/* token is not present */
}


/*
 * find_token returns the index of the token whose string is s, adding it
 * to the token table if necessary.
 */

long
find_token(const UCHAR *cs)
{
    size_t len;
    tkentry **pos;
    UCHAR *s = (UCHAR *)cs;

    pos = lookup(s, &len);

    if (*pos)
		return *pos - toktable;	/* already in table */
    else {			/* must add to table */
	if (ts_next > ts_cutoff) {
	    increase_table_size();	/* increase size and rehash */
	    pos = lookup(s, 0);	/* rehash current */
	}

	*pos = toktable + ts_next;


	if ((int)(len + 1) > strings_last - strings)
		new_string_space(len + 1);	/* get more string space */
	
	toktable[ts_next].length = len;
	toktable[ts_next].tkname = strings;
	toktable[ts_next].unop = 0;
	toktable[ts_next].binop = 0;

	while ( (*strings++ = *s++) ) ;	/* copy the string over */

	return ts_next++;	/* return while advancing next token */
	/* index */
    }
}


/*
 * new_string_space is called when find_token sees that there is insufficient
 * space in the current strings area to store the token under consideration.
 * new_string_space will remedy this situation if possible.
 */

#define default_string_space_request 1024

static void
new_string_space(request)
    size_t request;
{
    if (strings_next && *strings_next > (signed) request) {
	strings = (UCHAR *) strings_next;
	strings_last = ((UCHAR *) strings_next) + *strings_next;
	strings_next = *(long **) (strings_next + 1);
    }
    else {
	if (request >= default_string_space_request)
	    request++;		/* add one for null byte */
	else
	    request = default_string_space_request;

	strings = (UCHAR *) ss_malloc(request,FE_STRSPACE);
	strings_last = strings + request - 1;
    }
}


/*
 * increase_table_size is called when it is desired to increase the number
 * of available table entries.  The space allocated for both the token table
 * and the hash table will double in size.  Moreover the token table requires
 * twice as much space as the hash table.  So we use the strategy of allocating
 * new space for the token table, and using the present space for the token
 * table as the hash table space.  The space that the hash table currently
 * inhabits will be given to the string area for its use.
 */

static void
increase_table_size()
{
    tkentry *new_table;
    register unsigned int i;

#ifdef DEBUG
    printf("tablesize = %ld, collisions/searches=%g\n",
	   ts_prime,
	   (double) collisions / (double) nsearches);
    collisions = 0;
    nsearches = 0;
#endif

    if (ts_cutoff >= ts_prime || ts_allocidx > TS_LASTIDX) {
	fatal_error(FE_SYMSPACE, 0);
    }
    else {
	/* FIXME: ss_malloc never returns if allocation fails; yet if
	   allocation fails, we should be able to proceed for a while */
	new_table = (tkentry *)
	    ss_malloc(sizeof (tkentry) * tablesizes[ts_allocidx].allocation,
		      FE_SYMSPACE);
	if (new_table == (tkentry *) 0)
	    ts_cutoff = ts_prime;	/* Unable to allocate more space */
	else {
	    /* Copy toktable to new_table */
	    memcpy((char *) new_table, (char *) toktable,
		   sizeof (tkentry) * ts_next);
	    /* Zero toktable for use as hash table */
	    memset((char *) toktable, 0, sizeof (tkentry) * ts_allocated);

	    /* give the space occupied by hash table to string area */
	    ((long *) hashtable)[0] = ts_allocated * sizeof (long);
	    ((long *) hashtable)[1] = (long) strings_next;
	    strings_next = (long *) hashtable;

	    /* give space occupied by token table to hash table */
	    hashtable = (tkentry **) toktable;

	    /* give the token table the new space */
	    toktable = new_table;

	    /* fix values telling us about size of new table */
	    ts_allocated = tablesizes[ts_allocidx].allocation;
	    ts_prime = tablesizes[ts_allocidx].size;
	    ts_cutoff = tablesizes[ts_allocidx].cutoff;
	    ts_allocidx++;

	    /* rehash */
	    for (i = 1; i < ts_next; i++)
		*lookup(toktable[i].tkname, 0) = toktable + i;
	}

    }
}


void
symtab_init()
{
    register unsigned int i;
    char chtokstr[2];

    if (!toktable) {
	/* allocate space for the token table */
	toktable = (tkentry *) ss_malloc(sizeof (tkentry) * ts_allocated,
			 			FE_SYMTAB_INIT);

	/* Copy initial_table to toktable */
	memcpy((char *) toktable, (char *) initial_table, sizeof initial_table);
	
	/* malloc space for the hash table */
	hashtable = (tkentry **) ss_malloc(ts_allocated * sizeof (tkentry **),
					   FE_SYMTAB_INIT);
	memset((char *) hashtable, 0, sizeof (tkentry **) * ts_allocated);

	/* hash the initial entries */
	for (i = 1; i < ts_next; i++)
	    *lookup(toktable[i].tkname, 0) = toktable + i;
	
	/* malloc space for the character-to-token map */
	char_to_tok_map = (long *) ss_malloc(sizeof(long) * N_TOK_CHARS,
					     FE_SYMTAB_INIT);
	/* Initialize the character-to_token map */
	chtokstr[1] = 0;
	for (i=0; i<N_TOK_CHARS; i++) {
	    chtokstr[0] = i;
	    char_to_tok_map[i] = find_token((UCHAR *)chtokstr);
	}
	
	/* register the globals in this module with the saved state mechanism */
	ss_register_global((long *) &ts_allocidx);
	ss_register_global((long *) &ts_allocated);
	ss_register_global((long *) &ts_prime);
	ss_register_global((long *) &ts_cutoff);
	ss_register_global((long *) &ts_next);
	ss_register_global((long *) &toktable);
	ss_register_global((long *) &hashtable);
	ss_register_global((long *) &strings);
	ss_register_global((long *) &strings_last);
	ss_register_global((long *) &strings_next);
	ss_register_global((long *) &char_to_tok_map);
    } else {
    	/* Copy initial_table to toktable */
		memcpy((char *) toktable, (char *) initial_table, sizeof initial_table);
    }
}


#ifdef PACKAGE

/*
 * #define PCKGLOAD_DEBUG 1
 * #define PCKGLOAD_PRINT_TOKENS 1
 */

/*
 * Intialize the token table from the package token table.
 * The initial tokens are still initialized by the function "symtab_init"
 * the package token table doesn't contain these initial tokens.
 * Each token in the package token table is represented as follows:
 *      1. Length       A character if length < 255 and
 *                      character 255 and a short value for length
 *                      (Note that character 255 and the short value for length
 *                       in reveresed order in the package token table)
 *      2. Name         Character string including the ending zero for token.
 *      3. unop         Unary operator precedence and associativity (short)
 *      4. binop        Binary operator precedence and associativity (short)
 */
void
pckg_toktbl_init()
{
    register UCHAR *tokptr;	/* Pointer to package token table */
    register long numoftoks;	/* Number of tokens in package token table */
    register int i;
    long  firsttokid;		/* Token ID of first token in package token
				 * table
				 */
    int   len;

#ifdef PCKGLOAD_DEBUG
    printf("\n\nLoading Package Token Table ");
    fflush(stdout);
#endif /* PCKGLOAD_DEBUG */

    numoftoks = (long) pckg_toktbl_size;
    firsttokid = *(long *) &pckg_toktbl;
    tokptr = ((UCHAR *) &pckg_toktbl) + sizeof (long);

#ifdef PCKGLOAD_DEBUG
    printf("\nNumber of Tokens: %d     First TokID: %d", numoftoks, firsttokid);
    fflush(stdout);
#endif /* PCKGLOAD_DEBUG */

    /*
     * if (firsttokid != ts_next) {
     */
    if (firsttokid != 1)
	fatal_error(FE_SYM_PCKG, 0);

    /* Increase the table size if it is neccesssary. */
    while ((firsttokid + numoftoks) > ts_cutoff)
	increase_table_size();

    /* Copy tokens from package token table to the token table */
    /*
     * for (i=ts_next; numoftoks > 0; numoftoks--, i++) {
     */
    for (i = 1; numoftoks > 0; numoftoks--, i++) {
	tokptr = (UCHAR *) (((unsigned long) tokptr + 3) & 0xfffffffc);
	toktable[i].unop = *(unsigned short *) tokptr;
	tokptr += sizeof (unsigned short);
	toktable[i].binop = *(unsigned short *) tokptr;
	tokptr += sizeof (unsigned short);

	/* Skip the token length and unreverse it when len > 255 */
	len = (int) *(UCHAR *) tokptr;
	if (len == 255) {
	    *tokptr = *(tokptr + 2);
	    len = (int) *(unsigned short *) tokptr;
	    tokptr += sizeof (short);
	    *tokptr++ = 255;
	}
	else
	    tokptr++;

	toktable[i].tkname = (UCHAR *) tokptr;
	tokptr += (len + 1);	/* skip token name */

#ifdef PCKGLOAD_DEBUG
#ifdef PCKGLOAD_PRINT_TOKENS
	printf("\nToken: %04d %02d %04x %04x %s ",
	       i, len, TOKUNOP(i), TOKBINOP(i), TOKNAME(i));
	fflush(stdout);
#endif /* PCKGLOAD_PRINT_TOKENS */
#endif /* PCKGLOAD_DEBUG */

	/* hash the entry */
	*lookup(toktable[i].tkname, 0) = toktable + i;
    }

    /* New ts_next */
    ts_next = i;

#ifdef PCKGLOAD_DEBUG
    printf("\nPackage Token Table Loaded \n ");
    fflush(stdout);
#endif /* PCKGLOAD_DEBUG */

}


int
save_toktbl()
{
    register int idx;
    long  firsttokid;		/* Token ID of first token in package token
				 * table
				 */
    int   len;

    /*
     * firsttokid = (TK_EOF+1);
     */
    firsttokid = 1;
    COFF_LONG_RAWDATA(firsttokid)
	for (idx = firsttokid; idx < ts_next; idx++) {
	COFF_ALIGN4
	    COFF_SHORT_RAWDATA(TOKUNOP(idx))
	    COFF_SHORT_RAWDATA(TOKBINOP(idx))
	    len = *(UCHAR *) (toktable[idx].tkname - 1);
	if (len != 255) {
	    COFF_BYTE_RAWDATA(len)
	}
	else {
	    /* reverse them */
	    COFF_BYTE_RAWDATA(255)
		COFF_BYTE_RAWDATA(*(UCHAR *) (toktable[idx].tkname - 2))
		COFF_BYTE_RAWDATA(*(UCHAR *) (toktable[idx].tkname - 3))
	}
	COFF_STRING_RAWDATA(TOKNAME(idx))
    }

    return ((int) (ts_next - firsttokid));
}

#endif /* PACKAGE */
