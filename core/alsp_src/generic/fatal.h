/*=======================================================================*
 |		fatal.h
 |	Copyright (c) 1990-1995 Applied Logic Systems, Inc.
 |
 |		-- fatal error codes
 |
 | Creation: 12/17/90
 | Author: Kevin A. Buettner
 |
 | Description:
 |	Defines the fatal error codes for the system.  If part of the system
 |	must die in a non-graceful manner, it should call fatal_error with
 |	the error code provided and one string argument if necessary.
 |
 |	The error messages are in fatal.c and provide a convenient 
 |	mechanism for changing them if necessary.
 *=======================================================================*/

#ifndef _FATAL_H_INCLUDED_
#define _FATAL_H_INCLUDED_ 1

extern	void	fatal_error	PARAMS( (int, long) );

#define FE_SYMSPACE		0	/* Out of symbol space */
#define FE_STRSPACE		1	/* Out of string space */
#define FE_MAPSYM		2	/* Symbol mapping error */
#define FE_PATH			3	/* PATH variable inaccessible */
#define FE_INFND		4	/* Image not found */
#define FE_SMALLHEAP	5	/* Heap size too small */
#define FE_SMALLSTACK	6	/* Stack size too small */
#define FE_BIGHEAP		7	/* Heap size too big */
#define FE_BIGSTACK		8	/* Stack size too big */
#define FE_TAGERR		9	/* Invalid tagging on stack/heap */
#define FE_REMIDX		10	/* Error removing index block */
#define FE_DEVZERO		11	/* Error opening /dev/zero */
#define FE_STKOVERFLOW	12	/* Stack overflow */
#define FE_ICODEBUFINIT	13	/* Error allocating icode buffer */
#define FE_ICODEBUFOVER	14	/* Icode buffer overflow */
#define FE_XMEM_NTBL	15	/* Memory exhausted while allocating
	 			 * procedure entry
	 			 */
#define FE_FULL_NTBL	16	/* Procedure table full */
#define FE_XMEM_CLAUSE	17	/* Memory exhausted while allocating
				 			 * clause space
				 			 */
#define FE_GCMAGIC_CGC	18	/* Return address doesn't point at
				 			 * gc magic value (in wintcode.c)
				 			 */
#define FE_GCMAGIC		19	/* return address doesn't point at
				 			 * gc magic value (in gc.c)
				 			 */
#define FE_FULL_MODTBL	20	/* Module Table full */
#define FE_OVER_MODSTK	21	/* Module Stack overflow */
#define FE_OVER_CGSTK	22	/* Clause Group stack overflow */
#define FE_FULL_DEFUSES	23	/* Default use table full */
#define FE_FULL_DEFPROC	24	/* Default procs table full */
#define FE_FULL_MODUSE	25	/* Module use table full */
#define FE_SYM_PCKG		26	/* symbol/package initialization error */
#define FE_IN_COMP1		27	/* internal compiler error 1 */
#define	FE_OVER_HEAP	28	/* Heap/Choice point stack overflow */
#define FE_PANIC_FAIL	29	/* panic fail */
#define FE_PANIC_CONTINUE 30	/* panic continue */
#define FE_BAD_SWITCH	31	/* bad command line switch */
#define FE_BAD_NSWITCH	32	/* bad numeric switch */
#define FE_FDREFCNTS	33	/* error allocating fdrefcnts */
#define FE_DEMO_LIMIT	34	/* demonstration time limit exceeded */
#define FE_IN_BLTINIT	35	/* builtins init internal error */
#define FE_IN_INDEX1	36	/* indexing internal error */
#define FE_IN_PCKG1		37	/* internal packaging error */
#define FE_IN_PCKG2		38	/* internal packaging error */
#define FE_IN_PCKG3		39	/* internal packaging error */
#define FE_PCKG_RINFO	40	/* RInfo buffer full */
#define FE_PCKG_HTBL	41	/* Package hash table full */
#define FE_IN_PCKG4		42	/* internal packaging error */
#define FE_IN_PCKG5		43	/* internal packaging error */
#define FE_IN_PCKG6		44	/* internal packaging error */
#define FE_IN_PCKG7		45	/* internal packaging error */
#define FE_IN_PCKG8		46	/* internal packaging error */
#define FE_PCKG_NTBL	47	/* Procedure table full */
#define FE_IN_PCKG9		48	/* internal packaging error */
#define FE_IN_PCKG10	49	/* internal packaging error */
#define FE_IN_WGET1		50	/* internal w_get error */
#define FE_IN_WGET2		51	/* internal w_get error */
#define FE_IN_WINSTALL	52	/* internal w_install error */
#define FE_IN_HEAPCOPY	53	/* internal heap_copy error */
#define FE_IN_TERMCMP	54	/* internal error in termcmp */
#define FE_CAPTURE_INC  55      /* error in increasing capture size */
#define FE_ABOLISH_FAIL 56      /* abolish_predicate() failed */
#define FE_ASSERT_SSD   57      /* error asserting sys_searchdir */
#define FE_ASSERT_SD    58      /* error asserting searchdir */
#define FE_ASSERT_SYS   59      /* error asserting als_system */
#define FE_ASSERT_COM   60      /* error asserting app command line */
#define FE_GETCWD       61      /* error in getcwd */
#define FE_ALS_MEM_INIT	62	/* error in initial memory allocation */
#define FE_CODESPACE_INIT 63	/* error initializing code space */
#define FE_MODULE_INIT	64	/* error initializing module system */
#define FE_SYMTAB_INIT	65	/* error initializing symbol table */
#define FE_SS_MAXGLOBALS 66	/* AM_MAXGLOBALS constant not large enough */
#define FE_SS_INTEGRITY	67	/* Saved state file does not correspond to
				 			 * image
				 			 */
#define FE_SS_OPENERR	68	/* Cannot open saved state file */
#define FE_SS_FMALLOC	69	/* Problem with fmalloc */
#define FE_ALS_OPTIONS	70	/* Problem getting ALS_OPTIONS */
#define FE_AUTOLOAD	71	/* Problem autoloading file */
#define FE_FDREFOVERFLOW	72	/* Overflow/Underflow of file descriptors */

#endif /* _FATAL_H_INCLUDED_ */

