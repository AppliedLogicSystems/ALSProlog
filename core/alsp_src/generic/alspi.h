/*================================================================*
 |			alspi.h		
 |		Copyright (c) 1986-1995 Applied Logic Systems, Inc.
 |
 |			-- Prolog types & macros for the ALS-Prolog/C-Interface
 |
 | Author: Kevin A. Buettner
 | Creation: 11/7/86
 | 06/06/87 - K.Buettner -- Sun Interface
 | 06/08/88 - K.Buettner -- Changed names for consistancy
 | 06/13/88 - C.White -- Change filename to "alspi.h"
 | 11/09/88 - K.Buettner -- Motorola Interface
 *================================================================*/
#ifndef _ALSPI_H_INCLUDED_
#define _ALSPI_H_INCLUDED_

#include <stddef.h>
#include <stdarg.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Set up some macros for dealing with prototypes and other ANSI C features.
 */

#ifndef PARAMS
#if defined(__STDC__) || defined(__cplusplus)
#define CONST const
#define PARAMS(arglist) arglist
#else
#define CONST
#define PARAMS(arglist) ()
#endif
#endif /* PARAMS */

#ifndef PWordTypeDefined
typedef long PWord;
#define PWordTypeDefined 1
#endif

#define PI_VAR 		0		/* unbound variables */
#define PI_LIST 	1
#define PI_STRUCT 	2
#define PI_SYM 		3
#define PI_INT 		4
#define PI_UIA 		5
#define PI_DOUBLE 	6

typedef struct {
		char *name;
		int  arity;
		int (*func) PARAMS((void));
		char *funcname;
} PSTRUCT;

#define PI_BEGIN static PSTRUCT pi_init_array[] = {
#define PI_DEFINE(p,a,f) {p,a,f,((char *) -1)},
#define PI_MODULE(m) {m,-1,((int (*)PARAMS((void))) 0),((char *) -1)},
#define PI_END {((char *) -1),-1,((int (*)PARAMS((void))) 0),((char *) -1)} };

#define PI_PDEFINE(p,a,f,fn) {p,a,f,fn},

#define PI_INIT PrologInit(pi_init_array)

extern char *WinsTypeStr;

#define X_WIN_STR   "xwins"
#define MOTIF_WIN_STR "motif"
#define OL_WIN_STR    "openlook"
#define DEC_WIN_STR   "decwins"
#define NEXT_WIN_STR  "nextstep"
#define MS_WIN_STR   "mswins"
#define MAC_WIN_STR   "macwins"
#define WXWIN_WIN_STR "wxwins"
#define NO_WIN_STR    "nowins"

/*
 * Added 6/11/88 - chris
 */

#define PI_FAIL		return(0)
#define PI_SUCCEED	return(1)


/*
 * Informational, warning, and error messages
 *
 * PI_app_printf must be defined by the application.
 */

typedef enum {
    PI_app_printf_banner,		/* als startup banner */
    PI_app_printf_informational,	/* informational only */
    PI_app_printf_warning,		/* message is a warning */
    PI_app_printf_error,		/* message is an error */
    PI_app_printf_fatal_error		/* message is fatal error */
} PI_app_printf_flags;

/*
 * Declarations for foreign interface functions - 6/15/88 - chris
 */

extern	char *	PI_forceuia	PARAMS(( PWord *, int * ));
extern	void	PI_getan	PARAMS(( PWord *, int *, int ));
extern	void	PI_getargn	PARAMS(( PWord *, int *, PWord, int ));
extern	void	PI_gethead	PARAMS(( PWord *, int *, PWord ));
extern	void	PI_gettail	PARAMS(( PWord *, int *, PWord ));
extern	void	PI_getdouble	PARAMS(( double *, PWord ));
extern	void	PI_getstruct	PARAMS(( PWord *, int *, PWord ));
extern	char *	PI_getsymname	PARAMS(( char *, PWord, int ));
extern	char *	PI_getuianame	PARAMS(( char *, PWord, int ));
extern	void	PI_getuiasize	PARAMS(( PWord, int * ));
extern	void	PI_makedouble	PARAMS(( PWord *, int *, double ));
extern	void	PI_makelist	PARAMS(( PWord *, int * ));
extern	void	PI_makestruct	PARAMS(( PWord *, int *, PWord, int ));
extern	void	PI_makesym	PARAMS(( PWord *, int *, CONST char * ));
extern	void	PI_makeuia	PARAMS(( PWord *, int *, CONST char * ));
extern	void	PI_allocuia	PARAMS(( PWord *, int *, int ));
extern	int	PI_printf	PARAMS(( CONST char *, ... ));
extern	int	PI_aprintf	PARAMS(( CONST char *, CONST char *, ... ));
extern  int     PI_vprintf	PARAMS(( CONST char *, va_list ));
extern  int     PI_vaprintf	PARAMS(( CONST char *, CONST char *, va_list ));
extern	int	PI_rungoal	PARAMS(( PWord, PWord, int ));
extern	int	PI_rungoal_with_update	PARAMS(( PWord, PWord *, int * ));
extern	int	PI_unify	PARAMS(( PWord , int, PWord , int ));
extern	void	PrologInit	PARAMS(( PSTRUCT * ));
extern	void	PI_shutdown	PARAMS(( void ));
extern	int	PI_toplevel	PARAMS(( int * ));
extern	int	PI_prolog_init	PARAMS((int, char ** ));

#ifdef APP_PRINTF_CALLBACK
extern	void	PI_set_app_printf_callback(void (*callback)(int, va_list));
#endif
extern	void	PI_app_printf	PARAMS(( int, ... ));
extern  void    PI_vapp_printf  PARAMS(( int, va_list ));
extern	const char *	PI_get_options	PARAMS(( void ));

enum {CONSOLE_READ, CONSOLE_WRITE, CONSOLE_ERROR};

extern	void	PI_set_console_callback(int (*con_io)(int, char *, size_t));

#ifdef MacOS
extern	long	yield_interval;
extern  long	yield_counter;
extern	void	PI_yield_time	PARAMS(( void ));
#endif

#ifdef __cplusplus
}
#endif

/* Error codes */

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
#define FE_SYM_PCKG		26  /* symbol/package initialization error */
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
#define FE_FDREFOVERFLOW	72 /* Overflow/Underflow of file descriptors */

#endif /* _ALSPI_H_INCLUDED_ */
