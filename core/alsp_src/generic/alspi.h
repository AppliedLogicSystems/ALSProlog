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

#include <limits.h>
#include <stddef.h>
#include <stdarg.h>

#ifdef WIN32
#include <windows.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

extern char library_dir[PATH_MAX];
extern char executable_path[PATH_MAX];


/*
 * Set up some macros for dealing with prototypes and other ANSI C features.
 */

#if defined(macintosh)
#define ALSPI_API(X)		pascal X
#define ALSPI_APIP(X,Y)		pascal X (*Y)
#elif defined(WIN32)
#define ALSPI_API(X)		X __stdcall
#define ALSPI_APIP(X,Y)		X (__stdcall *Y)
#else
#define ALSPI_API(X)    	X
#define ALSPI_APIP(X,Y)		X (*Y)
#endif

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
		const char *name;
		int  arity;
		int (*func) (void);
		const char *funcname;
} PSTRUCT;

#define PI_BEGIN static PSTRUCT pi_init_array[] = {
#define PI_DEFINE(p,a,f) {p,a,f,((char *) -1)},
#define PI_MODULE(m) {m,-1,((int (*)(void)) 0),((char *) -1)},
#define PI_END {((char *) -1),-1,((int (*)(void)) 0),((char *) -1)} };

#define PI_PDEFINE(p,a,f,fn) {p,a,f,fn},

#define PI_INIT PrologInit(pi_init_array)

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

/* PI_system_setup is a structure containing initilization information
   for the ALS Prolog engine.
   
   Fields set to 0 or NULL will cause the engine to use its default
   value.
 */

typedef struct {
    unsigned long heap_size;   /* Prolog heap size in K long words */
    unsigned long stack_size;  /* Prolog stack size in K long words */
    unsigned long icbuf_size;  /* Prolog compiler buffer size in K code words */
    const char *alsdir;        /* Path to the ALS directory */
    const char *saved_state;   /* Path to saved state file */
    int load_executable_state; /* Boolean for loading from executable */

    /* OS Specific setup information */
    
    /* ANSI C main() command line argument count and vector */
    int argc;
    char **argv;
    
#ifdef WIN32
    /* Win32 WinMain() arguments */
    HINSTANCE hInstance;
    HINSTANCE hPrevInstance;
    LPSTR lpCmdLine;
    int nCmdShow;
#endif
} PI_system_setup;

/*
 * Declarations for foreign interface functions - 6/15/88 - chris
 */

extern	ALSPI_API(char *)	PI_forceuia	( PWord *, int * );
extern	ALSPI_API(void)	PI_getan	( PWord *, int *, int );
extern	ALSPI_API(void)	PI_getargn	( PWord *, int *, PWord, int );
extern	ALSPI_API(void)	PI_gethead	( PWord *, int *, PWord );
extern	ALSPI_API(void)	PI_gettail	( PWord *, int *, PWord );
extern	ALSPI_API(void)	PI_getdouble	( double *, PWord );
extern	ALSPI_API(void)	PI_getstruct	( PWord *, int *, PWord );
extern	ALSPI_API(char *)	PI_getsymname	( char *, PWord, int );
extern	ALSPI_API(char *)	PI_getuianame	( char *, PWord, int );
extern	ALSPI_API(void)	PI_getuiasize	( PWord, int * );
extern	ALSPI_API(void)	PI_makedouble	( PWord *, int *, double );
extern	ALSPI_API(void)	PI_makelist	( PWord *, int * );
extern	ALSPI_API(void)	PI_makestruct	( PWord *, int *, PWord, int );
extern	ALSPI_API(void)	PI_makesym	( PWord *, int *, const char * );
extern	ALSPI_API(void)	PI_makeuia	( PWord *, int *, const char * );
extern	ALSPI_API(void)	PI_allocuia	( PWord *, int *, int );
extern	ALSPI_API(int)	PI_printf	( const char *, ... );
extern	ALSPI_API(int)	PI_aprintf	( const char *, const char *, ... );
extern  ALSPI_API(int)	PI_vprintf	( const char *, va_list );
extern  ALSPI_API(int)	PI_vaprintf	( const char *, const char *, va_list );
extern	ALSPI_API(int)	PI_rungoal	( PWord, PWord, int );
extern	ALSPI_API(int)	PI_rungoal_with_update	( PWord, PWord *, int * );
extern	ALSPI_API(int)	PI_rungoal_with_update_and_catch	( PWord, PWord *, int *, int * );
extern	ALSPI_API(int)	PI_unify	( PWord , int, PWord , int );
extern	ALSPI_API(void)	PrologInit	( PSTRUCT * );
extern	ALSPI_API(void)	PI_shutdown	( void );
extern	ALSPI_API(void)	PI_toplevel	( void );
extern	ALSPI_API(int)	PI_status_toplevel	( int * );
extern	ALSPI_API(int)	PI_prolog_init	( int, char **);
extern	ALSPI_API(int)	PI_startup	( const PI_system_setup *);
extern	ALSPI_API(void)	PI_throw	(PWord obj, int objt);
extern	ALSPI_API(void)	PI_getball	(PWord *obj, int *objt);
extern	ALSPI_API(int)	PI_main		(int argc, char *argv[], void (*init)(void));
extern	ALSPI_API(void)	PI_interrupt	(void);


#ifdef APP_PRINTF_CALLBACK
extern	ALSPI_API(void)	PI_set_app_printf_callback(void (*callback)(int, va_list));
#endif
extern	ALSPI_API(void)	PI_app_printf	( int, ... );
extern  ALSPI_API(void)    PI_vapp_printf  ( int, va_list );
extern	ALSPI_API(const char *)	PI_get_options	( void );

typedef long (*console_func)(char *, long);

extern ALSPI_API(void)	PI_set_console_functions(console_func readf,
								console_func writef, console_func error);

#ifdef macintosh
extern	long	yield_interval;
extern  long	yield_counter;
extern	void	PI_yield_time	( void );
extern	ALSPI_API(void)	PI_set_yield_proc(void (*p)(void));
#endif

#ifdef macintosh
Boolean SIOUXIsAppWindow(WindowPtr w);
short SIOUXHandleOneEvent(EventRecord *event);
void SIOUXSetEventVector(short (*handler)(EventRecord *));
QDGlobals *GetQD(void);

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

/* Defined EXIT_ERROR for reporting invalid options, etc. */
#define EXIT_ERROR 2


/*=====================================================================*
 |		 cinterf.h
 |		Copyright (c) 1992-95, Applied Logic Systems Inc.
 |
 |			-- defines for support routines for C interface
 |
 | Author : Prabhakaran Raman
 | Creation : 2/1/92
 *=====================================================================*/
 
/*---------------------------------------------------------------------
 * Information on the fields of a C-structure/C-union is kept in
 * an array (one per structure/union) that has the following layout.
 * (The array is initialized with the structure information by the
 * interface generator ).
 *--------------------------------------------------------------------*/

typedef struct {
  char *fname;
  unsigned int foffset;
  short  ftype;    /* integer identifying C type; 0 for struct/union */
  char *type_name;  /* when ftype is 0, typename is set to struct name */
  int  arraysz;    /* N=0 => not an array, N>0 => array of size N */
} FieldEntry;      /* For array, currently, this is set to 1000   */
                   /* Later, when ctrans is modified to recognize */
                   /* array size, this field with be set correctly*/
                   /* What about multi-dimensional arrays ?       */

#define CI_BEGARRAY(name) static FieldEntry name[]= {

#define CI_FIELD(fieldstr,field,structname,typeid,typename) \
  { (fieldstr), ((unsigned int) &(((structname *)0)->field)), \
	  (typeid), (typename), 0 }

#define CI_ARRAYFIELD(fieldstr,field,structname,typeid,typename,size) \
  { (fieldstr), ((unsigned int) &(((structname *)0)->field[0])), \
	  (typeid), (typename), (size) }

#define CI_ENDARRAY  };

/*-----------------------------------------------------------------------*
 * The C interface maintains its own symbol table for
 * storing C-constants, C-type and C-structure information.
 * The following tags are used to identify the nature of
 * entries in the symbol table
 *-----------------------------------------------------------------------*/

#define CI_LONGTYPE		0
#define CI_PTRTYPE		1
#define CI_INTTYPE		2
#define CI_STRINGTYPE	3
#define CI_SHORTTYPE	4
#define CI_CHARTYPE		5
#define CI_FLOATTYPE	6
#define CI_DOUBLETYPE	7
#define CI_RCONSTTYPE	8
#define CI_STRUCTTYPE	9
#define CI_CTYPE		10

/*-----------------------------------------------------------------------* 
 * The following macros are used to load up the symbol table at
 * interface initialization time with info on C-constants,
 * C-types, and C-structures (the initialization routine as
 * well as each of the macro invocation code are output by
 * interface generator ).
 *-----------------------------------------------------------------------*/

extern	ALSPI_API(int)	sym_insert_2long ( char *, int, long, long );
extern	ALSPI_API(int)	sym_insert_dbl	( char *, int, double );
extern	ALSPI_API(int)	CI_get_integer	( PWord *, int );
extern	ALSPI_API(int)	CI_get_double	( double *, unsigned long, unsigned long );

extern	ALSPI_API(const char *) find_callback(void *func, void *object);

#define CI_INTCONST(name,val) 	\
	sym_insert_2long(name,CI_INTTYPE,(long)val,0);

#define CI_SHORTCONST(name,val) 	\
	sym_insert_2long(name,CI_SHORTTYPE,(long)val,0);

#define CI_CHARCONST(name,val) 	\
	sym_insert_2long(name,CI_CHARTYPE,(long)val,0);

#define CI_LONGCONST(name,val) 	\
	sym_insert_2long(name,CI_LONGTYPE,(long)val,0);

#define CI_PTRCONST(name,val) 	\
	sym_insert_2long(name,CI_PTRTYPE,(long)val,0);

#define CI_STRINGCONST(name,val) 	\
	sym_insert_2long(name,CI_STRINGTYPE,(long)val,0);

#define CI_FLOATCONST(name,val) 	\
	sym_insert_dbl(name,CI_FLOATTYPE,(double)val);

#define CI_DOUBLECONST(name,val) 	\
	sym_insert_dbl(name,CI_DOUBLETYPE,(double)(long)val);

#define CI_STRUCT(name,cname,defnptr)	\
  sym_insert_2long(name,CI_STRUCTTYPE,sizeof(cname),(long)defnptr);

#define CI_CTYPEDEF(name,ctype,typeid) \
  sym_insert_2long(name,CI_CTYPE,sizeof(ctype),typeid);

#define CI_RCONST(name,val)		\
	sym_insert_2long(name,CI_RCONSTTYPE,(long)val,0);

void pi_init(void);

#ifdef __cplusplus
}
#endif


#endif /* _ALSPI_H_INCLUDED_ */
