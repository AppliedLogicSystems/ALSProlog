/*
 * defs.h	-- Common definitions and configuration parameters
 *
 *	Copyright (c) 1990-1993 by Applied Logic Systems, Inc.
 *
 * Creation: 9/27/90 (originally config.h)
 * Author: Keith Hughes and Kevin Buettner
 *
 * This file contains common configuration parameters.  Certain assumptions
 * are made in this file about the architecture and machine on which
 * Prolog will be built for.  aconfig.h and mconfig, the architecture and
 * machine dependent configuration files can override these assumptions if
 * they are wrong.  Certain other parameters will simply not be defined here
 * because they will be different for every platform.  In this case, these
 * parameters will be defined in either aconfig.h or mconfig.h.
 * 
 */

#ifndef _DEFS_H_INCLUDED_
#define _DEFS_H_INCLUDED_ 1

/*
 * Operating System:
 *
 * Assume that some variant of unix is the operating system.  Override
 * in mconfig.h or aconfig.h if this is not the case.
 *
 * We now longer distiguish between variants of unix.  We test for specific
 * features in the configuration script instead.
 */

#define UNIX     1
#define OSStr "unix"

/*
 * SlowCut needs to be defined at the present time in order for our interrupt
 * mechanism to work properly.  It is unfortunate that this is the case.
 * SlowCut indicates that code for performing cut is not expanded inline;
 * that is, it is a goal like any other goal.  There are some ways to
 * properly get cut inlined and also allow interrupts to work which might
 * be explored in the future.
 */

#define SlowCut 1


/*
 * NewMath indicates that goals performing arithmetic are totally expanded.
 * InMath and FMath must also be defined if NewMath is defined.
 * Define NewMath if math code totally expanded; InMath and FMath must also
 * be defined if NewMath is defined.
 */

#define NewMath 1
#define InMath 1	/* Inline math */
#define FMath 1		/* Inline floating math */


/* 
 * It may be useful to turn off the following configuration parameters when
 * porting to a new platform.  Otherwise, they should be left alone.
 *
 * CodeGC	-- code space garbage collection
 * AutoIndexing	-- stable procedures generate indexing for themselves
 * SPY		-- support for spy points
 * OBP		-- support for .obp files
 * Indexing	-- generate first argument indexing
 * BigStruct	-- structures with arities larger than ESCAPE_ARITY are 
 *		   supported.  ESCAPE_ARITY is 255 on most platforms (see
 *		   mtypes.h).  Assembly versions of functor, arg, mangle,
 *		   and the unifier need to be modified in order to support
 *		   big structures.  The garbage collector and other C code
 *		   should use ifdef BigStruct to handle big structures.
 */

#define CodeGC		1
#define AutoIndexing	1
#define SPY		1
#define OBP		1
#define Indexing	1
#define BigStruct	1



/*
 * Other Parameters which are not defined by default; these should be defined
 * in either mconfig.h or aconfig.h if needed.
 *
 * DoubleType	-- floating point types are implemented in a direct fashion
 *		(similar to UIA's).  They are more compact and more easily
 *		decoded.  If DoubleType is not defined, floats and doubles
 *		are represented as '$double'(D1,D2,D3,D4) where D1-D4 are
 *		sixteen bit pieces of the double.
 *
 * MotorolaMath	-- defined if the Motorola math builtins implemented by
 *		Sam Daniel's group are supported.
 *
 * PACKAGE	-- defined if the application packaging system is to be
 *		included
 *
 * CMeta	-- certain meta-builtins are written in C
 *
 * SIO_ASM 	-- certain of the stream I/O primitives are implemented
 *		in assembly language
 *
 */

 
 
/*
 * Include the architecture and machine specific configuration files.  The
 * machine specific file is included after the architecture specific file so
 * that it may override certain architecture defined parameters in much the
 * same manner in which both of these files may override the paramters defined
 * in this file before this point.
 */

#include "aconfig.h"
#include "config.h"

#if defined(HAVE_VM_ALLOCATE) && defined(HAVE_VM_PROTECT)
#define MACH_SUBSTRATE 1
#undef HAVE_MMAP
#endif


#if HAVE_LIBC_H		/* NeXT has this */
#include <libc.h>	/* Get prototypes for C library */
#endif

#if	defined(HAVE_STDLIB_H)
#include <stdlib.h>
#endif	/* HAVE_STDLIB_H */

#include <stdio.h>

#if defined(HAVE_STDARG_H)
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#if defined(HAVE_STDDEF_H)
#include <stddef.h>
#endif

#if STDC_HEADERS || HAVE_STRING_H
#include <string.h>
  /* An ANSI string.h and pre-ANSI memory.h might conflict.  */
#if !STDC_HEADERS && HAVE_MEMORY_H
#include <memory.h>
#endif /* not STDC_HEADERS and HAVE_MEMORY_H */
#else /* not STDC_HEADERS and not HAVE_STRING_H */
#if HAVE_STRINGS_H
#include <strings.h>
  /* memory.h and strings.h conflict on some systems */
#endif
#endif /* not STDC_HEADERS and not HAVE_STRING_H */


/*
 * Set up some macros for dealing with prototypes and other ANSI C features.
 */

#ifndef PARAMS
#if defined(__STDC__)
#define CONST const
#define PARAMS(arglist) arglist
#else
#define CONST
#define PARAMS(arglist) ()
#endif
#endif /* PARAMS */

#ifdef UNIX
/* include some standard files */
#include <sys/types.h>
#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif
#ifdef HAVE_SYS_TIMES_H
#include <sys/times.h>
#endif
#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <signal.h>

/*
 * Don't use sigaction if implementation of sigaction is immature.
 */
#if defined(HAVE_SIGACTION) && !defined(SA_SIGINFO)
#undef HAVE_SIGACTION
#endif

#include "missing.h"		/* extern decls missing from header files */
#endif


/*
 * Define the system dependent directory and path separators for parsing path
 * lists
 *
 *			UNIX	VMS	DOS	Atari	Mac
 * Path Separator:	:	,	;	,	,
 * Dir Separator:	/	]	\	\	:
 */

#if	defined(UNIX)
#define PATH_SEPARATOR	':'
#define DIR_SEPARATOR	'/'

#elif	defined(VMS)
#define PATH_SEPERATOR	','
#define DIR_SEPARATOR	']'

#elif	defined(DOS)
#define	PATH_SEPARATOR	';'
#define DIR_SEPARATOR	'\\'

#elif	defined(AtariOS)
#define PATH_SEPARATOR	','
#define DIR_SEPARATOR	'\\'

#elif	defined(MacOS)
#define PATH_SEPARATOR	','
#define DIR_SEPARATOR	':'

#endif


#if defined(HAVE_LIBDL) || defined(HAVE_LIBLD)
/*
 * DynamicForeign indicates that dynamic loading of foreign code is
 * supported.
 */

#define DynamicForeign 1
#endif



/*
 * Stuff from memory.h:
 *
 * Some systems still use bcopy so define memmove in terms of bcopy if
 * we have to.
 */

#if !defined(HAVE_MEMMOVE) && defined(HAVE_BCOPY)
#define memmove(s1,s2,n) bcopy(s2,s1,n)
#endif


/*
 * Include commonly needed Generic include files
 */

#include "mtypes.h"	/* not generic, but every platform has one */
#include "types.h"
#include "alloc.h"
#include "fileio.h"	/* candidate for eventual elimination */
#include "parser.h"
#include "tokens.h"
#include "winter.h"
#include "fatal.h"
#include "alspi.h"
#include "chpt.h"
#include "built.h"


/* If string.h doesn't exist or is lacking certain functions, we provide our
 * own replacements for the functions declared therein...
 */
#ifndef HAVE_STRTOK
extern char *strtok PARAMS(( char *s1, const char *s2 ));
#endif
#ifndef HAVE_STRDUP
extern char *strdup PARAMS(( const char *s1 ));
#endif
#ifndef HAVE_STRSPN
extern size_t strspn PARAMS(( const char *s1, const char *s2 ));
#endif
#ifndef HAVE_STRCSPN
extern size_t strcspn PARAMS(( const char *s1, const char *s2 ));
#endif


/*
 * Declare the als memory allocation function and associated helpers
 */

extern	int	als_mem_init	PARAMS(( char *file, long offset ));
extern	long *	ss_pmalloc	PARAMS(( size_t size, int fe_num, long *asizep ));
extern	long *	ss_malloc	PARAMS(( size_t size, int fe_num ));
extern	void	ss_register_global PARAMS(( long *addr ));
extern	int	ss_save_state	PARAMS(( char * ));
extern	void	protect_bottom_stack_page PARAMS(( void ));
extern	long *	ss_fmalloc_start PARAMS(( void ));
extern	long *	ss_fmalloc	PARAMS(( size_t ));

/*
 * Declare prototypes of other functions which have no obvious header file.
 */

/* main.c */
extern	void	als_exit	PARAMS(( int ));
extern	void	heap_overflow	PARAMS(( void ));

/* disassem.c */
extern	void	list_asm	PARAMS(( Code *, int ));

/* loadfile.c */
extern	void	fix_MAGIC	PARAMS(( void ));
extern	void	f_icode		PARAMS(( int, long, long, long, long ));
extern	int	obp_open	PARAMS(( char * ));
extern	void	obp_close	PARAMS(( void ));
extern	int	f_load		PARAMS(( char * ));
extern	int	load_file	PARAMS(( char *, int ));
extern	void	obp_push	PARAMS(( void ));
extern	void	obp_pop		PARAMS(( void ));

/* sig.c */
extern	void	deathwatch	PARAMS(( void ));
extern	void	reissue_cntrlc	PARAMS(( void ));

/* vprintf.c */
extern	void	PI_oprintf	PARAMS(( char *, ... ));
extern	void	PI_oputchar	PARAMS(( int ));

/* fsdos.c, fsmac.c, fsunix.c, or fsvms.c */
extern	void	init_fsutils	PARAMS(( void ));

/* sig.c */
extern	void	init_sigint	PARAMS(( void ));
extern	void	reset_sigint	PARAMS(( void ));

/* cinterf.c */
extern	void	cinterf_init	PARAMS(( void ));

/* from either assembly code or wam.c */
extern	void	wm_exec		PARAMS(( Code * ));
extern	int	wm_rungoal	PARAMS(( PWord, PWord ));

/* foreign.c */
extern	int	load_foreign	PARAMS(( char *, char *, char * ));

#ifdef DynamicForeign
/* lforeign.c */
extern	void (*	load_object	PARAMS(( char *, char *, char * )) ) PARAMS((void));
extern	void	foreign_shutdown PARAMS(( void ));
#endif /* DynamicForeign */

#ifdef Portable
extern	void	wam_init	PARAMS(( void ));
#endif /* Portable */

/* icode1.c */
extern	int	init_icode_buf	PARAMS(( int ));


#endif /* _DEFS_H_INCLUDED_ */
