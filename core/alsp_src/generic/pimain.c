/*=====================================================================*
 |		pimain.c
 |	Copyright (c) 1988-1995, Applied Logic Systems, Inc.
 |
 |		-- default main() that initializes prolog and starts
 |			the development shell.
 |
 | 11/20/94 - C. Houpt -- Added Think/MetroWerks ccommand() call to allow
 |			      command line arguments for non-MPW versions.
 |			   -- Added include of pi_init header file to provide prototype.
 |			   -- Added PI_yield_time() to give other programs time.
 *=====================================================================*/

#ifdef HAVE_CONFIG_H
	/* In ALS-Prolog source tree */

#include "defs.h"

#else /* !HAVE_CONFIG_H */
	/* Not in ALS-Prolog source tree... */

#include "alspi.h"
#include <stdio.h>

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif

#ifndef NO_STDARG_H
#define HAVE_STDARG_H
#endif

#ifdef HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#endif /* !HAVE_CONFIG_H */

extern	void	main	PARAMS(( int, char ** ));

#include "pi_init.h"
#include "pi_cfg.h"

#ifdef MacOS
#ifdef HAVE_GUSI
#include <GUSI.h>
#endif

#ifdef MPW_TOOL
#include <CursorCtl.h>
#else
#if THINK_C
#include <console.h>
#endif			/* THINK_C */
#ifdef __MWERKS__
#include <console.h>
#include <SIOUX.h>

tSIOUXSettings  SIOUXSettings =
        {1, 1, 1, 0, 0, 0, 4, 80, 24, 0, 0, 22, 12, 0};

#endif			/* __MWERKS__ */
#endif			/* MPW_TOOL */

#include <Events.h>
#endif	/* MacOS */

void
main(int argc, char ** argv)
{
    int   exit_status;

#ifdef MacOS

#ifdef MPW_TOOL
    InitGraf((Ptr) &qd.thePort);
    InitCursorCtl(NULL);
#endif

#ifdef HAVE_GUSI
        GUSISetup(GUSIwithAppleTalkSockets);
        GUSISetup(GUSIwithInternetSockets);
        GUSISetup(GUSIwithPAPSockets);
        GUSISetup(GUSIwithPPCSockets);
        GUSISetup(GUSIwithUnixSockets);
#ifndef MPW_TOOL
	GUSISetup(GUSIwithSIOUXSockets);
#endif
#endif


#if (defined(THINK_C) || defined(__MWERKS__)) && !defined(MPW_TOOL)	
    argc = ccommand(&argv);
#endif

#if defined(__MWERKS__) && !defined(MPW_TOOL)
    printf("Please use Control-E (followed by a return) to signal end-of-file.\n");
    printf("Avoid Control-D, because this will terminate the application.\n\n");
#endif 


#endif /* MacOS */

    if ((exit_status = PI_prolog_init(WIN_STR, argc, argv)) != 0) {
	PI_app_printf(PI_app_printf_error, "Prolog init failed !\n");
	exit(1);
    }

#ifdef EXP_DATE
    if ((unsigned long) time(0) >= EXP_DATE) {
	PI_app_printf(PI_app_printf_error, "System validity date passed!\n");
	exit(1);
	}
#endif

    pi_init();

    if ((exit_status = PI_toplevel()) != 0) {
	PI_app_printf(PI_app_printf_error, "Prolog shell crashed !\n");
	exit(1);
    }

    PI_shutdown();
#if defined(__MWERKS__) && !defined(MPW_TOOL)
    printf("Exiting ALS Prolog.\n");
#endif
    exit(0);


}


/* PI_get_options returns a string containing option settings for ALSPro.
   Usually this string comes from getenv(), except on the Mac where it
   is stored in a preferences file.
*/
const char *PI_get_options(void)
{
#if defined(MacOS) && !defined(MPW_TOOL)
    /* This is a very simple version at the moment.  The preferences file
       must be in the same directory as the application.  This should be
       extended to also look in the preferences folder and system folder.
       The preferences file consists of a single line with the standard
       ALSPro options.
    */
    FILE *pref_file;
    static char pref_str[256];

    pref_file = fopen("ALSPro Prefs", "r");

    if (pref_file) {
	fgets(pref_str, 255, pref_file);
	fclose(pref_file);
	return pref_str;
    } else return NULL;
#else    
    return getenv("ALS_OPTIONS");
#endif
}

/*
 * PI_app_printf is called from the prolog environment to display error and
 * warning messages.  The first parameter, messtype, describes the type
 * of message.  These types are defined in alspi.h.  The message type may be
 * used to route the message supplied in va_alist to the place appropriate
 * for the application.
 *
 * Kev's note to ALS implementers:  
 *	We should be careful to only call PI_app_printf once for each
 *	particular message from Prolog.  Also, we should not make
 *	too many assumptions about what kind of device we are writing
 *	to.  In other words, line control information such as \n should
 *	probably be removed from most of our messages.  It will then
 *	be the responsiblity of PI_app_printf to output newlines or
 *	pop up windows or whatever.  It should also be the responsiblity
 *	of PI_app_printf to prepend information about the type of message.
 *	See the fatal error case (below) as an example.
 */


/*VARARGS0 */
void
#ifdef HAVE_STDARG_H
PI_app_printf(int messtype, ...)
#else
PI_app_printf(messtype,va_alist)
    int messtype;
    va_dcl
#endif
{
    va_list args;
    char *fmt;

#ifdef HAVE_STDARG_H
    va_start(args, messtype);
#else
    va_start(args);
#endif

    fmt = va_arg(args, char *);

    switch (messtype) {
	case PI_app_printf_banner :
	case PI_app_printf_informational :
	    vfprintf(stdout, fmt, args);
	    break;
	case PI_app_printf_fatal_error :
	    fprintf(stderr,"\nFatal Error: ");
	    vfprintf(stderr, fmt, args);
	    fprintf(stderr,"\n");
	    break;
	case PI_app_printf_warning :
	case PI_app_printf_error :
	default :
	    vfprintf(stderr, fmt, args);
	    break;
    }
}

#ifdef MacOS
/* PI_yield_time() is called periodically during the execution of prolog code to
   allow other processes to execute.  The MacOS uses cooperative multitasking, rather
   than preemptive.
   
   yield_interval is the number of prolog procedures executed between calls to
   PI_yield_time. yield_interval may be adjusted to provide more or fewer yields.
   
   yield_counter is used as a count-down until the next yield.
 */

long yield_interval = 100;
long yield_counter = 100;

static long last_yield = 0;

void	PI_yield_time(void)
{
    long tick;
    tick = TickCount();
#ifdef MPW_TOOL
    SpinCursor(1);
#endif
#if defined(__MWERKS__) && !defined(MPW_TOOL)
    SIOUXHandleOneEvent(NULL);
#endif
#ifdef THINK_C
    /* Do nothing for now. Think does not provide a handle event call for
       its console window. We could work around this, but it is not a priority.
    */
#endif

    /* Adjust the yield interval upwards until there are at least 3 ticks between
       yields.  */
    if (tick - last_yield <= 3) yield_interval += 100;
    last_yield = tick;
}

#if defined( __MWERKS__) && !defined(HAVE_GUSI)

#ifdef MPW_TOOL
#if __POWERPC__
#include <fcntl.h>
#else
#include <unix.h>
#endif
#else
#include <unix.h>
#endif

/* This a patch for metrowerk's open() function.  
   Hopefully this can be removed in a future release.
*/
int metrowerks_open_patch(const char *filename, int mode);
int metrowerks_open_patch(const char *filename, int mode)
{
#if defined(MPW_TOOL) && !__POWERPC__
  int mpw_mode;

/* MPW's open() mode values taken from {CIncludes}FCntl.h. */
#define MPW_O_RDONLY		 0 		/* Bits 0 and 1 are used internally */
#define MPW_O_WRONLY		 1 		/* Values 0..2 are historical */
#define MPW_O_RDWR 			 2		/* NOTE: it goes 0, 1, 2, *!* 8, 16, 32, ... */
#define MPW_O_APPEND	(1<< 3)		/* append (writes guaranteed at the end) */
#define MPW_O_RSRC 		(1<< 4)		/* Open the resource fork */
#define MPW_O_ALIAS		(1<< 5)		/* Open alias file */
#define MPW_O_CREAT		(1<< 8)		/* Open with file create */
#define MPW_O_TRUNC		(1<< 9)		/* Open with truncation */
#define MPW_O_EXCL 		(1<<10) 	/* w/ O_CREAT:  Exclusive "create-only" */
#define MPW_O_BINARY	(1<<11) 	/* Open as a binary stream */
#define MPW_O_NRESOLVE	(1<<14)		/* Don't resolve any aliases */

  switch(mode & 3) {
  case O_RDWR: mpw_mode = MPW_O_RDWR; break;
  case O_RDONLY: mpw_mode = MPW_O_RDONLY; break;
  case O_WRONLY: mpw_mode = MPW_O_WRONLY; break;
  };
  
  if (mode & O_APPEND) mpw_mode |= MPW_O_APPEND;
  if (mode & O_CREAT) mpw_mode |= MPW_O_CREAT;
  if (mode & O_EXCL) mpw_mode |= MPW_O_EXCL;
  if (mode & O_TRUNC) mpw_mode |= MPW_O_TRUNC;
  if (mode & O_BINARY) mpw_mode |= MPW_O_BINARY;
	
  mode = mpw_mode;
#endif

  return open(filename, mode);
}

#endif
#endif

