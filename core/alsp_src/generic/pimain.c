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

#if THINK_C
#include <console.h>
#endif			/* THINK_C */
#if __MWERKS__
#include <console.h>
#include <SIOUX.h>
#endif			/* __MWERKS__ */
#ifdef MPW_TOOL
#include <CursorCtl.h>
#endif			/* MPW_TOOL */

#include <Events.h>
#endif	/* MacOS */

void
main(argc, argv)
    int   argc;
    char **argv;
{
    int   exit_status;

#ifdef MacOS

#if (defined(THINK_C) || defined(__MWERKS__)) && !defined(MPW_TOOL)	
    argc = ccommand(&argv);
#endif

#if defined(__MWERKS__) && !defined(MPW_TOOL)
    printf("Please use # on a blank line to signal end-of-file.\n");
    printf("Avoid Control-D, because this will terminate the application.\n\n");
#endif 

#ifdef MPW_TOOL
    InitCursorCtl(NULL);
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
    exit(0);


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
    SpinCursor(32);
#endif
#ifdef __MWERKS__
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
#endif
