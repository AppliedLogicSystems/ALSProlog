/*=====================================================================*
 |		pimain.c
 |	Copyright (c) 1988-1995, Applied Logic Systems, Inc.
 |
 |		-- default main() that initializes prolog and starts
 |			the development shell.
 |
 | 11/20/94 - C. Houpt -- Added Think/MetroWerks ccommand() call to allow
 |			      command line arguments for non-MPW versions.
 |	Note: Need mod of pi_init header file format to provide prototype.
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

#include "pi_cfg.h"

#ifdef MacOS
#if defined(THINK_C) || defined(__MWERKS__)
#include <console.h>
#endif
#endif
/* extern void	load_me(boid); */

void
main(argc, argv)
    int   argc;
    char **argv;
{
    int   exit_status;

#ifdef MacOS
#if  !defined(__MPW_MWERKS__) && !defined(applec)	
    argc = ccommand(&argv);
#ifdef __MWERKS__
    printf("Please use <Control-D> <Return> on a blank line to signal end-of-file.\n\n");
#endif /* __MWERKS__ */
#endif
#endif

#if 0
    /* DO NOT MOVE THIS CODE ANYWHERE ELSE!!!!!!!!!!!!!!!!!!!!! The Mac uses
     * alloca to allocate the heap and stack on the system (application)
     * stack.  If you put the call to alloca in another function, the space
     * that gets allocated by the  call to alloca will be "released" when
     * that function terminates.  SO...  LEAVE THIS CODE WHERE IT IS!!  Thank
     * you.  ( I dont see why prolog data areas cannot be malloced on Mac
     * just like other systems; that will eliminate this ifdef and also make
     * it suitable for embedded apps - raman 6/14/93 )
     */
    wm_stackbot = (PWord *) alloca((DEFAULT_STACK_SIZE + DEFAULT_HEAP_SIZE) * sizeof (PWord));
    if (wm_stackbot == 0)
	fatal_error(FE_BIGSTACK, 0);
    if (((int) (wm_stackbot + (DEFAULT_STACK_SIZE + DEFAULT_HEAP_SIZE)) & 0x80000000) != 0)
	fatal_error(FE_TAGERR, 0);
#endif
    
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
