/*
 * vprintf.c            -- variable printf
 *      Copyright (c) 1985 by Kevin A. Buettner
 *      Copyright (c) 1986-1993 by Applied Logic Systems, Inc.
 *
 * Program Author: Kevin A. Buettner
 * Creation Date: 10/6/85
 * Revision History:
 *      Revised:  07/22/90      Kev     -- changed name from vprintf to
 *                                         PI_printf as the name conflicted
 *                                         with a unix section 3 function and
 *                                         there were also numerous problems
 *                                         with getting doubles working on 88k
 *                                         machines
 *      Revised:  07/12/93      Kev     -- changed PI_printf to PI_oprintf
 *                                         in order to accomodate new stream
 *                                         I/O.  Removed bufwrite support.
 *                                         This functionality is now performed
 *                                         via the new stream I/O package.
 */

#include "defs.h"
#ifdef HAVE_STDARG_H
#include <stdarg.h>
#else
#include <varargs.h>
#endif


/*
 * PI_oprintf uses the varargs package and vfprintf to print to
 * a file.
 */


/*VARARGS0 */
void
#ifdef HAVE_STDARG_H
PI_oprintf(char *fmt, ...)
#else
PI_oprintf(char *fmt, va_alist)
    va_dcl
#endif
{
    va_list args;

#ifdef HAVE_STDARG_H
    va_start(args, fmt);
#else
    va_start(args);
#endif

    vfprintf(outfd, fmt, args);
}


/*
 * PI_putchar is called to output a single character to either a file or
 * buffer.
 */

void
PI_oputchar(c)
    int   c;
{
    putc(c, outfd);
}
