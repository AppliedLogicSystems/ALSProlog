/*===================================================================*
 |		vprintf.c
 |	Copyright (c) 1985 by Kevin A. Buettner
 |	Copyright (c) 1986-1995 Applied Logic Systems, Inc.
 |
 |		-- variable printf
 | Program Author: Kevin A. Buettner
 | Creation Date: 10/6/85
 | 07/22/90 - K.Buettner -- changed name from vprintf to PI_printf as 
 |						 the name conflicted with a unix section 3 function 
 |						 and there were also numerous problems with getting 
 |						 doubles working on 88k machines
 | 07/12/93 - K.Buettner -- changed PI_printf to PI_oprintf in order to 
 |						 accomodate new stream I/O.  Removed bufwrite support:
 |                       Functionality now performed via stream I/O package.
 *===================================================================*/

#include "defs.h"

/*
 * PI_oprintf uses the varargs package and vfprintf to print to
 * a file.
 */


/*VARARGS0 */
void
PI_oprintf(const char *fmt, ...)
{
    va_list args;

    va_start(args, fmt);

#ifdef KERNAL
    vfprintf(stdout, fmt, args);
#else
    vfprintf(outfd, fmt, args);
#endif /* KERNAL */
}


/*
 * PI_putchar is called to output a single character to either a file or
 * buffer.
 */
void
PI_oputchar(int c)
{
    putc(c, outfd);
}
