/**********
 * dNcopy *
 **********/

/************************************************************
*                                                           *
* Copyright 1989 - 1991, Billy Shakespeare & Company, Inc.  *
* All rights reserved.                                      *
*                                                           *
*************************************************************
*                                                           *
* Published by                                              *
*        Copia International, Inc.                          *
*        Wheaton, Illinois                                  *
*        U. S. A.                                           *
*                                                           *
*************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#ifdef MSC
#include <sys\types.h>
#endif
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#include <sys\stat.h>
#endif

#ifndef unix 
#ifndef ACDLL
int DECLARE dNcopy(
 /* d_report arrangement: 3000 */
#else
int DECLARE DdNcopy( /* d_report arrangement: 3000 */
	DBGDAT_PTR dbgptr,
#endif
	/* inputs */
	CHAR_PTR src,    /* original NDX name */
	CHAR_PTR dest)
#else  /* unix */ 
int DECLARE dNcopy(
 	src, dest ) 
	CHAR_PTR src; 
	CHAR_PTR dest; 
#endif /* unix */ 
   /* new NDX name */

	/* output: none */
{
CHAR_PTR memory;
char fullname[ACTMPLEN];
int i, j;  /* general purpose integers */


if (!src || !dest || !ACstrcmp(src, dest))
{
    d_report = 3001;
    return(dILLEGAL);
}

/* open original file */
dUexpnm(src, "NDX", fullname);

memory = (CHAR_PTR) _ACalloc((unsigned long) NDXNODSZ);
if (!memory)
{
    d_report = 3002;
    return(dMEMERR);
}

i = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		fullname, d_READONLY);
if (i < 0)
{
    _ACfree(memory);
    return(i);
}

j = _ACread(i, memory, NDXNODSZ);

(void) _dcls(i);

if (j != NDXNODSZ)
{
    _ACfree(memory);
    d_report = 3003;
    return(dIOERR);
}

/* reset root & eof nodes */
_bytes4((long) 1, memory);
_bytes4((long) 2, memory + EOFNODE);

dUexpnm(dest, "NDX", fullname);

i = _dcre8new(fullname, FORCE);
if (i < 0)
{
    d_report = 3004;
    _ACfree(memory);
    return(dIOERR);    /* file creation by open() failed */
}
/* write the new header */
j = _ACwrite(i, memory, (unsigned) NDXNODSZ);
if (j == NDXNODSZ)
{
    /* finish the second half */
    ACmemset(memory, 0, NDXNODSZ);
    j = _ACwrite(i, memory, (unsigned) NDXNODSZ);
}

close(i);
_ACfree(memory);

if (j != (unsigned) NDXNODSZ)
{
    (void) _ACremove(fullname);
    d_report = 3005;
    return(dIOERR);
}

return(SUCCESS);
} /* end of dNcopy() */

