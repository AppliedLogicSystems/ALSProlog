/********************************
*          _asysio2.c		*
*********************************/

/************************************************************
*                                                           *
* Copyright 1990, 1991, Billy Shakespeare & Company, Inc.   *
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

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#ifdef _WINDOWS
#ifdef NULL
#undef NULL
#endif
#include <windows.h>
#endif

#include "db4.h"
#include "d4def.h"

#ifdef STANDARD
#include <io.h>
#endif

#if defined(MSC)
#define DIFFDRIVE EXDEV

#elif defined(TURBOC)
#define DIFFDRIVE ENOTSAM

#elif defined(ZORTECH)
#define DIFFDRIVE 17

#elif defined(LATTICE)
#define DIFFDRIVE EXDEV

#elif defined(__HIGHC__)
#define DIFFDRIVE EXDEV

#elif defined(unix)
#define DIFFDRIVE EXDEV

#elif defined(MacOS)
#define DIFFDRIVE EXDEV

#else
#error
#endif /* MSC, TURBOC, ZORTECH, LATTICE, __HIGHC__, unix, MacOS */


#define BLOCKSZ 0x1000
#define SMALLSZ 0x0100

#ifdef _WINDOWS
char from[ACTMPLEN], to[ACTMPLEN];
#endif

int _DECLARE _Arename(
#ifdef ACDLL
	dbgptr,
#endif
	oldname, newname )
#ifdef ACDLL
	DBGDAT_PTR dbgptr;
#endif
	CHAR_PTR oldname; /* original filename */
	CHAR_PTR newname;
{
	int rc, len, fd1, fd2;
	CHAR_PTR memory;
	unsigned int blksiz;

#ifndef _WINDOWS
	rc = rename(oldname, newname);
#else
	ACstrcpy(from, oldname);
	ACstrcpy(to, newname);
	rc = rename(from, to);
#endif

	if (!rc) return(0); /* success */

	if (errno != DIFFDRIVE) return(rc); /* i/o error */

	/* couldn't rename across different drives */

	blksiz = BLOCKSZ;
	while (1)
	{
    	memory = (CHAR_PTR) _ACalloc((unsigned long) blksiz);
    	if (memory) break;  /* got it! */
    	if (blksiz <= SMALLSZ) return(dMEMERR); /* no hope */
    	blksiz >>= 1;
	}

	fd1 = _dopn(
#ifdef ACDLL
			dbgptr,
#endif
			(CHAR_PTR) oldname, d_SINGLE);

	if (fd1 < 0)
	{
#if defined(MSC)
		if (errno == ENOENT) /* file not found */
#elif defined(ZORTECH)
		if (errno == ENOENT) /* file not found */
#elif defined(TURBOC)
		if (errno == ENOFILE) /* file not found */
#elif defined(LATTICE)
		if (errno == ENOENT) /* file not found */
#elif defined(__HIGHC__)
		if (errno == ENOENT) /* file not found */
#elif defined(unix)
		if (errno == ENOENT) /* file not found */
#elif defined(MacOS)
		if (errno == ENOENT) /* file not found */
#else
	#error
#endif	/* MSC, ZORTECH, TURBOC, LATTICE, __HIGHC__, unix, MacOS */
		{
	    	rc = dNOTFOUND;
		}
		else
		{
	    	rc = dIOERR;
		}
		_ACfree(memory);
        return(rc);  /* open failure */
	}

	fd2 = _dcre8new(newname, ~FORCE);
	if (fd2 < 0)
	{
    	(void) close(fd1);
    	_ACfree(memory);
    	return(fd2);
	}

	while (2)
	{
    	len = _ACread(fd1, memory, blksiz);
    	if (len <= 0)
    	{
        	rc = 0;
			break; /* done */
    	}
    	rc = _ACwrite(fd2, memory, len);
    	if (rc != len)
    	{
			rc = dIOERR;
			break;
    	}
	}
	(void) close(fd1);
	(void) close(fd2);
	_ACfree(memory);
	if (!rc) (void) _ACremove(oldname);
	return(rc);
} /* end of _Arename() */


#ifdef _WINDOWS
int _DECLARE _ACremove(CHAR_PTR filename)
{
	OFSTRUCT dummy;

   	(void) OpenFile((LPSTR) filename, (LPOFSTRUCT) &dummy,
			OF_DELETE);
	return(SUCCESS);
} /* end of _ACremove */
#endif

