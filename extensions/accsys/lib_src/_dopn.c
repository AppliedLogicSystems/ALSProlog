/**********
* _dopn.c *
***********/

/*****************************************************
*                                                    *
* Copyright 1989, Billy Shakespeare & Company, Inc.  *
* All rights reserved.                               *
*                                                    *
******************************************************
*                                                    *
* Published by                                       *
*        Copia International, Inc.                   *
*        Wheaton, Illinois                           *
*        U. S. A.                                    *
*                                                    *
******************************************************/
 
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"                          

#ifdef STANDARD
#include <io.h>
#else
extern char _osmajor;
#endif

#ifdef MSC
#define MSCLONE
#endif
#ifdef ZORTECH
#define MSCLONE
#include <dos.h>
#endif

#ifndef _WINDOWS
#define ERRNO errno
#else
#define ERRNO wrc.nErrCode
#endif


int _DECLARE _dopn(filename,mode)
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
		/*** d_report assignment: 10420 ***/
		/* inputs */
    CHAR_PTR filename; /* filename */
	int  mode;         /* file open mode */
{
	int i, j;  /* general purpose integers */
#ifdef _WINDOWS
	OFSTRUCT wrc;
#endif

#if defined(_WINDOWS)
	i = (mode & d_READONLY) ? OF_READ : OF_READWRITE;
#elif defined(unix)
	i = (mode & d_READONLY) ? O_RDONLY : O_RDWR;
#else
	i = (mode & d_READONLY) ? (O_RDONLY | O_BINARY) : (O_RDWR | O_BINARY);
#endif

#if defined(_WINDOWS)
    j = OF_SHARE_EXCLUSIVE;
#elif defined(unix)
#elif defined(MacOS)
#else
	j = d_DENYRW; /* Single-User version */
#endif

#if defined(MSCLONE)
#ifdef _WINDOWS
	i = OpenFile((LPSTR) filename,  (LPOFSTRUCT) &wrc, i | j);
#else
	if (_osmajor >= 3)
	{
    	i = sopen(filename, i, j);
	}
	else
	{
    	i = open(filename, i);
	}
#endif

#elif defined(TURBOC)
	if (_osmajor >= 3)
    	i = _open(filename, i | j);
	else
    	i = _open(filename, i);

#elif defined(__HIGHC__)
    	i = _open(filename, i);

#elif defined(unix)
    	i = open(filename, i);

#elif defined(MacOS)
    	i = open(filename, i);
		
#else
#error
#endif

    if (i < 0)
    {
#if defined(MSCLONE)
		if (ERRNO == ENOENT) /* file not found */
#elif defined(TURBOC)
		if (ERRNO == ENOFILE) /* file not found */
#elif defined(__HIGHC__)
		if (ERRNO == ENOENT) /* file not found */
#elif defined(unix)
		if (ERRNO == ENOENT) /* file not found */
#elif defined(MacOS)
		if (ERRNO == ENOENT) /* file not found */
#else
	#error
#endif
		{
	    	d_report = 10421;
	    	return(dNOTFOUND);
		}
		d_report = 10422;
		return(dIOERR);
    }

    return(i);

} /* end of _dopn() */


int _DECLARE _dcls(fh)
	int fh;
{
#if defined(MSCLONE)
    return(close(fh));
#elif defined(TURBOC)
    return(_close(fh));
#elif defined(__HIGHC__)
    return(close(fh));
#elif defined(unix)
    return(close(fh));
#elif defined(MacOS)
    return(close(fh));
#else
	#error
#endif
} /* end of _dcls() */
