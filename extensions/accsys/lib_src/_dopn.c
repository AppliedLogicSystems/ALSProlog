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

#ifndef unix
#ifndef _WINDOWS
	i = (mode & d_READONLY) ? (O_RDONLY | O_BINARY) : (O_RDWR | O_BINARY);
#else
	i = (mode & d_READONLY) ? OF_READ : OF_READWRITE;
#endif
#else 	/* unix */
	i = (mode & d_READONLY) ? O_RDONLY : O_RDWR;
#endif 	/* unix */

#ifndef unix
#ifndef _WINDOWS
	j = d_DENYRW; /* Single-User version */
#else
    j = OF_SHARE_EXCLUSIVE;
#endif
#endif 	/* unix */

#ifdef MSCLONE
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
#endif

#ifdef TURBOC
	if (_osmajor >= 3)
    	i = _open(filename, i | j);
	else
    	i = _open(filename, i);
#endif

#ifdef __HIGHC__
    	i = _open(filename, i);
#endif 	/* __HIGHC__ */

#ifdef unix
    	i = open(filename, i);
#endif 	/* unix */

    if (i < 0)
    {
#ifdef MSCLONE
		if (ERRNO == ENOENT) /* file not found */
		{
#endif
#ifdef TURBOC
		if (ERRNO == ENOFILE) /* file not found */
		{
#endif
#ifdef __HIGHC__
		if (ERRNO == ENOENT) /* file not found */
		{
#endif
#ifdef unix
		if (ERRNO == ENOENT) /* file not found */
		{
#endif 	/* unix */

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
#ifdef MSCLONE
    return(close(fh));
#endif
#ifdef TURBOC
    return(_close(fh));
#endif
#ifdef __HIGHC__
    return(close(fh));
#endif 	/* __HIGHC__ */
#ifdef unix
    return(close(fh));
#endif 	/* unix */
} /* end of _dcls() */
