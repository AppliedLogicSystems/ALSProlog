/************
 * dUtmpdir *
 ************/

/*****************************************************
*                                                    *
* Copyright 1991, Billy Shakespeare & Company, Inc.  *
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

#include <string.h>
#include <stdio.h>
#include "db4.h"
#include "d4def.h"

#ifndef ACDLL
int DECLARE dUtmpdir(
#ifndef unix 
CHAR_PTR tempdir)
#else  /* unix */ 
	tempdir ) 
	CHAR_PTR tempdir; 
#endif /* unix */ 

#else
int DECLARE DdUtmpdir(
#ifndef unix 
DBGDAT_PTR dbgptr, CHAR_PTR tempdir)
#else  /* unix */ 
	
 	dbgptr, tempdir ) 
	DBGDAT_PTR dbgptr; 
	CHAR_PTR tempdir; 
#endif /* unix */ 

#endif
	/* inputs: tempdir */    /*** d_report arrangement: none ***/
	/* output: none */
{
    int len;
    CHAR_PTR lastchar;

    len = ACstrlen(tempdir);
    if (len > ACTMPLEN) return(dBADNAME); /* too long */
    ACstrcpy(_dtmpdir, tempdir);
    lastchar = _dtmpdir + len - 1;
    if (len && (*lastchar != BACKSLASH))
    {
	*(++lastchar) = BACKSLASH;
	*(++lastchar) = (char) 0;
	len++;
    }
    return(SUCCESS);
} /* end of dUtmpdir() */

#ifdef _WINDOWS
char _dnmtmp[20];
#endif

CHAR_PTR DECLARE _dtmpnm(
#ifndef unix 

#ifdef ACDLL
			DBGDAT_PTR dbgptr,
#endif
			CHAR_PTR tempname)
#else  /* unix */ 
	tempname ) 
	CHAR_PTR tempname; 
#endif /* unix */ 

	/* input: none */    /*** d_report arrangement: none ***/
	/* output: tempname */
{
    CHAR_PTR ptr;
#ifdef _WINDOWS
    CHAR_PTR skip;
#endif

    ACstrcpy(tempname, _dtmpdir);
    ptr = tempname + ACstrlen(tempname);
#ifndef _WINDOWS
    return(tmpnam(ptr) ? tempname : (CHAR_PTR) 0);
#else
    _dnmtmp[0] = (char) 0;
    if (!tmpnam(_dnmtmp)) return((CHAR_PTR) 0);
    skip = _dnmtmp;
    if (*skip == BACKSLASH) skip++;
    ACstrcpy(ptr, skip);
    return(tempname);
#endif
} /* end of _dtmpnm() */
