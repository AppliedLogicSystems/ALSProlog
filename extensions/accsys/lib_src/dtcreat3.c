/*************
* dTcreat3.c *
**************/

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
#include <fcntl.h>
#ifdef MSC
#include <sys\types.h>
#endif
#include <string.h>
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <sys\stat.h>
#include <io.h>
#else
#include <fcntl.h>
#endif

/*
 *   Example)  dTcreat3("memofile");
 */
 /* dTcreat3() requires assoicated dbtname.DBF. */

#ifndef ACDLL
int DECLARE dTcreat3(
#ifndef unix 
CHAR_PTR dbtname)
#else  /* unix */ 
	dbtname ) 
	CHAR_PTR dbtname; 
#endif /* unix */ 

#else
int DECLARE DdTcreat3(
#ifndef unix 
DBGDAT_PTR dbgptr, CHAR_PTR dbtname)
#else  /* unix */ 
	
 	dbgptr, dbtname ) 
	DBGDAT_PTR dbgptr; 
	CHAR_PTR dbtname; 
#endif /* unix */ 

#endif
		/*** d_report arrangement: 2200 ***/
	/* input : dbtname -- DBT name */
	
	/* output: none */
{
int fh;
int i;  /* general purpose integer */
char name[80];
char buff[DBTMINSZ];

dUexpnm(dbtname, "DBT", name);

fh = _dcre8new((CHAR_PTR) name, FORCE);
if (fh < 0)
{
	d_report = 2201;
	return(dIOERR);    /* file creation by open() failed */
}

(void) ACmemset(buff, 0, DBTMINSZ); /* clear buffer */

/* dBASE III (PLUS) format */
_bytes4( (long) 1, buff);
i = SNODSIZ;

/* write the header */
if (_ACwrite(fh, buff, (unsigned) i) != (unsigned) i)
{
    close(fh);
    (void) _ACremove(name);
    d_report = 2202;
    return(dIOERR);
}

(void) close(fh); /* close DBT */

return(_dtchdbf(
#ifdef ACDLL
	dbgptr,
#endif
	name, ~isDBT4)); /* touch DBF */

} /* end of dTcreat3() */
