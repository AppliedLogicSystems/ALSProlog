/*************
* dTmemuse.c *
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

#include <stdlib.h>
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#ifndef ACDLL
long DECLARE dTmemuse(
#ifndef unix 
CHAR_PTR dbtname)
#else  /* unix */ 
	dbtname ) 
	CHAR_PTR dbtname; 
#endif /* unix */ 

#else
long DECLARE DdTmemuse(
#ifndef unix 
DBGDAT_PTR dbgptr, CHAR_PTR dbtname)
#else  /* unix */ 
	
 	dbgptr, dbtname ) 
	DBGDAT_PTR dbgptr; 
	CHAR_PTR dbtname; 
#endif /* unix */ 

#endif
				/*** d_report arrangement: N/A ***/
	/* input: dbtname -- character string that represents
                             the name of memo file to open */
	/* output: none */

	/* returns */
	        /* long: size of memory allocated */
{
int i;  /* general purpose integer */
int fh;
char exname[ACTMPLEN];
long lrc;

dUexpnm(dbtname, "DBT", exname);
fh = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		exname, d_READONLY);
if (fh < 0) return((long) fh);

lrc = _dTmemus(
#ifdef ACDLL
		dbgptr,
#endif
		fh, &i); /* i == dummy */
(void) _dcls(fh);

return(lrc);
} /* end of dTmemuse() */

long _DECLARE _dTmemus(
#ifndef unix 

			/*** d_report arrangement: 2090 ***/
	/* inputs */
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	int fh, /* file hanle of an open table file */
	/*** d_report arrangement: 2090 ***/ 
	/* output */
        INT_PTR blocksiz)
#else  /* unix */ 
 	fh, blocksiz ) 
	int fh; 
	INT_PTR blocksiz; 
#endif /* unix */ 
 /* basic size */

	/* returns */
	        /* long: total size of memory required */

{
int bsiz;
#define MINISIZ 0x2
char minibuf[MINISIZ];

*blocksiz = 0; /* for later check by calling routine */

bsiz = _dseekrd(fh, (long) 0x14, minibuf, MINISIZ);
if (bsiz != SUCCESS)
{
	d_report = 2091;
	return( (long) bsiz); /* _ACread() failed: not too much to do */
}

bsiz = _2bytes(minibuf);

*blocksiz = bsiz;

return( (long) bsiz + (long) SZTBLFILE );

} /* end of _dTmemus() */
