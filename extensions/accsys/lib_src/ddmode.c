/*************
 * dDmode.c *
 *************/

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
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

int DECLARE dDmode(
#ifndef unix 
#ifndef ACDLL
  /* d_report arrangement: 280 */
	/* input */
#else
int DECLARE DdDmode(  /* d_report arrangement: 280 */
	/* input */
	DBGDAT_PTR dbgptr,
#endif
	CHAR_PTR dbfname
		  )
#else  /* unix */ 
	dbfname ) 
	CHAR_PTR dbfname; 
#endif /* unix */ 


	/* output: none */
	/* returns: mode value; negative value if error */
{
int fh;
#define SMALL 0x32
char buff[SMALL];
int mode;
unsigned char dbftype;

mode = 0;
dUexpnm(dbfname, "DBF", buff);
fh = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		buff, d_SHARED);
if (fh < 0) return(fh);

if (_ACread(fh, buff, SMALL) != SMALL)
{
	d_report = 281;
	(void) _dcls(fh);
	return(dIOERR);
}

(void) _dcls(fh); /* no need to keep the file open */

dbftype = buff[0];
if (!(dbftype & isDBF))
{
	d_report = 282;
	return(dBADFILE);
}

if (dbftype & DBRONLY) mode |= d_READONLY;
if (dbftype & hasDBT)  mode |= (dbftype & isDBT4) ? hasDBT4 : hasDBT3;
if (*(buff + MDXFLAG)) mode |= hasMDX;
if (buff[ENCRYPT]) mode |= isENCRYPTED;

return(mode);
} /* end of dDmode() */

