/**************
 * dDapart.c *
 **************/

/***********************************************************
*                                                          *
* Copyright 1989, 1990, Billy Shakespeare & Company, Inc.  *
* All rights reserved.                                     *
*                                                          *
************************************************************
*                                                          *
* Published by                                             *
*        Copia International, Inc.                         *
*        Wheaton, Illinois                                 *
*        U. S. A.                                          *
*                                                          *
************************************************************/
 
#include <stdio.h>
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#ifndef ACDLL
int DECLARE dDapart(
#ifndef unix 
  /* d_report arrangement: 100 */
	/* input */
	CHAR_PTR dbfname)
#else  /* unix */ 
	dbfname ) 
	CHAR_PTR dbfname; 
#endif /* unix */ 

#else
int DECLARE DdDapart(
#ifndef unix 
  /* d_report arrangement: 100 */
	/* inputs */
	DBGDAT_PTR dbgptr,
	CHAR_PTR dbfname)
#else  /* unix */ 
	
 	dbgptr, dbfname ) 
	DBGDAT_PTR dbgptr; 
	CHAR_PTR dbfname; 
#endif /* unix */ 

#endif

	/* output: none */
{
int fh;
#define SMALL 0x20
char buff[SMALL];
int rc;

dUexpnm(dbfname, "DBF", buff);
fh = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		buff, d_SINGLE);
if (fh < 0) return(fh);
if (_ACread(fh, buff, SMALL) != SMALL)
{
	d_report = 101;
	(void) _dcls(fh);
	return(dIOERR);
}

if (!(buff[0] & isDBF))
{
	d_report = 102;
	(void) _dcls(fh);
	return(dBADFILE);
}
if (!buff[MDXFLAG])
{
	(void) _dcls(fh);
	return(SUCCESS);  /* nothing needs to be done */
}
if (buff[0] & DBRONLY)
{
    (void) _dcls(fh);
    d_report = 103;
    return(dRDONLY);
}

buff[0] = 0;
rc = _dseekwt(fh, (long) MDXFLAG, buff, 1);
if (rc != SUCCESS)
{
	d_report = 104;
	rc = dIOERR;
}
(void) _dcls(fh);

return(rc);

} /* end of dDapart() */
