/*************
 * dDchmod.c *
 *************/

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
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#ifndef ACDLL
int DECLARE dDchmod(
#ifndef unix 
  /* d_report arrangement: 120 */
	/* inputs */
	CHAR_PTR dbfname,
	int  mode

	/* output: none */
		   )
#else  /* unix */ 
	
 	dbfname, mode ) 
	CHAR_PTR dbfname; 
	int mode; 
#endif /* unix */ 
#else
int DECLARE DdDchmod(  /* d_report arrangement: 120 */
	/* inputs */
	DBGDAT_PTR dbgptr,
#endif

{
int fh;
char buff[ACTMPLEN];
int rc;

dUexpnm(dbfname, "DBF", buff);
fh = _dopn(
#ifdef ACDLL
		dbgptr,
#endif
		buff, d_SINGLE);
if (fh < 0) return(fh);

if (_ACread(fh, buff, 1) != 1)
{
	d_report = 121;
	(void) _dcls(fh);
	return(dBADFILE);
}
if (!(buff[0] & isDBF))
{
	d_report = 122;
	(void) _dcls(fh);
	return(dBADFILE);
}
if (buff[0] & DBRONLY)
{ /* currently read-only */
	if (mode == d_READONLY)
	{
		(void) _dcls(fh);
		return(SUCCESS); /* already read-only */
	}
}
else
{ /* currently read/write allowed */
	if (mode != d_READONLY)
	{ /* read/write request */
		(void) _dcls(fh);
		return(SUCCESS); /* already read/write allowed */
	}
}

if (mode == d_READONLY)
{
     buff[0] |= DBRONLY;
}
else
{
    buff[0] &= ~DBRONLY;
}

rc = _dseekwt(fh, (long) 0, buff, 1);
if (rc != SUCCESS)
{
	d_report = 123;
	rc = dIOERR;
}
(void) _dcls(fh);

return(rc);

} /* end of dDchmod() */

