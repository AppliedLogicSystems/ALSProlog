/**************
 * dDrecsta.c *
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

#ifdef ACDLL
#undef d_report
#define d_report tblptr->pdbg->d_report
#endif

int DECLARE dDrecsta(
#ifndef unix 
  /* d_report arrangement: 380 */
	/* inputs */
	DBF dbfptr,
	long recno)
#else  /* unix */ 
	
 	dbfptr, recno ) 
	DBF dbfptr; 
	long recno; 
#endif /* unix */ 

	/* output: none */
{
PCOMDAT comptr;
long size;
int rc;
PTBLNOD nodeptr;
long nodenum;
int  offset;

comptr = &(tblptr->tblcdata);
if (comptr->flag != OPEN)
{
    d_report = 381;
    return(dNOOPEN);
}
if ( (recno <= (long) 0) )
{
	d_report = 382;
	return(dOUTRANGE); /* zero or negative */
}

rc = _dtblsiz(dbfptr, &size);
if (rc != SUCCESS) return(rc);
if (recno > size)
{
	d_report = 383;
	return(dOUTRANGE); /* record number too large */
}


if ((rc = 
    _dfindbf(dbfptr, recno, ACREAD, &nodenum, &offset, &nodeptr)) != SUCCESS)
{
    return(rc);
}
return( (int) *(nodeptr->nodebuf + (offset * comptr->itemlen)) );
} /* end of dDrecsta() */
