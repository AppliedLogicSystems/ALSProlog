/**************
 * dNputkey.c *
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
 
#include "db4.h"
#include <stdlib.h>
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report nptr->pdbg->d_report
#endif

int DECLARE dNputkey(
#ifndef unix 
 /* d_report arrangement: 3270 */
	/* inputs */
	NDX    ndxptr,
	CHAR_PTR key,
	long   recno)
#else  /* unix */ 
	
 	ndxptr, key, recno ) 
	NDX ndxptr; 
	CHAR_PTR key; 
	long recno; 
#endif /* unix */ 

{
int rc;

if (nptr->ndx.xdxcdata.flag != OPEN)
{
    d_report = 3271;
    return(dNOOPEN);
}


rc = _dxput(
#ifdef ACDLL
	nptr->pdbg,
#endif
	&(nptr->ndx), &(nptr->ndxbuff), (PAVAILIST) 0, key, recno);

return(rc);

} /* end of dNputkey() */
