/**************
 * dNupdkey.c *
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
#define d_report nptr->pdbg->d_report
#endif

/*
 *  dNupdkey updates a key in NDX.
 */
int DECLARE dNupdkey(
#ifndef unix 
 /* d_report arrangement: 3310 */
	/* inputs */
	NDX  ndxptr,
	CHAR_PTR oldkey,
	CHAR_PTR newkey,
	long recno)
#else  /* unix */ 
	
 	ndxptr, oldkey, newkey, recno ) 
	NDX ndxptr; 
	CHAR_PTR oldkey; 
	CHAR_PTR newkey; 
	long recno; 
#endif /* unix */ 


	/* output : none */
{
int rc;

if ((nptr->ndx).xdxcdata.flag != OPEN)
{
    d_report = 3311;
    return(dNOOPEN);
}


rc = _dkeyrm(
#ifdef ACDLL
	nptr->pdbg,
#endif
	&(nptr->ndx), &(nptr->ndxbuff), (PAVAILIST) 0, oldkey, recno);
if (rc != SUCCESS)
{
    if (rc == dEOF) rc = dNOTFOUND;
    goto done;
}
rc = _dxput(
#ifdef ACDLL
	nptr->pdbg,
#endif
	&(nptr->ndx), &(nptr->ndxbuff), (PAVAILIST) 0, newkey, recno);

done:

return(rc);

} /* end of dNupdkey() */
