/**************
 * dXupdkey.c *
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
#define d_report iptr->amdx->pdbg->d_report
#endif

/*
 *  dXupdkey updates a key in MDX.
 */
int DECLARE dXupdkey(
#ifndef unix 
 /* d_report arrangement: 1450 */
	/* inputs */
	IDX  idxptr,
	CHAR_PTR oldkey,
	CHAR_PTR newkey,
	long recno)
#else  /* unix */ 
	
 	idxptr, oldkey, newkey, recno ) 
	IDX idxptr; 
	CHAR_PTR oldkey; 
	CHAR_PTR newkey; 
	long recno; 
#endif /* unix */ 


	/* output : none */
{
int rc;

if (iptr->amdx->flag != OPEN)
{   /* file not open */
    d_report = 1451;
    return(dNOOPEN);
}

if ((iptr->idx).xdxcdata.flag != OPEN)
{
    d_report = 1452;
    return(dNOOPEN);
}


rc = _dkeyrm(
#ifdef ACDLL
	iptr->amdx->pdbg,
#endif
	&(iptr->idx), &(iptr->amdx->mdxbuff), &(iptr->amdx->freelist),
	oldkey, recno);
if (rc != SUCCESS)
{
    if (rc == dEOF) rc = dNOTFOUND;
    goto done;
}

rc = _dxput(
#ifdef ACDLL
	iptr->amdx->pdbg,
#endif
	&(iptr->idx), &(iptr->amdx->mdxbuff), &(iptr->amdx->freelist),
	newkey, recno);

done:

return(rc);

} /* end of dXupdkey() */
