/*************
 * dNrmkey.c *
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
 
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report nptr->pdbg->d_report
#endif

/*
 *  dNrmkey removes a key from an index.
 */
int DECLARE dNrmkey(
#ifndef unix 
 /* d_report arrangement: 3300 */
	/* inputs */
	NDX  ndxptr,
	CHAR_PTR key,
	long recno)
#else  /* unix */ 
	
 	ndxptr, key, recno ) 
	NDX ndxptr; 
	CHAR_PTR key; 
	long recno; 
#endif /* unix */ 


	/* output : none */
{
int rc;

if ((nptr->ndx).xdxcdata.flag != OPEN)
{
    d_report = 3301;
    return(dNOOPEN);
}


rc = _dkeyrm(
#ifdef ACDLL
		nptr->pdbg,
#endif
		&(nptr->ndx), &(nptr->ndxbuff), (PAVAILIST) 0, key, recno);
if (rc != SUCCESS)
{
    if (rc == dEOF) rc = dNOTFOUND;
}


return(rc);

} /* end of dNrmkey() */
