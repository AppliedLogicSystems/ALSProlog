/**************
 * dNgetrno.c *
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
 *  dNgetrno obtains a record number from an index file based on a key.
 */
int DECLARE dNgetrno(
#ifndef unix 
 /* d_report arrangement: 3120 */
	/* inputs */
	NDX ndxptr,
	CHAR_PTR key,

	/* output */
	LONG_PTR recno)
#else  /* unix */ 
	
 	ndxptr, key, recno ) 
	NDX ndxptr; 
	CHAR_PTR key; 
	LONG_PTR recno; 
#endif /* unix */ 

{
int rc;
PINDEX pindex;
PADIOBUFF pio;

pindex = &(nptr->ndx);
pio = &(nptr->ndxbuff);
if ((pindex->xdxcdata).flag != OPEN)
{
    d_report = 3121;
    return(dNOOPEN);
}

rc = _dXgrec(
#ifdef ACDLL
		nptr->pdbg,
#endif
		pindex, pio, key, recno);
return(rc);
} /* end of dNgetrno() */
