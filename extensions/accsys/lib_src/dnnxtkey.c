/************
 * dNnxtkey *
 ************/

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
 
#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report nptr->pdbg->d_report
#endif

/*
 *  dNnxtkey reads a next record.
 */
int DECLARE dNnxtkey(
#ifndef unix 
 /* d_report arrangement: 3200 */
	/* input */
	NDX ndxptr,

	/* output */
	CHAR_PTR key,
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
    d_report = 3201;
    return(dNOOPEN);
}
if (pindex->position == (int) BOTTOM)
{
    d_report = 3202;
    return(dEOF);
}

switch(pindex->position)
{
case (int) UNINIT:
case (int) TOP:
    rc = _drewfwd(
#ifdef ACDLL
		nptr->pdbg,
#endif
		pindex, pio, REWIND);
    if (rc != SUCCESS) goto done;
    break;
}

pindex->position = INPROGRESS;
rc = _dnxtkey(
#ifdef ACDLL
		nptr->pdbg,
#endif
		pindex, pio, key, recno);

done:
return(rc);
} /* end of dNnxtkey() */
