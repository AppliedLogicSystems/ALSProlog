/************
 * dNprvkey *
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
 
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report nptr->pdbg->d_report
#endif

/*
 *  dNprvkey reads a prev record.
 */
int DECLARE dNprvkey(
#ifndef unix 
 /* d_report arrangement: 3260 */
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
    d_report = 3261;
    return(dNOOPEN);
}

if (pindex->position == (int) TOP)
{
    d_report = 3262;
    return(dBOF);
}

switch(pindex->position)
{
case (int) UNINIT:
case (int) BOTTOM:
    rc = _drewfwd(
#ifdef ACDLL
		nptr->pdbg,
#endif
		pindex, pio, FORWARD);
    if (rc != SUCCESS) goto done;
    break;
}


pindex->position = INPROGRESS;

rc = _dprvkey(
#ifdef ACDLL
		nptr->pdbg,
#endif
		PREVIOUS, pindex, pio, key, (pPXDXNOD) 0, recno);

done:

return(rc);
} /* end of dNprvkey() */
