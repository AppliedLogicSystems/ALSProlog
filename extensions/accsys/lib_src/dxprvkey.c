/************
 * dXprvkey *
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
#define d_report iptr->amdx->pdbg->d_report
#endif

/*
 *  dXprvkey reads a prev record.
 */
int DECLARE dXprvkey(
#ifndef unix 
 /* d_report arrangement: 1320 */
	/* input */
	IDX idxptr,

	/* output */
	CHAR_PTR key,
	LONG_PTR recno)
#else  /* unix */ 
	
 	idxptr, key, recno ) 
	IDX idxptr; 
	CHAR_PTR key; 
	LONG_PTR recno; 
#endif /* unix */ 

{
int rc;
PINDEX pindex;
PADIOBUFF pio;

if (iptr->amdx->flag != OPEN)
{   /* file not open */
    d_report = 1321;
    return(dNOOPEN);
}

pindex = &(iptr->idx);
pio = &(iptr->amdx->mdxbuff);

if ((pindex->xdxcdata).flag != OPEN)
{
    d_report = 1322;
    return(dNOOPEN);
}
if (pindex->position == (int) TOP)
{
    d_report = 1323;
    return(dBOF);
}

switch(pindex->position)
{
case (int) UNINIT:
case (int) BOTTOM:
    rc = _drewfwd(
#ifdef ACDLL
		iptr->amdx->pdbg,
#endif
		pindex, pio, FORWARD);
    if (rc != SUCCESS) goto done;
    break;
}

pindex->position = INPROGRESS;
rc = _dprvkey(
#ifdef ACDLL
		iptr->amdx->pdbg,
#endif
		PREVIOUS, pindex, pio, key, (pPXDXNOD) 0, recno);

done:
return(rc);
} /* end of dXprvkey() */
