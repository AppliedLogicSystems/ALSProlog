/************
 * dXnxtkey *
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
#define d_report iptr->amdx->pdbg->d_report
#endif

/*
 *  dXnxtkey reads a next record.
 */
int DECLARE dXnxtkey(
#ifndef unix 
 /* d_report arrangement: 1260 */
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
    d_report = 1261;
    return(dNOOPEN);
}

pindex = &(iptr->idx);
pio = &(iptr->amdx->mdxbuff);

if ((pindex->xdxcdata).flag != OPEN)
{
    d_report = 1262;
    return(dNOOPEN);
}

if (pindex->position == (int) BOTTOM)
{
    d_report = 1263;
    return(dEOF);
}

switch(pindex->position)
{
case (int) UNINIT:
case (int) TOP:
    rc = _drewfwd(
#ifdef ACDLL
		iptr->amdx->pdbg,
#endif
		pindex, pio, REWIND);
    if (rc != SUCCESS) goto done;
    break;
}

pindex->position = INPROGRESS;
rc = _dnxtkey(
#ifdef ACDLL
		iptr->amdx->pdbg,
#endif
		pindex, pio, key, recno);

done:
return(rc);
} /* end of dXnxtkey() */
