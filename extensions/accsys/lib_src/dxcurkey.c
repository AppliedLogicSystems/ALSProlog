/************
 * dXcurkey *
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
 *  dXcurkey reads the current key and its record number.
 */
int DECLARE dXcurkey(
#ifndef unix 
 /* d_report arrangement: 1080 */
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
PINDEX pindex;

if (iptr->amdx->flag != OPEN)
{   /* file not open */
    d_report = 1081;
    return(dNOOPEN);
}

pindex = &(iptr->idx);

if ((pindex->xdxcdata).flag != OPEN)
{
    d_report = 1082;
    return(dNOOPEN);
}

switch(pindex->position)
{
case (int) TOP:
    d_report = 1083;
    return(dBOF);
    
case (int) BOTTOM:
    d_report = 1084;
    return(dEOF);

case (int) UNINIT:
    d_report = 1085;
    return(dNOTFOUND); /* undefined */
}

(void) ACmemcpy(key, pindex->curkey, pindex->complen);
*recno = pindex->currecno;

return(SUCCESS);

} /* end of dXcurkey() */

