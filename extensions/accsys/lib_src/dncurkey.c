/************
 * dNcurkey *
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
 *  dNcurkey reads the current key and its record number.
 */
int DECLARE dNcurkey(
#ifndef unix 
 /* d_report arrangement: 3040 */
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
PINDEX pindex;

pindex = &(nptr->ndx);

if ((pindex->xdxcdata).flag != OPEN)
{
    d_report = 3041;
    return(dNOOPEN);
}

switch(pindex->position)
{
case (int) TOP:
    d_report = 3042;
    return(dBOF);
    
case (int) BOTTOM:
    d_report = 3043;
    return(dEOF);

case (int) UNINIT:
    d_report = 3044;
    return(dNOTFOUND); /* undefined */
}

(void) ACmemcpy(key, pindex->curkey, pindex->complen);
*recno = pindex->currecno;

return(SUCCESS);

} /* end of dNcurkey() */

