/************
* dDfldno.c *
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

#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report tblptr->pdbg->d_report
#endif

int DECLARE dDfldno(
#ifndef unix 

	/* input */  /* d_report arrangement: 220 */
	DBF     dbfptr,
	int	fieldpos,

	/* output */
	INT_PTR  offset,
	CHAR_PTR fieldnm,
	CHAR_PTR type,
	INT_PTR  width,
	INT_PTR  decimal)
#else  /* unix */ 
	
 	dbfptr, fieldpos, offset, fieldnm, type, width, decimal ) 
	DBF dbfptr; 
	int fieldpos; 
	INT_PTR offset; 
	CHAR_PTR fieldnm; 
	CHAR_PTR type; 
	INT_PTR width; 
	INT_PTR decimal; 
#endif /* unix */ 

{
PFLDTL fldtlptr;
PFLDNM fldnmptr;
int ofst;

if (tblptr->tblcdata.flag != OPEN)
{
    d_report = 221;
    return(dNOOPEN);
}

if (fieldpos < 1 || tblptr->fldsnum < fieldpos)
{
    d_report = 222;
    return(dOUTRANGE);
}

ofst = 0;
fldtlptr = tblptr->fltyplen;
fldnmptr = tblptr->fldnames;

fieldpos--;
while(fieldpos--)
{
    ofst += (int) fldtlptr->length;
    fldtlptr++;
    fldnmptr++;
}

*offset = ofst + 1; /* add 1 for the first status byte */
ACstrcpy(fieldnm, fldnmptr->fieldnm);
*type = fldtlptr->type;
*width = fldtlptr->length;
*decimal = (int) fldtlptr->decimal;

return(SUCCESS);
} /* end of dDfldno() */
