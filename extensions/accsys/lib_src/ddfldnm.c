/************
* dDfldnm.c *
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

int DECLARE dDfldnm(
#ifndef unix 

	/* inputs */      /* d_report arrangement: 200 */
	DBF    dbfptr,
	CHAR_PTR fieldnm,

	/* outputs */
	INT_PTR offset,
	INT_PTR fieldpos,
	CHAR_PTR type,
	INT_PTR width,
	INT_PTR decimal)
#else  /* unix */ 
	
 	dbfptr, fieldnm, offset, fieldpos, type, width, decimal ) 
	DBF dbfptr; 
	CHAR_PTR fieldnm; 
	INT_PTR offset; 
	INT_PTR fieldpos; 
	CHAR_PTR type; 
	INT_PTR width; 
	INT_PTR decimal; 
#endif /* unix */ 

{
	int i;
	PFLDTL fldtlptr;
	PFLDNM fldnmptr;
	int ofst;
	int position;
	int found;

	if (tblptr->tblcdata.flag != OPEN)
	{
    	d_report = 201;
    	return(dNOOPEN);
	}

	ofst = 0;
	position = 1;

	fldtlptr=tblptr->fltyplen;
	fldnmptr=tblptr->fldnames;

	found = 0;

	if (fieldnm)
	{
    	for(i=tblptr->fldsnum; i; i--, fldtlptr++, fldnmptr++)
    	{
			if (!ACstrcmp(ACstrupr(fieldnm), fldnmptr->fieldnm))
			{
	    		found = 1;
	    		break;
			}
			ofst += (int) fldtlptr->length;
			position++;
    	}
	}

	if (!found)
	{
    	d_report = 202;
    	return(dNOTFOUND);
	}
	*offset = ofst + 1; /* add 1 for the first status byte */
	*fieldpos = position;
	*type = fldtlptr->type;
	*width = fldtlptr->length;
	*decimal = (int) fldtlptr->decimal;

	return(SUCCESS);
} /* end of dDfldnm() */
