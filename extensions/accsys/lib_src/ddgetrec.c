/*************
* dDgetrec.c *
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
 
                /* d_report arrangement: 260 */

#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report tblptr->pdbg->d_report
#undef d_request
#define d_request tblptr->pdbg->d_request
#endif

int DECLARE dDgetrec(
#ifndef unix 

	/* inputs */
	DBF    dbfptr,
	long   recno,

	/* output */
	CHAR_PTR record)
#else  /* unix */ 
	
 	dbfptr, recno, record ) 
	DBF dbfptr; 
	long recno; 
	CHAR_PTR record; 
#endif /* unix */ 

{
	PCOMDAT comptr;
	CHAR_PTR recptr; /* pointer to record */
	long size;
	int rc;
	PTBLNOD nodeptr;
	long nodenum;
	int  iii; /* offset to record or copy-length */

	comptr = &(tblptr->tblcdata);
	if (comptr->flag != OPEN)
	{
    	d_report = 261;
    	return(dNOOPEN);
	}

	if ( (recno <= (long) 0) )
	{
		d_report = 262;
		return(dOUTRANGE); /* zero or negative */
	}

	size = comptr->size;

	if (recno > size)
	{
		d_report = 263;
		return(dOUTRANGE); /* record number too large */
	}


	if ((rc = 
    	_dfindbf(dbfptr, recno, ACREAD,
	     	(LONG_PTR) &nodenum, (INT_PTR) &iii, (pPTBLNOD) &nodeptr))
    	!= SUCCESS)
	{
    	return(rc);
	}

 	/* finally read record from buffer */
	recptr = nodeptr->nodebuf + (iii * comptr->itemlen);
	iii = comptr->itemlen; /* copy length */
	if (d_request & NO_STATUS)
	{
   		iii--;
   		recptr++;
	}
	(void) ACmemcpy(record, recptr, (unsigned) iii);

	return(SUCCESS);
} /* end of dDgetrec() */
