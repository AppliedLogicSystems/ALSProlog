/*************
* dDinsrec.c *
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
#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report tblptr->pdbg->d_report
#endif

int DECLARE dDinsrec(
#ifndef unix 
			/* d_report arrangement: 270 */
	DBF      dbfptr,
	long     recno,
	CHAR_PTR record)
#else  /* unix */ 
	
 	dbfptr, recno, record ) 
	DBF dbfptr; 
	long recno; 
	CHAR_PTR record; 
#endif /* unix */ 

{
	PCOMDAT comptr;
	PTBLNOD nodeptr;
	int rc, iii;
	long size, ll, lll;
	CHAR_PTR saverec;
	CHAR_PTR recptr;


	comptr = &(tblptr->tblcdata);
	if (comptr->flag != OPEN)
	{
    	d_report = 271;
    	return(dNOOPEN);
	}

	if (comptr->ftype & DBRONLY || comptr->fmode & d_READONLY)
	{
    	d_report = 273;
    	return(dRDONLY);
	}

	rc = dDreccnt(dbfptr, (LONG_PTR) &size);
	if (rc != SUCCESS) return(rc);
	if (recno <= (long) 0 || (size + 1) < recno)
	{
		d_report = 274;
		return(dOUTRANGE); /* record number out of range */
	}

rc = dDapprec(dbfptr, (CHAR_PTR) record); /* this record will be overwritten */
	if (rc != SUCCESS) return(rc);
	saverec = (CHAR_PTR) _ACalloc((unsigned long) (comptr->itemlen));
	if (!saverec)
	{
    	d_report = 275;
    	return(dMEMERR);
	}
	for (ll = size; recno <= ll; ll--)
	{
    	/* read record (k) from buffer */
    	rc = _dfindbf(dbfptr, ll, ACREAD,
    		  (LONG_PTR) &lll, (INT_PTR) &iii, (pPTBLNOD) &nodeptr);
    	if (rc != SUCCESS) goto byebye;
    	recptr = nodeptr->nodebuf + (iii * comptr->itemlen);
    	(void) ACmemcpy(saverec, recptr, (unsigned) comptr->itemlen);

    	/* update record (k + 1) */
    	rc = _dfindbf(dbfptr, ll + 1L, ACREAD,
		  	(LONG_PTR) &lll, (INT_PTR) &iii, (pPTBLNOD) &nodeptr);
    	if (rc != SUCCESS) goto byebye;
    	recptr = nodeptr->nodebuf + (iii * comptr->itemlen);
    	(void) ACmemcpy(recptr, saverec, (unsigned) comptr->itemlen);
    	nodeptr->bufflag |= ACWRITE;
	}

	rc = dDupdrec(dbfptr, recno, record);

byebye:

	_ACfree(saverec);
	return(rc);

} /* end of dDinsrec() */
