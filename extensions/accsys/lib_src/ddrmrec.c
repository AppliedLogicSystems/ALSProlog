/*************
 * dDrmrec.c *
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

		/* d_report arrangement: 400 */
#include <stdlib.h>
#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report tblptr->pdbg->d_report
#endif

extern void _DECLARE zerochk(
#ifndef unix 
DBF dbfptr, PCOMDAT comptr, PTBLNOD nodeptr)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;

int DECLARE dDrmrec(
#ifndef unix 

	DBF    dbfptr,
	long   recno)
#else  /* unix */ 
	
 	dbfptr, recno ) 
	DBF dbfptr; 
	long recno; 
#endif /* unix */ 

{
	long size, ll, lll, lastnod;
	int rc, iii;
	PCOMDAT comptr;
	CHAR_PTR saverec, recptr;
	PTBLNOD nodeptr;

	comptr = &(tblptr->tblcdata);
	if (comptr->flag != OPEN)
	{
    	d_report = 401;
    	return(dNOOPEN);
	}

	if (comptr->ftype & DBRONLY || comptr->fmode & d_READONLY)
	{
    	d_report = 403;
    	return(dRDONLY);
	}

	rc = _dtblsiz(dbfptr, &size);
	if (rc != SUCCESS) return(rc);
	if ( (recno > size) || (recno <= (long) 0) )
	{
		d_report = 404;
		return(dOUTRANGE); /* too large record number, zero or negative */
	}

	saverec = (CHAR_PTR) _ACalloc((unsigned long) comptr->itemlen);
	if (!saverec)
	{
    	d_report = 405;
    	return(dMEMERR);
	}

	if (recno == size)
	{
    	rc = _dfindbf(dbfptr, recno, ACREAD, &lll, &iii, &nodeptr);
    	if (rc == SUCCESS)
    	{
			nodeptr->has--;
			zerochk(dbfptr, comptr, nodeptr);
		nodeptr->bufflag |= ACWRITE; /* ENDMARK will be done by _dbfndwt() */
			comptr->changed = 1;
			comptr->size--;
    	}
    	goto byebye;
	}

	_dfindnd(dbfptr, size, &lastnod, &iii);

	for (ll = recno; ll < comptr->size; ll++)
	{
    	/* read record (k + 1) from buffer */
    	rc = _dfindbf(dbfptr, ll + 1L, ACREAD, &lll, &iii, &nodeptr);
    	if (rc != SUCCESS) goto byebye;
    	recptr = nodeptr->nodebuf + (iii * comptr->itemlen);
    	(void) ACmemcpy(saverec, recptr, (unsigned) comptr->itemlen);
    	if ((ll + 1L) == comptr->size)
    	{
			nodeptr->has--;
			zerochk(dbfptr, comptr, nodeptr);
    	}

    	/* update record (k) */
    	rc = _dfindbf(dbfptr, ll, ACREAD, &lll, &iii, &nodeptr);
    	if (rc != SUCCESS) goto byebye;
    	recptr = nodeptr->nodebuf + (iii * comptr->itemlen);
    	(void) ACmemcpy(recptr, saverec, (unsigned) comptr->itemlen);
    	nodeptr->bufflag |= ACWRITE;
    		/* ENDMARK will be done by _dbfndwt(), if needed */
	}
	comptr->changed = 1;
	comptr->size--;
	rc = SUCCESS;

	byebye:

	_ACfree(saverec);
	return(rc);

} /* end of dDrmrec() */

static void _DECLARE zerochk(
#ifndef unix 

	DBF	 dbfptr,
	PCOMDAT  comptr,
	PTBLNOD  nodeptr)
#else  /* unix */ 
	
 	dbfptr, comptr, nodeptr ) 
	DBF dbfptr; 
	PCOMDAT comptr; 
	PTBLNOD nodeptr; 
#endif /* unix */ 

{
	if (nodeptr->has == 0)
	{
	    comptr->eofnode--;
	    tblptr->lstpnode--;
	    if (comptr->eofnode < 2L)
	    {
	        comptr->eofnode = 2L;
		tblptr->lstpnode = 1L;
	    }
	    tblptr->tblacc[(nodeptr->nodeid) & ACCMASK] = NIL;
	    /*nodeptr->nodeid = (long) 0;  -- dDrmrec still needs this */
	    nodeptr->bufflag = UNUSED;
	    nodeptr->fstrec  = (long) 0;
	}
} /* end of zerochk() */
