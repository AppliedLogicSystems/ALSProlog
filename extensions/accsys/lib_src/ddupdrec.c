/*************
* dDupdrec.c *
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

#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report tblptr->pdbg->d_report
#undef d_request
#define d_request tblptr->pdbg->d_request
#endif

int _DECLARE _dDupdat(
#ifndef unix 
DBF dbfptr, long recno, CHAR_PTR record, int action)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;

int DECLARE dDupdrec(
#ifndef unix 

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
    return(_dDupdat(dbfptr, recno, record, UPDATE));
} /* end of dDupdrec() */

int DECLARE dDdelrec(
#ifndef unix 
DBF dbfptr, long recno)
#else  /* unix */ 
	
 	dbfptr, recno ) 
	DBF dbfptr; 
	long recno; 
#endif /* unix */ 

{
    return(_dDupdat(dbfptr, recno, (CHAR_PTR ) 0, DELETE));
} /* end of dDdelrec() */

int DECLARE dDrclrec(
#ifndef unix 
DBF dbfptr, long recno)
#else  /* unix */ 
	
 	dbfptr, recno ) 
	DBF dbfptr; 
	long recno; 
#endif /* unix */ 

{
    return(_dDupdat(dbfptr, recno, (CHAR_PTR ) 0, RECALL));
} /* end of dDrclrec() */


static int _DECLARE _dDupdat
		(
#ifndef unix 
DBF dbfptr, long recno, CHAR_PTR record, int action)
#else  /* unix */ 
	
 	dbfptr, recno, record, action ) 
	DBF dbfptr; 
	long recno; 
	CHAR_PTR record; 
	int action; 
#endif /* unix */ 

			/* d_report arrangement: 460 */
{
	PCOMDAT comptr;
	CHAR_PTR recptr; /* pointer to record */
	long size;
	int rc;
	PTBLNOD nodeptr;
	int iii; /* offset to record or copy-length */
	long lll;

	comptr = &(tblptr->tblcdata);
	if (comptr->flag != OPEN)
	{
    	d_report = 461;
    	return(dNOOPEN);
	}
	if (comptr->ftype & DBRONLY || comptr->fmode & d_READONLY)
	{
    	d_report = 463;
    	return(dRDONLY);
	}

	if ( (recno <= (long) 0) )
	{
		d_report = 464;
		return(dOUTRANGE); /* zero or negative */
	}

	size = comptr->size;
	if (recno > size)
	{
		d_report = 465;
		return(dOUTRANGE); /* record number too large */
	}

	lll = (long) tblptr->stadrs + (recno - 1) * comptr->itemlen;

	if ((rc = _dfindbf(dbfptr, recno, ACREAD,
		(LONG_PTR) &lll, (INT_PTR) &iii, (pPTBLNOD) &nodeptr))
    	!= SUCCESS)
	{
    	return(rc);
	}

	/* finally update record */
	recptr = nodeptr->nodebuf + (iii * comptr->itemlen);

	switch(action)
	{

		case UPDATE:
    		iii = comptr->itemlen;
    		if (d_request & NO_STATUS)
    		{
    			iii--;
				*recptr++ = ' ';
    		}
    		(void) ACmemcpy(recptr, record, (unsigned) iii);
    		break;

		case DELETE:
    		*recptr = '*';
    		break;

		case RECALL:
    		*recptr = ' ';
    		break;
	}

	nodeptr->bufflag = ACWRITE;

	return(SUCCESS);

} /* end of _dDupdat() */

