/*************
* dDapprec.c *
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

	/***
	 * After dDapprec(), the program should.
	 *  1. Update DBT, MDX, NDX, if any.
	 *  2. If last record to append, call dDulkapp() to release
	 *     APPEND LOCK for others. (for shared mode in multi-user)
	 ***/

#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report tblptr->pdbg->d_report
#undef d_request
#define d_request tblptr->pdbg->d_request
#undef d_recno
#define d_recno tblptr->pdbg->d_recno
#endif

int DECLARE dDapprec(
#ifndef unix 
 /* d_report arrangement: 320 */
	DBF dbfptr,
	CHAR_PTR record)
#else  /* unix */ 
	
 	dbfptr, record ) 
	DBF dbfptr; 
	CHAR_PTR record; 
#endif /* unix */ 

{
	long size;
	int rc;
	int offset;
	PCOMDAT comptr;
	PTBLNOD ndptr;
	long lll;

	d_recno = -1;
	comptr = &(tblptr->tblcdata);
	if (comptr->flag != OPEN)
	{
    	d_report = 321;
    	return(dNOOPEN);
	}
	if (comptr->ftype & DBRONLY || comptr->fmode & d_READONLY)
	{
    	d_report = 323;
    	return(dRDONLY);
	}

	size = comptr->size + 1L;

	rc = _dfindbf(dbfptr, size, ACWRITE, &lll, &offset, &ndptr);
	if (rc != SUCCESS)
	{
    	return(rc);
	}

	return(_dapprec(dbfptr, ndptr, record, offset));

} /* end of dDapprec() */

/*
 * The following code appends a record at the end of the
 * records list.
 */
int _DECLARE _dapprec(
#ifndef unix 

	/* inputs */  /* d_report arrangement: 340 */
 	DBF      dbfptr,
	PTBLNOD  ndptr,
	CHAR_PTR record,
	int    offset)
#else  /* unix */ 
	
 	dbfptr, ndptr, record, offset ) 
	DBF dbfptr; 
	PTBLNOD ndptr; 
	CHAR_PTR record; 
	int offset; 
#endif /* unix */ 


	/* outputs : none */
{
	PCOMDAT comptr;
	CHAR_PTR recptr; /* pointer to record */
	int iii;
	PTBLNOD ndptr2;
	PPOTATO potatptr;

	comptr = &(tblptr->tblcdata);

	while (1)
	{
    	if (ndptr->has < comptr->maxitems)
    	{
			recptr = ndptr->nodebuf + (comptr->itemlen * offset);
			iii = comptr->itemlen;
			if( d_request & NO_STATUS )
			{
    	    	iii--;
	    		*recptr++ = ' ';
        	}
        	(void) ACmemcpy(recptr, record, (unsigned) iii);
			ndptr->has++;
			ndptr->bufflag |= ACWRITE;
			comptr->changed = 1;
			comptr->size++;
			d_recno = comptr->size;
			return(SUCCESS);
    	}

    	/*
     	* no more space in this node: get a new node
     	*/
    	potatptr = _dgetptt( (pPPOTATO) &(tblptr->mru),
    			 (pPPOTATO) &(tblptr->lru));
    	ndptr2 = (PTBLNOD) potatptr->bufptr;

		if (ndptr2->bufflag & ACWRITE)
		{
            if (_dbfndwt(dbfptr, ndptr2, comptr) != SUCCESS)
	    	{
	        	d_report = 341;
	        	return(dIOERR);
	    	}
		}

    	/* initialize new node elements (ndptr->....) */
    	ndptr2->nodeid = comptr->eofnode++;
    	tblptr->tblacc[ndptr2->nodeid & ACCMASK]
			= (unsigned) (potatptr - tblptr->potatoes);

    	ndptr2->fstrec = ndptr->fstrec + ndptr->has;
    	ndptr2->has = 0;

    	offset = 0;
    	ndptr = ndptr2;

    	/* go back to top of the loop */
	} /* end of while-loop */

} /* end of _dapprec() */

