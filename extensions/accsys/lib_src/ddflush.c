/************
* dDflush.c *
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

#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report tblptr->pdbg->d_report
#endif

int DECLARE dDflush(
#ifndef unix 
DBF dbfptr)
#else  /* unix */ 
	dbfptr ) 
	DBF dbfptr; 
#endif /* unix */ 
 /*** d_report arrangement: 240 ***/
{
#define MBUFSIZE 7
	char minibuf[MBUFSIZE];
	CHAR_PTR miniptr;
	PTBLNOD nodeptr;
	int i;
	int year, month, day;
	PCOMDAT comptr;

	comptr = &(tblptr->tblcdata);
	if (comptr->flag != OPEN)
	{
    	d_report = 241;
    	return(dNOOPEN);
	}

	for (i=1, nodeptr=tblptr->nodes; i <= tblptr->potatnum; i++, nodeptr++)
	{
    	if (!(nodeptr->bufflag & ACWRITE)) continue;

    	if (_dbfndwt(dbfptr, nodeptr, comptr) != SUCCESS)
    	{
        	d_report = 242;
        	return(dIOERR);
    	}

	} /* end of for loop */

	if (comptr->changed)
	{ /* current rec. # != original # */
    	dUtoday(&month, &day, &year);
    	miniptr = minibuf;
    	*miniptr++ = (char) (year - 1900) & 0x00ff;
    	*miniptr++ = (char) month;
    	*miniptr++ = (char) day;

    	_bytes4(comptr->size, miniptr);

    	i = _dseekwt(comptr->handle, (long) 1, minibuf, MBUFSIZE);
    	if (i != SUCCESS)
    	{
			d_report = 243;
			return(dIOERR);
    	}

    	comptr->changed = 0;
	}

	return(SUCCESS);
} /* end of dDflush() */

