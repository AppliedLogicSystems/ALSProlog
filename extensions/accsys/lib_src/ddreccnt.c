/**************
 * dDreccnt.c *
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
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report tblptr->pdbg->d_report
#endif

int DECLARE dDreccnt(
#ifndef unix 
  /* d_report arrangement: 420 */
	/* input */
	DBF dbfptr,

	/* output */
	LONG_PTR reccnt)
#else  /* unix */ 
	
 	dbfptr, reccnt ) 
	DBF dbfptr; 
	LONG_PTR reccnt; 
#endif /* unix */ 

{
	PCOMDAT comptr;
	int rc;

	comptr = &(tblptr->tblcdata);
	if (comptr->flag != OPEN)
	{
    	d_report = 421;
    	return(dNOOPEN);
	}

	rc = _dtblsiz(dbfptr, reccnt);

	return(rc);

} /* end of dDreccnt() */


int _DECLARE _dtblsiz( dbfptr, reccnt)
 /*** d_report arrangement: 426 ***/
	DBF dbfptr;
	LONG_PTR reccnt;
{
	PCOMDAT comptr;

	comptr = &(tblptr->tblcdata);

	*reccnt = comptr->size;

	return(SUCCESS);
} /* end of _dtblsiz() */
