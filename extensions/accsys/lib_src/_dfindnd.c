/**************
 * _dfindnd.c *
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

void _DECLARE _dfindnd(dbfptr,recno,nodenum,offset)
		/* inputs */      /* d_report arrangement: N/A */
 	DBF	dbfptr;
	long	recno; /* offset record number */

		/* output */
    LONG_PTR nodenum; /* node number */
	INT_PTR  offset;  /* offset # of record in node */
{
	long nodeno;
	PCOMDAT comptr;

	comptr = &(tblptr->tblcdata);

	nodeno = (recno - 1) / comptr->maxitems;
	*nodenum = nodeno + 1;
	*offset = (int) (recno - nodeno * comptr->maxitems - 1);

} /* end of _dfindnd() */

