/**************
 * dDfields.c *
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

int DECLARE dDfields(
#ifndef unix 
		/* d_report arrangement: 180 */
	DBF dbfptr
	    )
#else  /* unix */ 
	dbfptr ) 
	DBF dbfptr; 
#endif /* unix */ 

{
    if (tblptr->tblcdata.flag != OPEN)
    {
        d_report = 181;
        return(dNOOPEN);
    }

    return(tblptr->fldsnum);
} /* end of dDfields() */

