/************
 * dDdate.c *
 ************/

/*****************************************************
*                                                    *
* Copyright 1989, Billy Shakespeare & Company, Inc.  *
* All rights reserved.                               *
*                                                    *
******************************************************
*                                                    *
* Published by                                       *
*        Copia International, Inc.                   *
*        Wheaton, Illinois                           *
*        U. S. A.                                    *
*                                                    *
******************************************************/

#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report tblptr->pdbg->d_report
#endif

int DECLARE dDdate(
#ifndef unix 
		/* d_report arrangement: 160 */
	/* input */
	DBF dbfptr,

	/* outputs */
	INT_PTR month,
	INT_PTR day,
	INT_PTR year)
#else  /* unix */ 
	
 	dbfptr, month, day, year ) 
	DBF dbfptr; 
	INT_PTR month; 
	INT_PTR day; 
	INT_PTR year; 
#endif /* unix */ 

{
    if (tblptr->tblcdata.flag != OPEN)
    {
        d_report = 161;
        return(dNOOPEN);
    }
    *month = tblptr->month;
    *day   = tblptr->day;
    *year  = tblptr->year + 1900;

    return(SUCCESS);

} /* end of dDdate() */

