/**************
 * dUdfto3i.c *
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
 
/****************************************************************
 * Gregorian Calendar Definition:                               *
 *      Every fourth year is a leap year except century years.  *
 *      If a century year is divisible by 400, e.g., 2000,      *
 *      the century year is a leap year.                        *
 ****************************************************************/      

#include <stdlib.h>
#include <string.h>
#include "db4.h"
#include "d4def.h"

void DECLARE dUdfto3i(
#ifndef unix 

        /* input */     /*** d_report arrangement: N/A ***/
        CHAR_PTR dfield, /* date field */

        /* outputs */
        INT_PTR month,
        INT_PTR day,
        INT_PTR year)
#else  /* unix */ 
	
 	dfield, month, day, year ) 
	CHAR_PTR dfield; 
	INT_PTR month; 
	INT_PTR day; 
	INT_PTR year; 
#endif /* unix */ 

{
char buf[9];

(void) ACmemcpy(buf, dfield, 8);
*(buf + 8) = (char) 0;
if (!ACstrcmp((CHAR_PTR) buf, (CHAR_PTR) "        "))
{				       /* 01234567 */
    *day   = 9999;
    *month = 9999;
    *year  = 9999;
    return;
}
*day = ACatoi(buf + 6);
*(buf + 6) = (char) 0;
*month = ACatoi(buf + 4);
*(buf + 4) = (char) 0;
*year = ACatoi(buf);
} /* end of dUdfto3i() */


