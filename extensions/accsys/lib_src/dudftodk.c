/**************
 * dUdftodk.c *
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

static char blankdate[] = {0x7d, 0xc3, 0x94, 0x25, 0xad, 0x49, 0xb2, 0x54};

#ifndef unix 
#ifndef ACDLL
int DECLARE dUdftodk(
#else
int DECLARE DdUdftodk(DBGDAT_PTR dbgptr,
#endif
        /* input */    /*** d_report arrangement: N/A ***/
        CHAR_PTR dfield,

        /* output */
        CHAR_PTR datekey)
#else  /* unix */ 
int DECLARE dUdftodk(
 	dfield, datekey ) 
	CHAR_PTR dfield; 
	CHAR_PTR datekey; 
#endif /* unix */ 

{
int month, day, year;
char buffer[9];

(void) ACmemcpy((CHAR_PTR) buffer, dfield, 8);
*(buffer + 8) = (char) 0;
if (!ACstrcmp((CHAR_PTR) buffer, (CHAR_PTR) "        "))
{				       /* 01234567 */
    (void) ACmemcpy(datekey, (CHAR_PTR) blankdate, 8);
    return(SUCCESS);
}

day = ACatoi(buffer + 6);
*(buffer + 6) = (char) 0;
month = ACatoi(buffer + 4);
*(buffer + 4) = (char) 0;
year = ACatoi(buffer);

#ifndef ACDLL
return(dU3itodk(
#else
return(DdU3itodk(dbgptr,
#endif
	month, day, year, datekey));
} /* end of dUdftodk() */
