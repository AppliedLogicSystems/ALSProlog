/**************
 * dU3itodf.c *
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifndef unix 
#ifndef ACDLL
int DECLARE dU3itodf(
#else
int DECLARE DdU3itodf(DBGDAT_PTR dbgptr,
#endif
        /* inputs */    /*** d_report arrangement: 4000 ***/
        int month,
        int day,
        int year,

        /* output */
        CHAR_PTR dfield)
#else  /* unix */ 
int DECLARE dU3itodf(
 	month, day, year, dfield ) 
	int month; 
	int day; 
	int year; 
	CHAR_PTR dfield; 
#endif /* unix */ 

{
static char checktbl[] = {0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
int leapyear;
#ifndef _WINDOWS
char tempbuf[9];
#endif

if (day == 9999 && month == 9999 && year == 9999)
{
	ACmemset(dfield, ' ', 8);
	return (SUCCESS);
}

leapyear = dUleap(year);

if (day <= 0 || month <= 0 || year <= 0) goto error;
if (month > 12 || month < 1) goto error;
if (month == 2)
{
    if (day > 29) goto error;
    if (day == 29 && !leapyear) goto error;
}
else
{
    if (day > checktbl[month]) goto error;
}

#ifndef _WINDOWS
sprintf(tempbuf, "%04d", year);
sprintf(tempbuf + 4, "%02d", month);
sprintf(tempbuf + 6, "%02d", day);
ACmemcpy(dfield, tempbuf, 8);
#else
wsprintf(_dWINbuf, "%04d", year);
wsprintf(_dWINbuf + 4, "%02d", month);
wsprintf(_dWINbuf + 6, "%02d", day);
ACmemcpy(dfield, _dWINbuf, 8);
#endif

return (SUCCESS);

error:
    d_report = 4001;
    return(dOUTRANGE);

} /* end of dU3itodf() */
