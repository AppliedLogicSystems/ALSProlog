/************
 * dUleap.c *
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
 
/****************************************************************
 * Gregorian Calendar Definition:                               *
 *      Every fourth year is a leap year except century years.  *
 *      If a century year is divisible by 400, e.g., 2000,      *
 *      the century year is a leap year.                        *
 ****************************************************************/

#include "db4.h"

int DECLARE dUleap(
#ifndef unix 
int year)
#else  /* unix */ 
	year ) 
	int year; 
#endif /* unix */ 
  /*** d_report arrangement: N/A ***/
        /* returns 1 if leap year, 0 otherwise */
{

if (year % 4) return(0);

if (!(year % 400)) return(1);

if (year % 100) return(1);

return(0);

} /* end of dUleap(year) */



