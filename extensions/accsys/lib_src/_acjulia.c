/**************
 * _ACjulia.c *
 **************/

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

#include <stdlib.h>
#include "d4def.h"

double _DECLARE _ACjulian(year)   /* d_report arrangement: N/A */
	int year;
{
	long julyear;

	julyear = (long) year * 365 + year/4 - year/100 + year/400;

	return((double) julyear);
} /* end of _ACjulian() */

