/**************
 * _dcalndr.c *
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

int _daystbl[14] =
/* 0, 1  2   3   4    5    6    7    8    9   10   11   12 */
{  0, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 };
/*       1   2   3    4    5    6    7    8    9   10   11   12 */
