/*************
 * dUtoday.c *
 *************/

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
 

#include <time.h>
#include "db4.h"

#ifdef _WINDOWS
	static time_t today;
#endif

void DECLARE dUtoday(
#ifndef unix 

		/* input: none */

		/* outputs */
	INT_PTR month,
	INT_PTR day,
	INT_PTR year)
#else  /* unix */ 
	
 	month, day, year ) 
	INT_PTR month; 
	INT_PTR day; 
	INT_PTR year; 
#endif /* unix */ 

{
#ifndef _WINDOWS
	time_t today;
#endif
	struct tm *p;

	time(&today);
	p = localtime(&today);
	if (p)
	{
		*year  = p->tm_year + 1900;
		*month = p->tm_mon + 1;
		*day   = p->tm_mday;
	}
	else
	{
		*year  = 1980;
		*month = 1;
		*day   = 1;
	}
} /* end of dUtoday() */
