/*************
 * dUatonf.c *
 *************/

/*****************************************************************
*                                                                *
* Copyright 1989, 1990, 1991, Billy Shakespeare & Company, Inc.  *
* All rights reserved.                                           *
*                                                                *
******************************************************************
*                                                                *
* Published by                                                   *
*        Copia International, Inc.                               *
*        Wheaton, Illinois                                       *
*        U. S. A.                                                *
*                                                                *
******************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifndef unix 
#ifndef ACDLL
int DECLARE dUatonf(
#else
int DECLARE DdUatonf(DBGDAT_PTR dbgptr,
#endif
			/*** d_report arrangement: 4040 ***/
	/* inputs */
	CHAR_PTR string,
	int  width,
	int  decimal,

	/* output */
        CHAR_PTR nfield)
#else  /* unix */ 
int DECLARE dUatonf(
 	string, width, decimal, nfield ) 
	CHAR_PTR string; 
	int width; 
	int decimal; 
	CHAR_PTR nfield; 
#endif /* unix */ 

{
#define NDIGS 30
char digtbl[NDIGS];
CHAR_PTR sptr, digptr;
int i, decpt, sign;
int digs;

if ( (decimal < 0) || (decimal && width <= (decimal + 1)) )
{
    d_report = 4041;
    return(dOUTRANGE);
}
(void) ACmemset(nfield, ' ', width);
for (sptr=string; (*sptr && *sptr == ' '); sptr++) {};
if (!(*sptr)) return(SUCCESS); /* all blanks */
if (*sptr == '-')
{
    sign = 1;
    sptr++;
}
else
{
    sign = 0;
}

decpt = -1;
digs = 0;
for (digptr=digtbl, i=1; i<=NDIGS; i++)
{
    if (*sptr == '.')
    {
	if (decpt == -1) decpt = i - 1;
	sptr++;
	continue;
    }
    if ('0' <= *sptr && *sptr <= '9')
    {
	digs++;
	*digptr++ = *sptr++;
    }
    else
    {
	*digptr++ = '0';
    }
}
*digptr = (char) 0;
if (decpt == -1) decpt = digs;

_Atonf((CHAR_PTR) digtbl, width, decimal, decpt, sign, nfield);
return(SUCCESS);
} /* end of dUatonf() */

