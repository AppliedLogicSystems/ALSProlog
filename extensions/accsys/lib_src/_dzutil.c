/************
 * dZutil.c *
 ************/

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

#include <stdio.h>
#include "db4.h"
#include "d4def.h"
#include "dzinclud.h"

int _DECLARE badexit(
#ifndef unix 
PSORTGDAT sgp, int badnumber)
#else  /* unix */ 
	
 	sgp, badnumber ) 
	PSORTGDAT sgp; 
	int badnumber; 
#endif /* unix */ 

{
	if (action != OP0) (void) _ACremove(origtemp);
	(void) _ACremove(temp1nm);
	(void) _ACremove(temp2nm);
	_ACfree((VOID_PTR) sgp);
	return(badnumber);
} /* end of badexit() */

/* getqfnum() gets a number from sort-qualifier
   After reading a number, it places the pointer to the next meaningful
   position in the qualifier by skipping commas(').
   No error checking of the qualifier is done.
 */

long _DECLARE getqfnum(
#ifndef unix 

    /* input */
    CHAR_PTR sptr,

    /* output */
    pCHAR_PTR newsptr
    	     )
#else  /* unix */ 
	
 	sptr, newsptr ) 
	CHAR_PTR sptr; 
	pCHAR_PTR newsptr; 
#endif /* unix */ 


    /* returns: number from qualifier */
{
long num;

num = (long) 0;
while ('0' <= *sptr && *sptr <= '9')
    num = (num << 3) + num + num  /* num * 10 */
          + (*sptr++ - '0');
if (*sptr == ',') sptr++;

*newsptr = sptr;

return(num);
} /* end of getqfnum() */
