/*************
 * dUnftod.c *
 *************/

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
 
#include <stdlib.h>
#include <string.h>
#include "db4.h"
#include "d4def.h"

double DECLARE dUnftod(
#ifndef unix 

        /* input */       /*** d_report arrangement: N/A ***/
        CHAR_PTR nfield, /* ptr to Numeric/Float field buffer */
	int	 width)
#else  /* unix */ 
	
 	nfield, width ) 
	CHAR_PTR nfield; 
	int width; 
#endif /* unix */ 
  /* field width */
{
    double retval;
#define MINISIZE 40
    char minibuf[MINISIZE];

    if (width >= MINISIZE) return(0L); /* wrong width */
    ACmemcpy((CHAR_PTR) minibuf, nfield, width);
    *(minibuf + width) = (char) 0;
    retval = ACatof((CHAR_PTR) minibuf);
    return(retval);
} /* end of dUnftod() */
