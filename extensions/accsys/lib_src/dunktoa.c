/*************
 * dUnktoa.c *
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
#include "db4.h"
#include "d4def.h"

#ifndef unix 
#ifndef ACDLL
int DECLARE dUnktoa(
#else
int DECLARE DdUnktoa(DBGDAT_PTR dbgptr,
#endif
        /* inputs */       /*** d_report arrangement: N/A ***/
	int   style,
        CHAR_PTR key, /* ptr to numeric/float key buffer */
	int   width,
	int   decimal,

	/* output */
	CHAR_PTR string)
#else  /* unix */ 
int DECLARE dUnktoa(
 	style, key, width, decimal, string ) 
	int style; 
	CHAR_PTR key; 
	int width; 
	int decimal; 
	CHAR_PTR string; 
#endif /* unix */ 

{
double number;
int rc;

number = dUnktod(style, key);

#ifndef ACDLL
rc = dUdtonf(
#else
rc = DdUdtonf(dbgptr,
#endif
	number, width, decimal, string);
if (rc != SUCCESS) return(rc);

*(string + width) = (char) 0; /* terminate with NULL */
return(SUCCESS);
} /* end of dUnktoa() */
