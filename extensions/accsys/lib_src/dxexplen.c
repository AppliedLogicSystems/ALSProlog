/*************
* dXexplen.c *
**************/

/************************************************************
*                                                           *
* Copyright 1989 - 1990, Billy Shakespeare & Company, Inc.  *
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

/*
 *   dXexplen() designates the index expression length of a tag in MDX.
 */
int DECLARE dXexplen(
#ifndef unix 

		/*** d_report arrangement: N/A ***/
	/* inputs */
	MDX      mdxptr,     /* MDX file pointer */
	CHAR_PTR tagname,  /* tag name */

	/* outputs */
	INT_PTR  length)
#else  /* unix */ 
	
 	mdxptr, tagname, length ) 
	MDX mdxptr; 
	CHAR_PTR tagname; 
	INT_PTR length; 
#endif /* unix */ 
  /* index expression length */
{
int dummy;
long ldummy;
char cdummy;

return(_dxstag( mdxptr, (CHAR_PTR) tagname, 0,
		(INT_PTR) &dummy, (LONG_PTR) &ldummy, (UINT_PTR) &dummy,
		length, (CHAR_PTR) 0, (CHAR_PTR) 0, (INT_PTR) &dummy,
	(CHAR_PTR) &cdummy));
} /* end of dXexplen() */
