/*************
* dXexpr.c *
**************/

/************************************************************
*                                                           *
* Copyright 1989 - 1991, Billy Shakespeare & Company, Inc.  *
* All rights reserve d.                                     *
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
 *   dXexpr() copies the index expression of a tag from MDX.
 */
int DECLARE dXexpr(
#ifndef unix 

		/*** d_report arrangement: N/A ***/
	/* inputs */
	MDX      mdxptr,   /* MDX file pointer */
	CHAR_PTR tagname,  /* tag name */

	/* outputs */
	CHAR_PTR iexpr,    /* index expression */
	CHAR_PTR FORcond,  /* FOR condition */
	INT_PTR  unique,   /* 1: unique   0=>non-unique */
	CHAR_PTR order)
#else  /* unix */ 
	
 	mdxptr, tagname, iexpr, FORcond, unique, order ) 
	MDX mdxptr; 
	CHAR_PTR tagname; 
	CHAR_PTR iexpr; 
	CHAR_PTR FORcond; 
	INT_PTR unique; 
	CHAR_PTR order; 
#endif /* unix */ 
    /* 'A'scending or 'D'escending */
{
int dummy;
long ldummy;

return(_dxstag(	mdxptr, (CHAR_PTR) tagname, 0,
		(INT_PTR) &dummy, (LONG_PTR) &ldummy, (UINT_PTR) &dummy,
		(INT_PTR) &dummy, iexpr, FORcond, unique, order));
} /* end of dXexpr() */
