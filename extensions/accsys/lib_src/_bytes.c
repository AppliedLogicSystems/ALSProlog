/************
 * _bytes.c *
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

#include <stdlib.h>
#include "d4def.h"

unsigned short _DECLARE _2bytes(ptr) /* d_report arrangement: N/A */
	CHAR_PTR ptr;
{
	unsigned short retval;

	retval  = (unsigned short) *ptr & 0x00ff;
	ptr++;
	retval |= ((unsigned short) *ptr) << 8;

	return(retval);
} /* end of _2bytes() */


#ifdef __MWERKS__
/* Metrowerks complains about short parameters that aren't declared
   in the strict ANSI C format. */
void _DECLARE _bytes2(unsigned short value,CHAR_PTR ptr)  /* d_report arrangement: N/A */
#else
void _DECLARE _bytes2(value,ptr)  /* d_report arrangement: N/A */
	unsigned short value;
	CHAR_PTR ptr;
#endif
{
	*ptr++ = (char) (value & (char) 0xff);
	*ptr   = (char) ((value >> 8) & (char) 0xff);
} /* end of _bytes2() */
 

long _DECLARE _4bytes(ptr)  /* d_report arrangement: N/A */
	CHAR_PTR ptr;
{
	long retval;

	retval  = (long) *ptr++ & 0xff;
	retval |= ((long) *ptr++ & 0xff) << 8;
	retval |= ((long) *ptr++ & 0xff) << 16;
	retval |= ((long) *ptr & 0xff) << 24;

	return(retval);
} /* end of _4bytes() */


void _DECLARE _bytes4(value,ptr)  /* d_report arrangement: N/A */
	long   value;
	CHAR_PTR ptr;
{
	*ptr++ = (char) (value & (char) 0xff);
	*ptr++ = (char) ((value >> 8) & (char) 0xff);
	*ptr++ = (char) ((value >> 16) & (char) 0xff);
	*ptr   = (char) ((value >> 24) & (char) 0xff);
} /* end of _bytes4() */

