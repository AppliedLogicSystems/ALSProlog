/**************
 * _dmemcmp.c *
 **************/

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
 
		/* d_report arrangement: N/A */
#include "db4.h"
#include <string.h>
#include "d4def.h"

#ifndef unix
short _DECLARE _diszero(const CHAR_PTR numkey);
#else 	/* unix */
short _DECLARE _diszero();
#define 	const 
#endif 	/* unix */


/* This file contains comparator functions.
    (1) CHARACTER type
    ascending   : memcmp   - prototyped in string.h (default)
    descending	: _ddescmp - defined in this file

    (2) NUMERIC and DATE types
    ascending   : _dncmp    - defined in this file
    descending	: _ddesncm  - defined in this file
*/

#ifdef _WINDOWS
/* comparator for ascending order for Windows (for keycmp ptr. in INDEX) */
int _DECLARE ACmemCMP(
	const VOID_PTR  left,
	const VOID_PTR  right,
	size_t		len)
{
    int rc;

    rc = ACmemcmp(left, right, len);
    return(rc);
}
#endif

/* comparator for descending order */
int _DECLARE _ddescmp(left,right,len)
	const VOID_PTR  left;
	const VOID_PTR  right;
	unsigned int	len;
{
    return(ACmemcmp(right, left, len));
}

/* comparator for NUMERIC/DATE key ascending order */
int _DECLARE _dncmp(left,right,len)
	const VOID_PTR  left;
	const VOID_PTR  right;
	unsigned int	len; /* ignore this */
{
    if ( *((DOUBLE_PTR) left) > *((DOUBLE_PTR) right) ) return(1);
    if ( *((DOUBLE_PTR) left) == *((DOUBLE_PTR) right) ) return(0);
    return(-1);
}

/* comparator for NUMERIC/DATE key descending order */
int _DECLARE _ddesncm(left,right,len)
	const VOID_PTR left;
	const VOID_PTR right;
	unsigned int   len; /* ignore this */
{
    if ( *((DOUBLE_PTR) left) < *((DOUBLE_PTR) right) ) return(1);
    if ( *((DOUBLE_PTR) left) == *((DOUBLE_PTR) right) ) return(0);
    return(-1);
}

/* comparator for NUMERIC/FLOAT key ascending order */
int _DECLARE _dfcmp(left,right,len)
	CHAR_PTR left;
	CHAR_PTR right;
	unsigned int len;
{
    short leftneg, rightneg;
    short leftzero, rightzero;

    leftneg  = (short) *(left + 1) & 0x080;
    rightneg = (short) *(right + 1) & 0x080;

    if (!leftneg && rightneg) return(1);
    if (leftneg && !rightneg) return(-1);

    if (!leftneg && !rightneg)
    {
        leftzero  = _diszero((CHAR_PTR) left);
	rightzero = _diszero((CHAR_PTR) right);
        if (leftzero)
	{
	    if (rightzero) return(0);
	    return(-1);
	}
        if (rightzero) return(1);
	if (*left < *right) return(-1);
	if (*left > *right) return(1);
	return(ACmemcmp(left + 2, right+ 2, len - 2));
    }

    if (*left > *right) return(-1);
    if (*left < *right) return(1);
    return(ACmemcmp(right + 2, left+ 2, len - 2));
}

/* comparator for NUMERIC/FLOAT key descending order */
int _DECLARE _ddesfcm(left,right,len)
	const VOID_PTR left;
	const VOID_PTR right;
	unsigned int len; /* ignore this */
{
	return(_dfcmp((CHAR_PTR) right, (CHAR_PTR) left, len));
}

/* checks if a NUMERIC/FLOAT key is a zero */
static short _DECLARE _diszero(numkey)
	const CHAR_PTR numkey;
{
    return((*numkey == 0x34 && *(numkey + 1) == 1) ? 1 : 0);
}
