/*=================================================================*
 |			cmp.c
 |		Copyright (c) 1988-1995 Applied Logic Systems, Inc.
 |
 |			-- support routines for compare/3
 |
 |		Routines:
 |
 |		cmp_gettokstring(tok)
 |		cmp_int_double(i,da)
 |		cmp_double_double(a1,a2)
 |
 | Author: Kevin A. Buettner
 | Creation: 2/29/88
 | Revision History:
 | 11/16/94	C. Houpt	Added pointer type pragmas for MetroWerks.
 *=================================================================*/

#include "defs.h"

extern	UCHAR *	cmp_gettokstring	PARAMS(( long ));
extern	int	cmp_int_double		PARAMS(( int, long ));
extern	int	cmp_double_double	PARAMS(( long, long ));

/*-----------------------------------------------------------------------*
 | cmp_gettokstring is called by the code which implements 
 | compare/3 to get the string corresponding to a token.  The entire 
 | prolog word representing the token (complete with tag and arity) 
 | is passed in.  These are stripped off and the TOKNAME macro is used 
 | to return the string.
 *-----------------------------------------------------------------------*/

#ifdef POINTERS_IN_A0
#pragma pointers_in_D0
#endif

UCHAR *
cmp_gettokstring(tok)
    long tok;
{
    return TOKNAME(MFUNCTOR_TOKID(tok));
}
#ifdef POINTERS_IN_A0
#pragma pointers_in_A0
#endif

#define DBLARG(d,p,a) ((short *)&(d))[(a)-1] = (short) ((*(((long *) (p))+a))>>4)

/*-----------------------------------------------------------------------------*
 | cmp_int_double is called to compare a Prolog integer and a Prolog double.
 *-----------------------------------------------------------------------------*/

int
cmp_int_double(i,da)
    int i;
    long da;
{
    double d;

    da -= MTP_STRUCT;		/* kill the structure tag */

    DBLARG(d,da,1);
    DBLARG(d,da,2);
    DBLARG(d,da,3);
    DBLARG(d,da,4);

    i >>= 4;
    if (((double) i) < d)
	return -1;
    else if (((double) i) == d)
	return 0;
    else
	return 1;
}

/*-----------------------------------------------------------------------------*
 | cmp_double_double is called to compare two doubles.  -1, 0, or 1 is returned
 |	to indicate the result
 *-----------------------------------------------------------------------------*/

int
cmp_double_double(a1,a2)
    long a1, a2;
{
    double d1, d2;

    a1 -= MTP_STRUCT;
    a2 -= MTP_STRUCT;

    DBLARG(d1,a1,1);
    DBLARG(d1,a1,2);
    DBLARG(d1,a1,3);
    DBLARG(d1,a1,4);

    DBLARG(d2,a2,1);
    DBLARG(d2,a2,2);
    DBLARG(d2,a2,3);
    DBLARG(d2,a2,4);

    if (d1 < d2) 
	return -1;
    else if (d1 == d2)
	return 0;
    else
	return 1;
}
