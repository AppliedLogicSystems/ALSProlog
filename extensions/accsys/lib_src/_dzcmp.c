/************
 * _dZcmp.c *
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
#include <string.h>
#include "db4.h"
#include "d4def.h"
#include "dzinclud.h"

#ifndef unix 

short _DECLARE _diszero(CHAR_PTR numkey);

int _DECLARE morecmp(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);
int _DECLARE finalcmp(UCHAR_PTR left, UCHAR_PTR right);

/* ptr to all comparators */
int (_DECLARE *asyscmp)(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right);

#else 	/* unix */

short _DECLARE _diszero();
int _DECLARE morecmp();
int _DECLARE finalcmp();
int (_DECLARE *asyscmp)();

#endif 	/* unix */



/* OP0, Ascending, Case-sensitive  -- most normal cases */
int _DECLARE A0Ccmp(sgp, left, right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
register int comprc;

comprc = ACmemcmp(left + compofst, right + compofst, complen);
if (comprc) return(comprc);
return(morecmp(sgp, left, right));
} /* end of A0Ccmp() */

/* OP0, Descending, Case-sensitive */
int _DECLARE D0Ccmp(sgp, left, right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
register int comprc;

comprc = ACmemcmp(right + compofst, left + compofst, complen);
if (comprc) return(comprc);
return(morecmp(sgp, left, right));
} /* end of D0Ccmp() */

/* OP0, Ascending, Ignore-case */
int _DECLARE A0Icmp(sgp, left, right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
register int comprc;

comprc = ACmemicmp(left + compofst, right + compofst, complen);
if (comprc) return(comprc);
return(morecmp(sgp, left, right));
} /* end of A0Icmp() */

/* OP0, Descending, Ignore-case */
int _DECLARE D0Icmp(sgp, left, right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
register int comprc;

comprc = ACmemicmp(right + compofst, left + compofst, complen);
if (comprc) return(comprc);
return(morecmp(sgp, left, right));
} /* end of D0Icmp() */

/* OP0: for subsequent fields */
static int _DECLARE morecmp(sgp, left, right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
UCHAR_PTR arg1, arg2;
CHAR_PTR sqfy; /* copy of next sort qualifier */
register int comprc;
char nxtorder; /* next key order: Ascending or Descending */
int nxtofst;   /* offset to next key */
int nxtlen;    /* length of next key */
int nokeys;    /* copy of global 'keys' */
int nxtktype;  /* next key type (dummy var.) */

nokeys = keys;
sqfy = nextsptr;

while(--nokeys)
{
    nxtorder = *sqfy++;
    nxtktype = *sqfy++;
    nxtofst = (int) getqfnum(sqfy, (pCHAR_PTR) &sqfy);
    nxtlen  = (int) getqfnum(sqfy, (pCHAR_PTR) &sqfy);

    if (nxtktype == 'I')
    {
	if (nxtorder == 'A')
	{
            arg1 = left;
            arg2 = right;
	}
	else
	{
            arg1 = right;
            arg2 = left;
	}
	comprc = ACmemicmp(arg1 + nxtofst, arg2 + nxtofst, nxtlen);
    }
    else
    {	/* C, N, T and others */
	if (nxtorder == 'A')
	{
            arg1 = left;
            arg2 = right;
	}
	else
	{
            arg1 = right;
            arg2 = left;
	}
	comprc = ACmemcmp(arg1 + nxtofst, arg2 + nxtofst, nxtlen);
    }
    if (comprc) return(comprc);
}

return(comprc);
} /* end of morecmp() */


/****** Compartors for OP1 and OP2 *******/
		/*!!!  compofst == 4 !!!*/
	
/* Ascending, Non-unique, Character-key  -- most normal cases */
int _DECLARE A12cmp(sgp, left, right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
register int rc;

rc = ACmemcmp(left + 4, right + 4, complen);
if (rc) return(rc);
return(finalcmp(left, right));
} /* end of A12cmp() */

/* Descending, Non-unique, Character-key */
int _DECLARE D1cmp(sgp, left, right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
register int rc;

rc = ACmemcmp(right + 4, left + 4, complen);
if (rc) return(rc);
return(finalcmp(right, left));
} /* end of D1cmp() */

/* Ascending, Unique, Character-key */
int _DECLARE UA12cmp(sgp, left, right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
return(ACmemcmp(left + 4, right + 4, complen));
} /* end of A12cmp() */

/* Descending, Unique, Character-key */
int _DECLARE UD1cmp(sgp,left,right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
return(ACmemcmp(right + 4, left + 4, complen));
} /* end of D1cmp() */

/* Ascending, Non-unique, Numeric/Date-key */
		/*!!! complen == 8 !!!*/
int _DECLARE A12Ncmp(sgp,left,right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
    if ( *((DOUBLE_PTR) (left + 4)) > *((DOUBLE_PTR) (right + 4)) ) return(1);
    if ( *((DOUBLE_PTR) (left + 4)) < *((DOUBLE_PTR) (right + 4)) ) return(-1);
    return(finalcmp(left, right));
} /* end of A12Ncmp() */

/* Descending, Non-unique, Date-key */
int _DECLARE D1Tcmp(sgp,left,right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
    if ( *((DOUBLE_PTR) (left + 4)) < *((DOUBLE_PTR) (right + 4)) ) return(1);
    if ( *((DOUBLE_PTR) (left + 4)) > *((DOUBLE_PTR) (right + 4)) ) return(-1);
    return(finalcmp(right, left));
} /* end of D1Tcmp() */

/* Ascending, Unique, Numeric/Date-key */
int _DECLARE UA12Ncmp(sgp,left,right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
    if ( *((DOUBLE_PTR) (left + 4)) > *((DOUBLE_PTR) (right + 4)) ) return(1);
    if ( *((DOUBLE_PTR) (left + 4)) == *((DOUBLE_PTR) (right + 4)) ) return(0);
    return(-1);
} /* end of UA12Ncmp() */

/* Descending, Unique, Numeric/Date-key */
int _DECLARE UD1Tcmp(sgp,left,right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
    if ( *((DOUBLE_PTR) (left + 4)) < *((DOUBLE_PTR) (right + 4)) ) return(1);
    if ( *((DOUBLE_PTR) (left + 4)) == *((DOUBLE_PTR) (right + 4)) ) return(0);
    return(-1);
} /* end of UD1Tcmp() */

/* Ascending, Non-unique, Numeric/Float */
		/*!!! complen == 8 !!!*/
int _DECLARE A1Ncmp(sgp,left,right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
    int rc;
    short leftneg, rightneg;
    short leftzero, rightzero;
    UCHAR_PTR lll, rrr;

    lll = left + 4;
    rrr = right + 4;

    leftneg = (short) *(lll + 1) & 0x080;
    rightneg = (short) *(rrr + 1) & 0x080;

    if (!leftneg && rightneg) return(1);
    if (leftneg && !rightneg) return(-1);

    if (!leftneg && !rightneg)
    {
    	leftzero  = _diszero((CHAR_PTR) lll);
	rightzero = _diszero((CHAR_PTR) rrr);
        if (leftzero) 
	{	if (rightzero) return(finalcmp(left, right));
		return(-1);
	}
        if (rightzero) return(1);
	if (*lll < *rrr) return(-1);
	if (*lll > *rrr) return(1);
	rc = ACmemcmp(lll + 2, rrr+ 2, complen - 2);
	if (rc) return(rc);
	return(finalcmp(left, right));
    }

    if (*lll > *rrr) return(-1);
    if (*lll < *rrr) return(1);
    rc = ACmemcmp(rrr + 2, lll+ 2, complen - 2);
    if (rc) return(rc);
    return(finalcmp(right, left));

} /* end of A1Ncmp() */

/* Descending, Non-unique, Numeric/Float-key */
int _DECLARE D1Ncmp(sgp,left,right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
    return(A1Ncmp(sgp, right, left));
} /* end of D1Ncmp() */

/* Ascending, Unique, Numeric/Float-key */
int _DECLARE UA1Ncmp(sgp,left,right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
    short leftneg, rightneg;
    short leftzero, rightzero;

    left += 4;
    right += 4;

    leftneg  = (short) *(left + 1) & 0x080;
    rightneg = (short) *(right + 1) & 0x080;

    if (!leftneg && rightneg) return(1);
    if (leftneg && !rightneg) return(-1);

    if (!leftneg && !rightneg)
    {
    	leftzero  = _diszero((CHAR_PTR) left);
	rightzero = _diszero((CHAR_PTR) right);
        if (leftzero) 
	{	if (rightzero) return(0);
		return(-1);
	}
        if (rightzero) return(1);
	if (*left < *right) return(-1);
	if (*left > *right) return(1);
	return(ACmemcmp(left + 2, right+ 2, complen - 2));
    }

    if (*left > *right) return(-1);
    if (*left < *right) return(1);
    return(ACmemcmp(right + 2, left+ 2, complen - 2));

} /* end of UA1Ncmp() */

/* Descending, Unique, Numeric/Float-key */
int _DECLARE UD1Ncmp(sgp,left,right)
	PSORTGDAT sgp; UCHAR_PTR left; UCHAR_PTR right;
{
    return(UA1Ncmp(sgp, right, left));
} /* end of UD1Ncmp() */

static int _DECLARE finalcmp(left,right)
	UCHAR_PTR left; UCHAR_PTR right;
{
    register int rc;

    left += 3;
    right += 3;
    rc = *(left--) - *(right--);
    if (rc) return(rc);
    rc = *(left--) - *(right--);
    if (rc) return(rc);
    rc = *(left--) - *(right--);
    if (rc) return(rc);
    rc = *left - *right;
    return(rc);
} /* end of finalcmp() */

/* checks if a NUMERIC/FLOAT key is a zero */
static short _DECLARE _diszero(numkey)
	CHAR_PTR numkey;
{
    return((*numkey == 0x34 && *(numkey + 1) == 1) ? 1 : 0);
}

#ifndef unix
int (_DECLARE *op1cmp[TOTALid]) 
	(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right) =
	/*  C       N       F        T  <-- Ascending */
	{A12cmp, A1Ncmp, A1Ncmp, A12Ncmp,
	/*  C       N       F        T  <-- Descending */
	D1cmp,  D1Ncmp, D1Ncmp, D1Tcmp};

int (_DECLARE *Uop1cmp[TOTALid])
	(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right) =
	/*  C        N        F        T  <-- Ascending, UNIQUE */
	{UA12cmp, UA1Ncmp, UA1Ncmp, UA12Ncmp,
	/*  C       N       F          T  <-- Descending, UNIQUE */
	UD1cmp,   UD1Ncmp, UD1Ncmp, UD1Tcmp};

int (_DECLARE *op2cmp[TOTALid])
	(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right) =
	/*  C       N        F        T  <-- Ascending */
	{A12cmp, A12Ncmp, A12Ncmp, A12Ncmp,
	/*  C       N       F        T  <-- Descending */
	    0,      0,      0,       0};

int (_DECLARE *Uop2cmp[TOTALid])
	(PSORTGDAT sgp, UCHAR_PTR left, UCHAR_PTR right) =
	/*  C       N        F        T  <-- Ascending */
	{UA12cmp, UA12Ncmp, UA12Ncmp, UA12Ncmp,
	/*  C       N       F        T   <-- Descending */
	    0,      0,      0,       0};

#else 	/* unix */

int (_DECLARE *op1cmp[TOTALid]) 
	=
	/*  C       N       F        T  <-- Ascending */
	{A12cmp, A1Ncmp, A1Ncmp, A12Ncmp,
	/*  C       N       F        T  <-- Descending */
	D1cmp,  D1Ncmp, D1Ncmp, D1Tcmp};

int (_DECLARE *Uop1cmp[TOTALid])
	=
	/*  C        N        F        T  <-- Ascending, UNIQUE */
	{UA12cmp, UA1Ncmp, UA1Ncmp, UA12Ncmp,
	/*  C       N       F          T  <-- Descending, UNIQUE */
	UD1cmp,   UD1Ncmp, UD1Ncmp, UD1Tcmp};

int (_DECLARE *op2cmp[TOTALid])
	=
	/*  C       N        F        T  <-- Ascending */
	{A12cmp, A12Ncmp, A12Ncmp, A12Ncmp,
	/*  C       N       F        T  <-- Descending */
	    0,      0,      0,       0};

int (_DECLARE *Uop2cmp[TOTALid])
	=
	/*  C       N        F        T  <-- Ascending */
	{UA12cmp, UA12Ncmp, UA12Ncmp, UA12Ncmp,
	/*  C       N       F        T   <-- Descending */
	    0,      0,      0,       0};

#endif 	/* unix */
