/***********
* _dutil.c *
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

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif

#ifdef unix
#include <sys/types.h>
#include <unistd.h>
#endif 	/* unix */

void _DECLARE _dbasenm(hasdot,basename)
		/*** d_report arrangement: N/A ***/
		/* input */
	CHAR_PTR hasdot; /* file name with a dot */

		/* output */
	CHAR_PTR basename;
{
	int i;
#define BACKSLASH 0x5c
	CHAR_PTR dotptr, cptr, lastslash;

	dotptr = ACstrchr(hasdot, (int) '.');
	if (!dotptr) dotptr = hasdot + ACstrlen(hasdot);
	lastslash = ACstrrchr(hasdot, (int) BACKSLASH);
	cptr = lastslash ? lastslash + 1 : hasdot;
	for (i=1; cptr < dotptr && i <= 8; i++, cptr++)
    	*basename++ = toupper(*cptr);
	*basename = (char) 0;

} /* end of _dbasenm() */

/* seek and write */

int _DECLARE _dseekwt(fn,position,writebuf,writelen)
	int fn;
	long position;
	CHAR_PTR writebuf;
	unsigned writelen;
{
	if (lseek(fn, position, SEEK_SET) == (long) -1)
	{
    	return(dIOERR);
	}
	if (_ACwrite(fn, writebuf, writelen) != writelen)
	{
    	return(dIOERR);
	}
	return(SUCCESS);
} /* end of _dseekwt() */

/* seek and read */

int _DECLARE _dseekrd(fn, position,readbuf,readlen)
	int fn;
	long position;
	CHAR_PTR readbuf;
	unsigned readlen;
{
	if (lseek(fn, position, SEEK_SET) == (long) -1)
	{
    	return(dIOERR);
	}
	if (_ACread(fn, readbuf, (unsigned) readlen) != (unsigned) readlen)
	{
    	return(dIOERR);
	}
	return(SUCCESS);
} /* end of _dseekrd() */


#ifdef _WINDOWS
int _DECLARE ACatoi(stringptr)
	CHAR_PTR stringptr;
{
int result;
int digit;

while (*stringptr && *stringptr == ' ') stringptr++;
if (!stringptr) return(0);

result = 0;
while (1)
{
    digit = *stringptr;
    if (digit < '0' || '9' < digit) break;
    result = result * 10 + digit - '0';
    stringptr++;
}
return(result);
} /* end of ACatoi() */

static char xtoabuf[80];
double _DECLARE ACatof(ascii)
	CHAR_PTR ascii;
{
	ACstrcpy(xtoabuf, ascii);
	return(atof(xtoabuf));
} /* end of ACatof() */

void _DECLARE ACitoa(ivalue,ascii,base)
	int ivalue;
	CHAR_PTR ascii;
	int base;
{
	itoa(ivalue, xtoabuf, base);
	ACstrcpy(ascii, xtoabuf);
} /* end of ACitoa() */

void _DECLARE ACltoa(lvalue,ascii,base)
	long lvalue;
	CHAR_PTR ascii;
	int base;
{
	ltoa(lvalue, xtoabuf, base);
	ACstrcpy(ascii, xtoabuf);
} /* end of ACitoa() */

#endif

#ifndef STANDARD
#ifndef unix
	/* simulate itoa() of other compilers */
void _DECLARE itoa(ivalue,buffer,ignore)
	int ivalue;
	CHAR_PTR buffer;
	int ignore; /* ignore this */
{
	(void) stci_d(buffer, ivalue);
} /* end of itoa() */

/* simulate ltoa() of other compilers */
void _DECLARE ltoa(lvalue,buffer,ignore)
	long lvalue;
	CHAR_PTR buffer;
	int ignore; /* ignore this */
{
	(void) stcl_d(buffer, lvalue);
} /* end of ltoa() */

#endif 	/* unix */
/* simulate memicmp() of other compilers */
int _DECLARE memicmp(left, right, len)
	void *left;
	void *right;
	 size_t len;
{
	char *ll, *rr;
	int rc;

	ll = (char *) left;
	rr = (char *) right;
	while (len--)
	{
		rc = (int) *ll - (int) *rr;
		if (rc) return(rc);
	}
	return(0);
} /* end of memicmp() */
#endif

#ifdef ZORTECH
	/* This stricmp() makes a partial match if one string is
	 * shorter than the other.
	 */
int stricmp(char *str1, char *str2)
{
size_t len1, len2;

len1 = strlen(str1);
len2 = strlen(str2);

return(memicmp(str1, str2, (len1 <= len2) ? len1 : len2));
} /* end of stricmp() */
#endif

