/************
* dNcreat.c *
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

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <sys\types.h>
#include <sys\stat.h>
#include <io.h>
#endif

/*
 *   Example)  dNcreat("INDEX", "LASTNAME+FIRSTNAME", "C40U");
 */
#ifndef ACDLL
int DECLARE dNcreat( ndxname, keyexpr, typelen)
		/*** d_report arrangement: 3020 ***/
#else
int DECLARE DdNcreat( dbgptr, ndxname, keyexpr, typelen)
	DBGDAT_PTR dbgptr;
#endif
	CHAR_PTR ndxname; /* NDX name */
	CHAR_PTR keyexpr; /* key expression  */
	CHAR_PTR typelen; /* type and length */
{
int i;  /* general purpose integer */
int keytype; /* key type */
int keylen; /* key length */
int ndxtype;
unsigned char uniq; /* for NDX unique feature */
CHAR_PTR memory;
char exname[ACTMPLEN];

dUexpnm(ndxname, "NDX", exname);

if (ACstrlen(keyexpr) > MAXKEYEX)
{
    d_report = 3021;    /* key expression too long */
    return(dOUTRANGE);
}

keytype = toupper(*typelen);
typelen++;
switch(keytype)
{
case 'C':
    keylen = ACatoi(typelen);
    if (keylen <= 0 || MAXKEYLEN < keylen)
    {
	d_report = 3022;
	return(dOUTRANGE);
    }
    while (isdigit(*typelen)) typelen++;
    ndxtype = CHARACTER;

    break;

case 'N':
case 'F':
case 'D':
    keylen = NDKEYLEN;
    typelen++;
    ndxtype = NUMDATE;
    break; /* OK */
default:
    d_report = 3023;
    return(dILLEGAL);
}

uniq = toupper(*typelen) == 'U' ? UNIQUE : 0;

memory = (CHAR_PTR) _ACalloc((unsigned long) DNODSIZ);
if (!memory)
{
    d_report = 3024;
    return(dMEMERR);
}

(void) ACmemset(memory, 0, DNODSIZ); /* clear buffer */

*memory = 0x01;
*(memory + EOFNODE) = 0x02;
*(memory + ORDERBYTE) = uniq;
*(memory + EOFNODE2) = 0x02;
*(memory + XDXKTYPE) = (char) keytype;
*(memory + NDXKTYPE) = (char) ndxtype;
_bytes2(keylen, memory + KCOMPLEN);
_bytes2( (NDXNODSZ - 8) / (keylen + 8), memory + MAXKEYS);
_bytes2( keylen + 8, memory + KEYLEN);
ACstrcpy(memory + KEXPRESS, keyexpr);

i = _dcre8new((CHAR_PTR) exname, FORCE);
if (i < 0)
{
	d_report = 3025;
	_ACfree(memory);
	return(dIOERR);    /* file creation by open() failed */
}

/* write the buffer */
if (_ACwrite(i, memory, (unsigned) DNODSIZ) != (unsigned) DNODSIZ)
{
    close(i);
    (void) _ACremove(exname);
    _ACfree(memory);
    d_report = 3026;
    return(dIOERR);
}

close(i);
_ACfree(memory);
return(SUCCESS);

} /* end of dNcreat() */
