/*************
 * _dZmain.c *
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
 

/****************************************************************
 * _dZmain(from-file, to-file, opcode, sort-qualifier program)  *
 ****************************************************************/
/*** sort-qualifier format ***/
/* when opcode == OP0 (DBF sorting)
 * nnn,[A|D][C|I|N|T]bbbb,llll,[A|D][C|M|N|T]bbbb,llll,..........
 *    nnn  -- # of key fields
 *    A    -- Ascending order
 *    D    -- Descending order
 *    C    -- Make distinction between lower and upper case ('C' field type)
 *    I    -- No distinction between lower and upper case ('C' field type)
 *    N    -- 'N' field type
 *    T    -- 'D' field type
 *    bbbb -- beginning column of the sort field
 *    llll -- length for the field (useful only in 'C' and 'M' key types)
 * 
 * example 1)
 *     1AC1,25
 * example 2)
 *     3AM1,10,DC27,35,AN11,8
 *
 * when opcode == OP1 (MDX indexing)
 * nnn,[A|D][C|I|N|F|T]llll,[U|D],tagname
 *    nnn  -- # of keys
 *    A    -- Ascending order
 *    D    -- Descending order
 *    C    -- Make distinction between lower and upper case ('C' field type)
 *    I    -- No distinction between lower and upper case ('C' field type)
 *	      (unused)
 *    N    -- 'N' field type
 *    F    -- 'F' field type
 *    T    -- 'D' field type
 *    llll -- key length for the key
 *    U  --  Unique Keys
 *    D  --  Duplicate keys allowed
 *    tagname - tag name
 *    assumption:
 *		 system key length == llll + 4
 *	         beginning column number of key == 4
 *
 *	<-  4 bytes  ->
 *	---------------------------------------------------------
 *	|record number|		key				|
 *	---------------------------------------------------------
 *	<-------------- system key ----------------------------->
 *
 * example 1)
 *     1,AC25,mytag
 *
.  * when opcode == OP2 (NDX indexing)
 * nnn,[A|D][C|I|N|T]llll,[U|D]
 *    nnn  -- # of keys
 *    A    -- Ascending order
 *    D    -- Descending order -- Not allowed for NDX
 *    C    -- Make distinction between lower and upper case ('C' field type)
 *    I    -- No distinction between lower and upper case ('C' field type)
 *	      (unused)
 *    N    -- 'N' field type
 *    F    -- 'F' field type
 *    T    -- 'D' field type
 *    llll -- key length for the key
 *    U  --  Unique Keys
 *    D  --  Duplicate keys allowed
 *    This implies:
 *         system key length == llll + 4
 *         beginning column number of key == 4
 * 
 *	<-  4 bytes  ->
 *	---------------------------------------------------------
 *	|record number|		key				|
 *	---------------------------------------------------------
 *	<-------------- system key ----------------------------->
 *
 * example 1)
 *     1000,DN8
 *
 */

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include "db4.h"
#include "d4def.h"

#ifdef unix
#include <sys/types.h>
#include <unistd.h>
#endif 	/* unix */

#ifdef STANDARD
#include <sys\types.h>
#include <share.h>
#include <sys\stat.h>
#include <io.h>
#else
long _DECLARE filelength(
#ifndef unix 
int handle)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;
#endif
#include "dzinclud.h"

int _DECLARE _dZmain(
#ifndef unix 
	/* d_report: 10600+ */
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	CHAR_PTR origfile, CHAR_PTR nextfile, int opcode,
	CHAR_PTR squalify)
#else  /* unix */ 
	
 	origfile, nextfile, opcode, squalify ) 
	CHAR_PTR origfile; 
	CHAR_PTR nextfile; 
	int opcode; 
	CHAR_PTR squalify; 
#endif /* unix */ 

{
PSORTGDAT sgp;
int i;
long unitsize;
long lll; /* general purpose long integer */
long flen; /* file length */
unsigned int blocks;
CHAR_PTR ptr;
int external; /* 0: internal sort  1: external sort */
int samename; /* 0: argv[1] != argv[3]   1: argv[1] == argv[3] */

#ifdef DEBUG
printf("\norigfile=%s", origfile);
printf("\nnextfile:%s", nextfile);
printf("\nopcode=%d", opcode);
printf("\nsqualify->%s", squalify);
#endif

sgp = (PSORTGDAT) _ACalloc((unsigned long) SZSORTGDAT);
if (!sgp)
{
    d_report = 10601;
    return(dMEMERR);
}
ACmemset( (CHAR_PTR) sgp, 0, SZSORTGDAT);
action = opcode;
ACstrcpy(origtemp, origfile);

uniq = 0;
uniquniq = 0;

(void) _dtmpnm(
#ifdef ACDLL
		dbgptr,
#endif
		(CHAR_PTR) temp1nm);
(void) _dtmpnm(
#ifdef ACDLL
		dbgptr,
#endif
		(CHAR_PTR) temp2nm);

if (opcode == OP0)
{
    if (ACstrcmp(origfile, nextfile))
    {
        samename = 0; /* origfile != nextfile */
    }
    else
    {
        samename = 1;  /* origfile == nextfile */
        (void) _dtmpnm(
#ifdef ACDLL
			dbgptr,
#endif
			nextfile);
    }

#ifndef ACDLL
    infilep = (UPTR) dDopen(origfile, d_SINGLE, 1);
#else
    infilep = (UPTR) DdDopen(dbgptr, origfile, d_SINGLE, 1);
#endif
    if (!infilep)
    {
        return(badexit(sgp, dretcode));
    }

    flen = filelength( ((DBF) infilep)->handle);

    if ((i = dDreccnt( (DBF) infilep, (LONG_PTR) &tblcount)) != SUCCESS)
    {
        return(badexit(sgp, i));
    }
    if (!tblcount)
    {
#ifndef ACDLL
	if ((i = dDcopy(origfile, nextfile)) != SUCCESS)
#else
	if ((i = DdDcopy(dbgptr, origfile, nextfile)) != SUCCESS)
#endif
	{
        	return(badexit(sgp, i));
	}
	goto cleanup;
    }
    itemlen = dDreclen( (DBF) infilep);

    /* get primary key info for comparison */
    keys = (int) getqfnum(squalify, (pCHAR_PTR) &nextsptr);
    AsDecend = *nextsptr++;
    keytype = *nextsptr++;
    if (keytype == 'I')
    {
	asyscmp = (AsDecend == 'D') ?
		D0Icmp : /* Descending, op0, Ignore-case */
		A0Icmp ; /* Ascending,  op0, Ignore-case */
    }
    else
    {	/* C, N, F, T and others */
	asyscmp = (AsDecend == 'D') ?
		D0Ccmp : /* Descending, op0, Case-sensitive */
		A0Ccmp ; /* Ascending,  op0, Case-sensitive */
    }

    compofst = (int) getqfnum(nextsptr, (pCHAR_PTR) &nextsptr);
    complen  = (int) getqfnum(nextsptr, (pCHAR_PTR) &nextsptr);
}
else
{  /* opcode == OP1 or OP2 */
    indbfd = _dopn(
#ifdef ACDLL
			dbgptr,
#endif
			origfile, d_SINGLE);
    infilep = (UPTR) &indbfd;
    if (indbfd < 0)
    {
	d_report = 10602;
        return(badexit(sgp, dIOERR));
    }
    /* MDX-index or NDX template created by calling program */
    flen = filelength(indbfd);
    keys = 1; /* only one key field is effective in MDX or MDX:
                 later used in dcompare() */

    tblcount = getqfnum(squalify, (pCHAR_PTR) &nextsptr);
    AsDecend = *nextsptr++;
    keytype = *nextsptr++;
    compofst = 4;
    complen =  (int) getqfnum(nextsptr, (pCHAR_PTR) &nextsptr);
    					/* key length */
    if (*nextsptr++ == 'U') uniq++;

    switch(keytype)
    {
    case 'C':
	i = CHARid;
	break;
    case 'N':
    	i = NUMERICid;
	break;
    case 'F':
    	i = FLOATid;
	break;
    case 'T':
    	i = DATEid;
	break;
    default:
	i = CHARid;
    }
    if (AsDecend == 'D') i += DESCENDid;

    if (opcode == OP1)
    {
	if (uniq) 
	    asyscmp = Uop1cmp[i];
	else
	    asyscmp = op1cmp[i];
    }
    else
    {
	if (uniq) 
	    asyscmp = Uop2cmp[i];
	else
	    asyscmp = op2cmp[i];
    }

    nextsptr++;
    /* "nextsptr" points to tagname for OP1 */
    /* "nextsptr" points to end of string for OP2 */
    if (opcode == OP1)
    {
	ACstrcpy(tagnm, nextsptr);
    }

    itemlen = complen;
    complen -= 4; /* complen previously included 'recno' 4-bytes */

#ifdef DEBUG
printf("\ndZmain.c  itemlen=%d  compofst=%d  complen=%d",
			itemlen, compofst, complen);
#endif

}

/* set up initial memory */
#ifdef DEBUG
printf("\ndZmain.c flen = %ld tblcount=%ld opcode=%d", flen, tblcount, opcode);
#endif

if (!tblcount) goto cleanup;

if (getismem(
#ifdef ACDLL
	dbgptr,
#endif
	sgp, flen, tblcount, (INT_PTR) &external, (UINT_PTR) &blocks)) 
{
    d_report = 10603;
    return(badexit(sgp, dMEMERR));
}
#ifdef DEBUG
printf("\ndZmain  getismem OK  external=%d blocks=%d opcode=%d",
			external, blocks, opcode);
#endif

if (external)
{
    temp1fd = _dcre8new(temp1nm, FORCE);
    if (temp1fd < 0)
    {
	d_report = 10604;
        return(badexit(sgp, dIOERR));
    }
    temp2fd = _dcre8new(temp2nm, FORCE);
    if (temp2fd < 0)
    {
	(void) close(temp1fd);
	(void) _ACremove(temp1nm);
	d_report = 10605;
        return(badexit(sgp, dIOERR));
    }
}

#ifdef DEBUG
printf("\ndZmain.c  opened 2 temp. files (if external)  opcode=%d", opcode);
#endif

/* internal sort */
blocks = (unsigned int) isort(
#ifdef ACDLL
		dbgptr,
#endif
		sgp, (DBF) infilep,
	        tblcount, itemlen, blocks, external, (LONG_PTR) &unitsize);
if ((int) blocks < 0) return(badexit(sgp, blocks));

#ifdef DEBUG
printf("\ndZmain.c  after isort()  opcode=%d", opcode);
#endif

_ACfree(sortptr);
if (external) _ACfree(leafs[0].leafmem);
_ACfree(sortbuf);

#ifdef DEBUG
printf("\n_dZmain.c  after freeing memory  opcode=%d  external=%d",
	opcode, external);
#endif

/* no need for input file any longer */
if (opcode == OP0)
{                                        
    (void) dDclose( (DBF) infilep);
}
else
{
    /* opcode == OP2 || opcode == OP3 */
    close(indbfd);
}

#ifdef DEBUG
printf("\ndZmain.c  after closing input file opcode=%d", opcode);
#endif

/* ready to create a sorted or indexed files */
switch(opcode)
{
case OP0:
#ifndef ACDLL
    if ((i = dDcopy(origfile, nextfile)) != SUCCESS)
#else
    if ((i = DdDcopy(dbgptr, origfile, nextfile)) != SUCCESS)
#endif
    {
        return(badexit(sgp, i));
    }
#ifndef ACDLL
    outfilep = (UPTR) dDopen(nextfile, d_SINGLE, 1);
#else
    outfilep = (UPTR) DdDopen(dbgptr, nextfile, d_SINGLE, 1);
#endif
    break;

case OP1:
#ifndef ACDLL
    outfilep = (UPTR) dXopen(nextfile, d_SINGLE, 4);
#else
    outfilep = (UPTR) DdXopen(dbgptr, nextfile, d_SINGLE, 4);
#endif

#ifdef DEBUG
printf("\ndzmain() at dXopen()  nextfile=%s  outfilep=%ld",
	nextfile, outfilep);
#endif
    if (!outfilep) break;
    idx_ptr = dXactidx( (MDX) outfilep, tagnm);
    if (!idx_ptr)
    {
#ifdef DEBUG
	printf("\n!idx_ptr  d_report=%d  tagnm=%s", d_report, tagnm);
#endif
	(void) dXclose( (MDX) outfilep);
    }
    break;

case OP2:
#ifdef DEBUG
	printf("\nOP2: nextfile=%s", nextfile);
#endif
#ifndef ACDLL
    outfilep = (UPTR) dNopen(nextfile, d_SINGLE, 10);
#else
    outfilep = (UPTR) DdNopen(dbgptr, nextfile, d_SINGLE, 10);
#endif
    break;
}

if (!outfilep) 
{
    d_report = 10606;
    return(badexit(sgp, dretcode));
}

/* sort entire file */
if (blocks == 1)
{
    if (opcode != OP0)
    {
        i = initvector(
#ifdef ACDLL
			dbgptr,
#endif
			sgp, ((MDX) outfilep)->handle);
	if (i != SUCCESS) return(badexit(sgp, i));
    }
    for (lll = (long) 1, ptr=leafs[0].leafmem;
         lll <= tblcount;
         lll++, ptr += itemlen)
    {
	if (opcode == OP0)
	{
	    i = dDapprec((DBF) outfilep, ptr);
	    if (i != SUCCESS) return(badexit(sgp, i));
	}
	else
	{
	    i = bldindex(
#ifdef ACDLL
			dbgptr,
#endif
			sgp, ((PMDXFILE) outfilep)->handle, ptr);
	    if (i != SUCCESS) return(badexit(sgp, i));
	}
    }
    if (opcode != OP0)
    {
	i = setheader(
#ifdef ACDLL
		dbgptr,
#endif
		sgp, ((PMDXFILE) outfilep)->handle);
	if (i != SUCCESS) return(badexit(sgp, i));
    }
    _ACfree(leafs[0].leafmem);
}
else
{
    if (external)
    {
	if (opcode != OP0)
	{
            i = initvector(
#ifdef ACDLL
			dbgptr,
#endif
			sgp, ((MDX) outfilep)->handle);
	    if (i != SUCCESS) return(badexit(sgp, i));
	}

#ifdef DEBUG
        printf("\nGoing to getxsmem()  itemlen=%d ...", itemlen);
#endif
	if (getxsmem(
#ifdef ACDLL
			dbgptr,
#endif
			sgp))
        {
	    d_report = 10607;
            return(badexit(sgp, dMEMERR));
        }
    }

    i = dmerge(
#ifdef ACDLL
		dbgptr,
#endif
		sgp, outfilep, tblcount, blocks,
		unitsize, external, leafs[0].msize);
    freeallm(sgp, K-1);
    _ACfree(sortbuf);
    if (i != SUCCESS) return(badexit(sgp, i));
}

#ifdef DEBUG
printf("\ndZmain.c  All sorting/indexing done!\n");
#endif

switch(opcode)
{
case OP0:
    if ((i = dDclose( (DBF) outfilep)) != SUCCESS)
    {
	return(badexit(sgp, i));
    }
    break;

case OP1:
    if ((i = dXclose( (MDX) outfilep)) != SUCCESS)
    {
	return(badexit(sgp, i));
    }
    break;

case OP2:
    if ((i = dNclose( (NDX) outfilep)) != SUCCESS)
    {
	return(badexit(sgp, i));
    }
    break;
}

if (external)
{
    close(temp1fd);
    (void) _ACremove(temp1nm);
}

cleanup:

if (opcode == OP0)
{
    if (samename)
    {
        /* nextfile[] has contained a temporary filename */
        dUexpnm(origfile, "DBF", temp1nm);
        dUexpnm(nextfile, "DBF", temp2nm);
        (void) _ACremove(temp1nm);
        if (_Arename(
#ifdef ACDLL
			dbgptr,
#endif
			(CHAR_PTR) temp2nm, (CHAR_PTR) temp1nm))
        {
	    d_report = 10608;
            return(badexit(sgp, dIOERR));
        }
    }
}
if (opcode != OP0) (void) _ACremove(origfile);
_ACfree((VOID_PTR) sgp);
return(SUCCESS);
} /* end of _dZmain function */

#ifndef STANDARD
static long _DECLARE filelength(
#ifndef unix 
int handle)
#else  /* unix */ 
	handle ) 
	int handle; 
#endif /* unix */ 

{
	long size;
	long here = lseek(handle, (long) 0, SEEK_CUR);
	size = lseek(handle, (long) 0, SEEK_END);
	(void) lseek(handle, here, SEEK_SET);
	return(size);
} /* end of filelength() */
#endif
