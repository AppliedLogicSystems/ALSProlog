/************
* _dindex.c *
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

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include "db4.h"
#include "d4def.h"
#ifdef MSC
#include <sys\types.h>
#endif
#ifdef STANDARD
#include <sys\stat.h>
#include <process.h>
#include <io.h>
#endif
#include "dzinclud.h"

#define BSIZE  12288


#ifdef ACDLL
#undef d_report
#define d_report tblptr->pdbg->d_report
#undef _dWINbuf
#define _dWINbuf tblptr->pdbg->_dWINbuf
#undef _dminbuf
#define _dminbuf tblptr->pdbg->_dminbuf
#endif

int _DECLARE _dindex(dbfptr,tofilenm,tagname,order,type,fldoffset,keylen,
					unique,keygen,condcheck)
			/*** d_report arrangement: 10120 ***/
		/* inputs */
	DBF  dbfptr;     /*** caller will close this DBF ***/
	CHAR_PTR tofilenm;  /* name of MDX to work on */
	CHAR_PTR tagname;  /* tag name */
	int order;
	int type;
	int fldoffset;
	int keylen;
	int  unique;	/* 1: uniq key only  0: duplicate keys allowed */
#ifndef unix 
#ifndef __HIGHC__
#ifndef ACDLL
	int  (aPOINTER keygen)(CHAR_PTR  record, CHAR_PTR key);
			/* pointer to key generator function */
	int  (aPOINTER condcheck)(CHAR_PTR record);
			/* pointer to condition-checker function */
#else
	FARPROC keygen;
	FARPROC condcheck;
#endif
#endif 	/* __HIGHC__ */
#endif 	/* unix */
#ifdef __HIGHC__
	int  (*keygen)();
	int  (*condcheck)();
#endif 	/* __HIGHC__ */
#ifdef unix 
	int  (*keygen)();
	int  (*condcheck)();
#endif 	/* unix */
		/* output: none */
{
	int sqcnt; /* # of char's in qualifier */
	int rc;
	int  reclen;
	int allchars; /* # of all char's in a command line */
	long tblhas, indexes;
	PTBLNOD nodeptr;
	long recno;
	long nodenum;
	int offset;
	CHAR_PTR buffer, bufptr, recptr;
	unsigned int bufofst; /* buffer offset */
	int uniqfd;

#ifndef _WINDOWS
	char uniqnm[ACTMPLEN + 8];
#else
#define uniqnm _dWINbuf
	CHAR_PTR WINbufp;
#endif

	rc = dDreccnt(dbfptr, &tblhas);
	if (rc != SUCCESS) return(rc);
	if (tblhas <= (long) 0) return(SUCCESS);
	if (!_dtmpnm(
#ifdef ACDLL
		tblptr->pdbg,
#endif
		uniqnm))
	{
    	d_report = 10121;
    	return(dIOERR);
	}

#ifndef STANDARD
    (void) remove(uniqnm);
#endif

	buffer = (CHAR_PTR) _ACalloc((unsigned long) BSIZE);

	if (!buffer)
	{
    	d_report = 10123;
    	return(dMEMERR);
	}

	uniqfd = _dcre8new(uniqnm, FORCE);
	if (uniqfd < 0)
	{
    	d_report = 10124;
    	rc = dIOERR; /* creation failed */
    	goto endpt2;
	}

	keylen += 4; /* keylen now includes the record number bytes */
	reclen = dDreclen(dbfptr);
	bufofst = 0;
	indexes = (long) 0;
	for (recno = (long) 1; ;recno++)
	{
    	if (bufofst + keylen > BSIZE || recno > tblhas)
    	{
        	if (_ACwrite(uniqfd, buffer, bufofst) != bufofst)
        	{
            	d_report = 10125;
            	rc = dIOERR;
	    		goto endpt3;
        	}
        	bufofst = 0;
    	}
    	if (recno > tblhas) break; /* done */

    	bufptr = buffer + bufofst;

    	/* Use an AccSys internal function */
    	rc = _dfindbf(dbfptr, recno, ACREAD, (LONG_PTR) &nodenum,
    		(INT_PTR) &offset, (pPTBLNOD) &nodeptr);
    	if (rc != SUCCESS) goto endpt3;

    	offset *= reclen; /* now offset is the actual length in bytes */
    	recptr = nodeptr->nodebuf + offset;

    	/* check the FOR-condition */
    	if (condcheck && !(*condcheck)(recptr)) continue;
			/* if not FOR-condition, ignore this */
    	/* form a key */
    	_bytes4(recno, bufptr);
    	if (fldoffset)
    	{
			switch(type)
			{
				case 'C':
				case 'I':
					(void) ACmemcpy(bufptr + 4, recptr + fldoffset,
			      		keylen - 4);
					break;
				case 'N':
				case 'F':
#ifndef ACDLL
					dUnftonk(
#else
					DdUnftonk(tblptr->pdbg,
#endif
						tagname ? MDXstyle : NDXstyle,
						recptr + fldoffset, keylen - 4,
		    			bufptr + 4);
					break;
				case 'T':
#ifndef ACDLL
					dUdftodk(recptr + fldoffset, bufptr + 4);
#else
					DdUdftodk(tblptr->pdbg, recptr + fldoffset, bufptr + 4);
#endif
					break;
				}
    	}
    	else
    	{
			if ((*keygen)(recptr, bufptr + 4) != SUCCESS)
			{
            	rc = dERROR;
	    		goto endpt3;
			}
    	}
    	/* made it! */
    	indexes++;
    	bufofst += keylen;
	} /* end of for-loop */

	(void) close(uniqfd);
	_ACfree(buffer);

	if (!indexes) return(SUCCESS);

	/* build indexing qualifier */
	allchars = ACstrlen(uniqnm);
	allchars += ACstrlen("\001");
	allchars += ACstrlen(tofilenm);
	/* ACstrcpy(args[4], "1"); */ /* OP1 */
	allchars += 1; /* ACstrlen("2") == 1 */

	/*** set qualifier (args[5]) ***/
	(void) ltoa(indexes, _dminbuf, 10); /* no. of elements */ 
	sqcnt = ACstrlen(_dminbuf);
	allchars += sqcnt;
	*(_dminbuf + sqcnt) = ','; /* comma */
	sqcnt++;
	allchars++;

	*(_dminbuf + sqcnt) = order; /* indexing order */
	sqcnt++;
	allchars++;

	if (type == 'D') type = 'T';

	*(_dminbuf + sqcnt) = type; /* key type */
	sqcnt++;
	allchars++;

	(void) itoa(keylen, _dminbuf + sqcnt, 10); /* key length inc. rec.no. */
	sqcnt += ACstrlen(_dminbuf + sqcnt); /* no space after key length */
	allchars += sqcnt;
	*(_dminbuf + sqcnt) = ','; /* comma */

	*(_dminbuf + sqcnt) = unique ? 'U' : 'D'; /* unique keys ?*/
	sqcnt++;
	allchars++;

	if (tagname)
	{
		*(_dminbuf + sqcnt) = ','; /* comma */
		sqcnt++;
		allchars++;

		ACstrcpy(_dminbuf + sqcnt, tagname);
		rc = ACstrlen(tagname);
		sqcnt += rc;
		allchars += rc;
	}
	*(_dminbuf + sqcnt) = (char) 0; /* terminate the qualifier */

	if (allchars >= 128)
	{
    	(void) _ACremove(uniqnm);
    	d_report = 10122;
    	return(dOUTRANGE);
	}

	rc = _dZmain(
#ifdef ACDLL
		tblptr->pdbg,
#endif
		uniqnm, tofilenm, tagname ? OP1 : OP2, _dminbuf);

	return(rc);

endpt3:
	close(uniqfd);
    (void) _ACremove(uniqnm);

endpt2:
	_ACfree(buffer);

	return (rc); /* error return before reaching _dZmain() */
} /* end of _dindex() */
