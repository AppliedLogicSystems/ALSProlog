/**************
 * _dzisort.c *
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
#include "dzinclud.h"

void _DECLARE qmemsort(
#ifndef unix 
PSORTGDAT sgp, unsigned int nelem, int esize,
			CHAR_PTR sorted)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;
void _DECLARE crheapi(
#ifndef unix 
PSORTGDAT sgp, unsigned int next, CHAR_PTR newsortelem)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;
void _DECLARE prheapn(
#ifndef unix 
PSORTGDAT sgp, unsigned int nelem)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;

int _DECLARE isort(
#ifndef unix 
  /* d_report = 10610+ */
	/* inputs */
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	PSORTGDAT sgp,
	DBF  tblfd,
	long reccnt,
	int reclen,
	unsigned int blocks,
	int external,

	/* output */
	LONG_PTR unitsize
		  )
#else  /* unix */ 
	
 	sgp, tblfd, reccnt, reclen, blocks, external, unitsize ) 
	PSORTGDAT sgp; 
	DBF tblfd; 
	long reccnt; 
	int reclen; 
	unsigned int blocks; 
	int external; 
	LONG_PTR unitsize; 
#endif /* unix */ 

        /* returns :  # of sorted blocks */
{
long thissize;
unsigned int  nelem; /* number of elements */
long recno;
int sortblks;
CHAR_PTR cptr;
unsigned int memsize;
unsigned int readlen;
int leafidx; /* index to leafs[] */
int rc;

unsigned debugu;


*unitsize = (long) 0;
thissize = (long) 0;
sortblks = 0;
nelem = 0;

leafidx = 0;
cptr = sortbuf;

memsize = leafs[0].msize;

for (recno = (long) 1; ;recno++)
{
    if ( (thissize + reclen > (long) memsize) || (recno > reccnt) )
    {
	sortblks++;
	qmemsort(sgp, nelem, reclen, leafs[leafidx].leafmem);
        leafs[leafidx].items = nelem;

        if (blocks == 1 && (recno > reccnt) )
        {
            /* no need for merge */
            *unitsize = thissize;
            break; /* done */
        }

	if (external)
        {
            debugu = _ACwrite(temp1fd, leafs[0].leafmem, (unsigned) thissize);
            if (debugu != (unsigned) thissize)
	    {
#ifdef DEBUG
	        printf(
"\ndZisort.c  temp1fd=%d  thissize=%ld  (unsigned) thissize=%u debugu=%u",
temp1fd, thissize, (unsigned) thissize, debugu);
#endif
		d_report = 10611;
	        return(dIOERR);
	    }
        }
        else
        {   /* internal sort only */

            leafidx++;
            if (leafidx >= K) break; /* done */
            memsize = leafs[leafidx].msize;
        }

	if (!(*unitsize)) *unitsize = thissize;

	if (recno > reccnt) break; /* done */

	cptr = sortbuf;
	thissize = (long) 0;
	nelem = 0;
    }

    if (action == OP0)
    {
        if ( (rc = dDgetrec(tblfd, recno, cptr)) != SUCCESS)
        {
	    return(rc);
        }
	cptr += reclen;
	thissize += reclen;
	nelem++;
    }
    else
    {
        /* opcode == OP1 or OP2 */
        readlen = _ACread(((PTBLFILE) tblfd)->tblcdata.handle, cptr, reclen);
	if (!readlen)
        {
#ifdef DEBUG
	    printf("\ndIOERR in isort(): reclen=%d  readlen=%d",
	    	    reclen, readlen);
#endif
	    d_report = 10612;
	    return(dIOERR);
        }
	cptr += readlen;
	thissize += readlen;
	nelem += readlen / reclen;
    }

} /* end of for loop */

return(sortblks);

} /* end of isort() */


void _DECLARE crheapi( sgp, next, newsortelem)
        /* inputs */
	PSORTGDAT sgp;
        register unsigned int next;  /* next spot in heap */
        CHAR_PTR newsortelem;  /* new sort element */

        /* output : none */
{
register unsigned int father;
CHAR_PTR temp;

father = (next >> 1) & 0x7fff;
sortptr[next] = newsortelem;

while (next != 1 &&
	(*asyscmp)
	(sgp, (UCHAR_PTR) sortptr[father], (UCHAR_PTR) sortptr[next]) < 0)
{
    /* interchange father and son */
    temp = sortptr[father];
    sortptr[father] = sortptr[next];
    sortptr[next] = temp;

    /* advance up tree */
    next = father;
    father = (next >> 1) & 0x7fff;
} /* end of while-loop */
} /* end of crheapi() */


void _DECLARE prheapn(
#ifndef unix 
PSORTGDAT sgp, unsigned int nelem)
#else  /* unix */ 
	
 	sgp, nelem ) 
	PSORTGDAT sgp; 
	unsigned int nelem; 
#endif /* unix */ 

{
register unsigned int last;
CHAR_PTR waslastkey;


for (last = nelem; last >= 2; last--)
{
    /* move root element down to last place */
    waslastkey = sortptr[last];
    sortptr[last] = sortptr[1];

    { /*** 1:  BEGIN ***/
    /* adjust tree to heap of size (last - 1) */
    register unsigned int father = 1;
    register unsigned int son;

    /* find larger of root's sons */
    if ( last >= 4 &&
    	 (*asyscmp)(sgp, (UCHAR_PTR) sortptr[3], (UCHAR_PTR) sortptr[2]) > 0)
        son = 3;
    else
        son = 2;

    /* move element upward until find place */
    /* for saved waslastkey */
    while (   (son < last)
           && (*asyscmp)
	   	(sgp, (UCHAR_PTR) sortptr[son], (UCHAR_PTR) waslastkey) > 0)
    {
        sortptr[father] = sortptr[son];
        father = son;
        son = father << 1;

        /* find larger of father's son */
        if ( (son + 2) <= last &&
             (*asyscmp) (sgp, (UCHAR_PTR) sortptr[son + 1],
	     		      (UCHAR_PTR) sortptr[son]) > 0)
                son++;
    } /* end of while-loop */

    sortptr[father] = waslastkey;

    } /*** 1: END ***/

} /* end of for-loop */
} /* end of prheapn() */


static void _DECLARE qmemsort( sgp, nelem, esize, sorted)
        /* inputs */
	PSORTGDAT sgp;
        unsigned int nelem; /* number of elements */
        int esize;          /* size of each element in bytes */

        /* output */
        CHAR_PTR sorted;       /* sorted chunk of  memory */
{
register unsigned int uu;
CHAR_PTR buf;

/* set up first root */
sortptr[1] = sortbuf;

/* put rest in heap */
buf = sortbuf + esize;
for (uu = 2; uu <= nelem; uu++)
{
  crheapi(sgp, uu, buf);
  buf += esize;
} /* end of for-loop */

prheapn(sgp, nelem);

for (uu = 1; uu <= nelem; uu++)
{
    ACmemcpy(sorted, sortptr[uu], esize);
    sorted += esize;
}

} /* end of qmemsort() */
