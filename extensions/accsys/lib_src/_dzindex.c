/**************
 * _dzindex.c *
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
#include <fcntl.h>
#include <string.h>
#include <stdlib.h>
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <sys\types.h>
#include <sys\stat.h>
#endif
#include "dzinclud.h"

/* intialize the LEVEL table elements */
/* Intialization differnece between for NDX and for MDX:
	NDX  => vecptr->curnodenum is predetermined by the initvector()
		function. 
	MDX  => vecptr->curnodenum is acquired as needed by the
		bldindex() function. */

int _DECLARE initvector(sgp,handle)
#ifdef ACDLL
			DBGDAT_PTR dbgptr,
#endif
			PSORTGDAT sgp; int handle;
{					/* d_report: 10620+ */
PLEVEL vecptr;
long  blks, nodenumber, elements;
unsigned int nodesize;
int level;
char minibuf[0x30];
int dummy;
char cdummy;
long ll;

if (action == OP1)
{
    level = _dseekrd(handle, (long) 0, (CHAR_PTR) minibuf, 0x30);
    if (level != SUCCESS)
    {
	d_report = 10621;
	return(level);
    }
    initeof = _4bytes(minibuf + MDXENODE);
    freelist.mdxeof = initeof;
    freelist.ghead  = _4bytes(minibuf + MDXGHEAD);
    freelist.gnums  = _4bytes(minibuf + MAVLNODE);
    blksz = (int) _4bytes(minibuf + BLKSIZ);
    freelist.blksiz = blksz;

    skeylen  = itemlen;
    nodesize = (unsigned int) freelist.blksiz << SNODSHIFT;
    maxitems = (nodesize - 12) / skeylen;

    level = _dxstag((MDX) outfilep, tagnm, 1, &dummy, &hdrnode,
    		    (UINT_PTR) &dummy, &dummy, (CHAR_PTR) 0,
		    (CHAR_PTR) 0, (INT_PTR) &dummy, (CHAR_PTR) &cdummy);
    if (level != SUCCESS)
    {
	d_report = 10622;
	return(level);
    }
    ll = (long) hdrnode << SNODSHIFT;
    level = _dseekrd(handle, ll, (CHAR_PTR) minibuf, 0x4);
    if (level != SUCCESS)
    {
	d_report = 10623;
	return(level);
    }
    initnode = _4bytes(minibuf);
}
else
{   /* opcode == OP2 */
    skeylen  = itemlen + 4;
    maxitems = (SNODSIZ - 8) / skeylen;
    nodesize = SNODSIZ;

    hdrnode  = (long) 0;
    nodenumber = (long) 1;
    initnode =   (long) 1;
}
lastnode = (long) 0;
usednodes = (long) 0;

elements = tblcount;
leafblks = ((elements - 1) / maxitems) + 1;

blks = leafblks;
vecptr = vector;
level = 0;
while (1)
{
    level++;

    vecptr->elmleft = elements;
    vecptr->buffer = _ACalloc((unsigned long) nodesize);
    if (!(vecptr->buffer))
    {
	d_report = 10624;
	return(dMEMERR);
    }
    vecptr->nodesz = nodesize;
    vecptr->count = 0;
    if (action == OP1)
    {   /* MDX */
	vecptr->bufptr = vecptr->buffer + 8;
	/* vecptr->bufptr += 4; */
    }
    else
    {
	vecptr->bufptr = vecptr->buffer + 4;
	vecptr->curnodenum = nodenumber;
	nodenumber += blks;
    }
    if (!(vecptr->buffer))
    {
	d_report = 10625;
	return(dMEMERR);
    }
    if (level == 1)
    { /* leaf level */
        if (elements <= maxitems) break; /* done */
    }
    else
    { /* non-leaf level */
        if (elements <= (maxitems + 1)) break; /* done */
    }

    /* setup for initializing non-leaf */
    elements = blks;
    blks = ((blks - 1) / (maxitems + 1)) + 1;
    vecptr++;
}
vanchor = vecptr;

#ifdef DEBUG
for (level=1, vecptr=vector; vecptr <= vanchor; vecptr++, level++)
{
    printf("\nVECTOR level: %d", level);
    printf("\nelmleft=%ld  nodesz=%d  count=%d  curnodenum=%ld",
	      vecptr->elmleft, vecptr->nodesz, vecptr->count,
	      vecptr->curnodenum);
}
#endif
return(SUCCESS);

} /* end of initvector() */

int _DECLARE bldindex(sgp,handle,kptr)	/* d_report: 10630+ */
#ifdef ACDLL
		DBGDAT_PTR dbgptr,
#endif
		PSORTGDAT sgp; int handle; CHAR_PTR kptr;
{
int rc;
PLEVEL vecptr;
int levelno;
long ll;

if (uniq)
{
    if (!uniquniq);
    {
	if (!ACmemcmp(uniquebuf, kptr, itemlen)) return(SUCCESS);
    }
    uniquniq = 1;
    ACmemcpy(uniquebuf, kptr, itemlen);
}

/* put the key in the leaf level */
if (action == OP1)
{
	ACmemcpy(vector->bufptr, kptr, itemlen);
}
else
{
	ACmemset(vector->bufptr, 0, 4);
	ACmemcpy(vector->bufptr + 4, kptr, itemlen);
}
vector->bufptr += skeylen;
vector->count++;
if (--(vector->elmleft) && vector->count != maxitems) return(SUCCESS);
ACmemset(vector->bufptr, 0, 4);  /* ending 0x0000 in non-leaf */
_bytes4((long) vector->count, vector->buffer);
/* for MDX, another pointer needs to be written */
if (action == OP1)
{
    if (initnode)
    {
	curnode = initnode;
	initnode = (long) 0;
    }
    else
    {
        curnode = _dxgetnd(
#ifdef ACDLL
			dbgptr,
#endif
			handle, (PAVAILIST) &freelist);
    }
    _bytes4(lastnode, vector->buffer + 4);
}
else
{
    curnode++;
}

ll = curnode << SNODSHIFT;
rc = _dseekwt(handle, ll, (CHAR_PTR) (vector->buffer),
		vector->nodesz);
if (rc != SUCCESS)
{
    d_report = 10631;
    return(rc);
}

lastnode = curnode;
usednodes++;

if (action == OP1)
{  /* MDX */
	vector->bufptr = vector->buffer + 8;
}
else
{  /* NDX */
	vector->bufptr = vector->buffer + 4;
}
vector->count = 0;

for (levelno = 1, vecptr = vector + 1; vecptr <= vanchor; levelno++, vecptr++)
     /* This for-loop goes through the non-leaf node including root node */
{
    if (++(vecptr->count) == 1 && vecptr->elmleft == 1) continue;

    _bytes4(lastnode, vecptr->bufptr);
    if (vecptr->count <= maxitems)
    {
	if (action == OP1)
		ACmemcpy(vecptr->bufptr + 4, kptr + 4, itemlen - 4);
	else
		ACmemcpy(vecptr->bufptr + 4, kptr, itemlen);

	vecptr->bufptr += skeylen;
    }

/*
    printf("\n     BEFORE vecptr->ELMLEFT=%ld  curnode=%ld",
			vecptr->elmleft, curnode);
*/

    if (--(vecptr->elmleft) && vecptr->count <= maxitems)
		return(SUCCESS);
/*
    printf("\n     AFTER vecptr->ELMLEFT=%ld  curnode=%ld",
			vecptr->elmleft, curnode);
*/
    _bytes4((long) (vecptr->count - 1), vecptr->buffer);
    /* for MDX, another pointer needs to be written */
    if (action == OP1)
    {
	curnode = _dxgetnd(
#ifdef ACDLL
			dbgptr,
#endif
			handle, (PAVAILIST) &freelist);
	_bytes4(lastnode, vecptr->buffer + 4);
    }
    else
    {
	curnode++;
    }
    ll = curnode << SNODSHIFT;
    rc = _dseekwt(handle, ll, (CHAR_PTR) vecptr->buffer, vecptr->nodesz);
    if (rc != SUCCESS)
    {
	d_report = 10632;
	return(rc);
    }

    lastnode = curnode;
    usednodes++;

    if (action == OP1)
    {  /* MDX */
	vecptr->bufptr = vecptr->buffer + 8;
    }
    else
    {  /* NDX */
    	vecptr->bufptr = vecptr->buffer + 4;
    }
    vecptr->count = 0;
} /* end of for-loop */

return(SUCCESS);

} /* end of bldindex() */


int _DECLARE setheader(sgp,handle)
#ifdef ACDLL
			DBGDAT_PTR dbgptr,
#endif
			PSORTGDAT sgp; int handle;
{	  			/* d_report: 10636+ */
char minibuf[8];
char eofbuf[4];
int rc;
long ll;

_bytes4(curnode, minibuf);
if (action == OP1)
{
    if (freelist.mdxeof != initeof)
    {
	_bytes4(freelist.mdxeof, eofbuf);
	rc = _dseekwt(handle, (long) MDXENODE, (CHAR_PTR) eofbuf, 4);
	if (rc != SUCCESS)
	{
	    d_report = 10636;
	    return(rc);
	}
    }
    _bytes4(usednodes * blksz, minibuf + 4);
}
else
{
    _bytes4(curnode + 1,  minibuf + 4);
}

ll = (long) hdrnode << SNODSHIFT;
rc = _dseekwt(handle, ll, (CHAR_PTR) minibuf, 8);
if (rc != SUCCESS)
{
    d_report = 10637;
    return(rc);
}

if (action == OP1)
{
    
    ll = (long) hdrnode << SNODSHIFT;
    rc = _dseekwt(handle, ll + ACTHEAD, (CHAR_PTR) minibuf, 4);
    if (rc != SUCCESS)
    {
	d_report = 10638;
	return(rc);
    }
    *minibuf = (char) HASIDX;
    rc = _dseekwt(handle, ll + IDXMARK, (CHAR_PTR) minibuf, 1);
    if (rc != SUCCESS)
    {
	d_report = 10639;
	return(rc);
    }
}

/* free memory used for indexing */
for (; vector <= vanchor; vanchor--) _ACfree(vanchor->buffer);

return(SUCCESS);

} /* end of setheader() */
