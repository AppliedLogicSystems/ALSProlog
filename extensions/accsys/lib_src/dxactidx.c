/*************
* dXactidx.c *
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

#include "db4.h"
#include <stdlib.h>
#include <string.h>
#include "d4def.h"


#ifdef ACDLL
#undef d_report
#define d_report mptr->pdbg->d_report
#undef dretcode
#define dretcode mptr->pdbg->dretcode
#endif

IDX DECLARE dXactidx(
#ifndef unix
		/*** d_report assignment: 1000 ***/
	/* inputs */
	MDX mdxptr, /* MDX file that the index belongs to */
	CHAR_PTR tagname) /* tagname */
#else 	/* unix */
	mdxptr,tagname)
	MDX mdxptr;
	CHAR_PTR tagname;
#endif /*unix */
{
PIIDXMDX pidx; /* ptr to single index in MDX */
PINDEX   pindex;
int i;  /* general purpose integer */
int idxid; /* index id */
CHAR_PTR tempmem;
CHAR_PTR memory; /* memory allocated for .DB or .Xnn */
unsigned int memsize; /* memory size */
PCOMDAT comptr;
long hdrnode;
int dummy;
char cdummy;

 
#ifdef sun4
 
    int asun4int;
 
#define EXTRA_MEMORY_FOR_ALLIGNMENTS    16
 
#define MEMORY_IN_LONG_BOUNDARY(m,s)                \
    {                                               \
        asun4int=((int)m & 0x03);                   \
        if ( asun4int != 0 ) {                      \
            m += (sizeof(long) - asun4int);         \
            s += (sizeof(long) - asun4int) ;        \
        }                                           \
    }
 
#endif  /* sun4 */



if (mptr->flag != OPEN)
{
    d_report = 1001;
    dretcode = dNOOPEN;
    return( (IDX) 0);
}

i = _dxstag(mdxptr, (CHAR_PTR) tagname, 0,
		(INT_PTR) &idxid, (LONG_PTR) &hdrnode, (UINT_PTR) &memsize,
	    (INT_PTR) &dummy, (CHAR_PTR) 0, (CHAR_PTR) 0, (INT_PTR) &dummy,
	    (CHAR_PTR) &cdummy);
if (i != SUCCESS)
{
    dretcode = i;
    return( (IDX) 0);
}


#ifdef sun4
    memsize += EXTRA_MEMORY_FOR_ALLIGNMENTS;
#endif  /* sun4 */


memory = (CHAR_PTR) _ACalloc((unsigned long) memsize);
if (!memory)
{
    d_report = 1002;
    dretcode = dMEMERR;
    return( (IDX) 0);
}
tempmem = (CHAR_PTR) _ACalloc((unsigned long) DNODSIZ);
if (!tempmem)
{
    _ACfree(memory);
    d_report = 1003;
    dretcode = dMEMERR;
    return( (IDX) 0);
}


#ifdef sun4
    MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif  /* sun4 */
 

pidx = (PIIDXMDX) memory;
memory += SZIIDXMDX;
memsize -= SZIIDXMDX;

/*** fill in INDEX structure ***/
pindex = &(pidx->idx);
/** fill in COMDAT structure **/
comptr = &(pindex->xdxcdata);
i = _dseekrd(mptr->handle, (long) (hdrnode << SNODSHIFT), tempmem, DNODSIZ);
if (i != SUCCESS)
{
	d_report = 1004;
	dretcode = i;
	return( (IDX) 0);
}
comptr->flag = CLOSED; /* will be opened shortly */
comptr->handle = mptr->handle;
comptr->special = 0; /* nothing special, yet */
comptr->ftype = (int) *(tempmem + ORDERBYTE) & 0x00ff;
comptr->fmode = mptr->fmode;
comptr->size = (long) 0;
comptr->eofnode = _4bytes(tempmem + EOFNODE); /* # of blocks in use */
comptr->itemlen = (int) _2bytes(tempmem + KEYLEN);
comptr->nodesz = mptr->freelist.blksiz << SNODSHIFT;
comptr->maxitems = (int) _2bytes(tempmem + MAXKEYS);
comptr->changed = 0; /* nothing changed yet */
comptr->curnode = (long) 0;

/* comptr->flag = OPEN; */ /* Now, this index is active  !!!Wait!!! */

/** end of COMDAT structure initialization **/

pindex->indexid = idxid;
pindex->position = UNINIT;
pindex->root = _4bytes(tempmem);


#ifdef sun4
    MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif  /* sun4 */
 

pindex->keytype = (int) *(tempmem + XDXKTYPE) & 0x00ff;
pindex->exprlen = ACstrlen(tempmem + KEXPRESS); /* index expression */
ACmemcpy(memory, tempmem + KEXPRESS, pindex->exprlen + 1);
pindex->express = memory;
memory  += pindex->exprlen + 1;
memsize -= pindex->exprlen + 1;

switch(pindex->keytype)
{
case 'N':
case 'F':
    pindex->keycmp = (comptr->ftype & DESCEND) ? _ddesfcm : _dfcmp;
    break;

case 'D':
    pindex->keycmp = (comptr->ftype & DESCEND) ? _ddesncm : _dncmp;
    break;

default:
    pindex->keycmp = (comptr->ftype & DESCEND) ? _ddescmp : ACmemCMP;
}

pindex->complen = (int) _2bytes(tempmem + KCOMPLEN);

for (i = 0; i < ACCESSSZ; i++)
{
    pindex->xdxacc[i].nodeid = (long) 0;
    pindex->xdxacc[i].potatoid = NIL;
    pindex->xdxacc[i].offset = (unsigned short) 0;
}

#ifdef sun4
    MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif  /* sun4 */
 

pindex->leaf = pindex->work = (PXDXVEC) 0;
pindex->currecno = (long) 0;
pindex->curkey = memory;
memory += comptr->itemlen;
memsize -= comptr->itemlen;
pindex->bnumglst  = _4bytes(tempmem + BNOGLIST);
pindex->hasix = _2bytes(tempmem + IDXMARK);
pindex->tail = _4bytes(tempmem + ACTTAIL);
pindex->head = _4bytes(tempmem + ACTHEAD);

/*** end of INDEX structure initialization ***/


#ifdef sun4
    MEMORY_IN_LONG_BOUNDARY(memory,memsize)
#endif  /* sun4 */
 

pidx->amdx = mptr;
pidx->hdrnode = hdrnode;
pidx->tagname = memory;
i = ACstrlen(tagname) + 1;
memory += i;
memsize -= i;
(void) ACstrcpy(pidx->tagname, tagname);

if (!(mptr->tailiidx))
{
    mptr->headiidx = mptr->tailiidx = pidx;
    pidx->nextidx = pidx->previdx = (PIIDXMDX) 0;
}
else
{
    mptr->tailiidx->nextidx = pidx;
    pidx->previdx = mptr->tailiidx;
    pidx->nextidx = (PIIDXMDX) 0;
    mptr->tailiidx = pidx;
}

comptr->flag = OPEN; /* Yes, this index is now active! */

_ACfree(tempmem);

return((IDX) pidx);
} /* end of dXactidx() */


long DECLARE dXidxbuf(
#ifndef unix 

		/*** d_report assignment: N/A ***/
	/* inputs */
	MDX mdxptr, /* MDX file that the index belongs to */
	CHAR_PTR tagname)
#else  /* unix */ 
	
 	mdxptr, tagname ) 
	MDX mdxptr; 
	CHAR_PTR tagname; 
#endif /* unix */ 
 /* tagname */
{
int rc;
int dummy;
char cdummy;
long hdrnode;
unsigned memsize;

rc = _dxstag(mdxptr, (CHAR_PTR) tagname, 0,
		(INT_PTR) &dummy, (LONG_PTR) &hdrnode, (UINT_PTR) &memsize,
	    (INT_PTR) &dummy, (CHAR_PTR) 0, (CHAR_PTR) 0, (INT_PTR) &dummy,
	    (CHAR_PTR) &cdummy);
if (rc != SUCCESS) return( (long) rc);

return( (long) memsize);

} /* end of dXidxbuf() */

