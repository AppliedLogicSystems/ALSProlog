/**************
 * _dzmerge.c *
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
#include "db4.h"
#include "d4def.h"
#ifdef STANDARD
#include <sys\types.h>
#include <sys\stat.h>
#include <io.h>
#endif
#include "dzinclud.h"

NONLEAF nonleafs[7] =
{
		 /* parent,        loser,     l, r,   winner      */
    /* 0 */	 nonleafs + 4,   (LEAFPTR) 0, 0, 1, (LEAFPTR) 0,
    /* 1 */	 nonleafs + 4, 	 (LEAFPTR) 0, 2, 3, (LEAFPTR) 0,
    /* 2 */	 nonleafs + 5, 	 (LEAFPTR) 0, 4, 5, (LEAFPTR) 0,
    /* 3 */	 nonleafs + 5, 	 (LEAFPTR) 0, 6, 7, (LEAFPTR) 0,
    /* 4 */	 nonleafs + 6, 	 (LEAFPTR) 0, 0, 1, (LEAFPTR) 0,
    /* 5 */	 nonleafs + 6,   (LEAFPTR) 0, 2, 3, (LEAFPTR) 0,
    /* 6 */	 NULL, 		 NULL, 4, 5, 	    NULL
};

int _DECLARE dmerge(
#ifndef unix 
	/* d_report: 10660+ */
	/* inputs */
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	PSORTGDAT sgp,
        UPTR outptr,         /* output file pointer */
	long totitems,       /* total items */
	unsigned int blocks, /* number of total blocks: each block is
                                already internally sorted */
	long unitsize,	     /* block size in bytes */
        int external,        /* 0: internal sort (may still merge)
                                1: external incl. merge */
        unsigned int initmsiz /* initial memory size for leafs[].msize */
        /* itemlen <---- global var. */
		   )
#else  /* unix */ 
	
 	sgp, outptr, totitems, blocks, unitsize, external, initmsiz ) 
	PSORTGDAT sgp; 
	UPTR outptr; 
	long totitems; 
	unsigned int blocks; 
	long unitsize; 
	int external; 
	unsigned int initmsiz; 
#endif /* unix */ 

	/* output : none */
{
int _DECLARE rdnext(
#ifndef unix 

#ifdef ACDLL
		DBGDAT_PTR dbgptr,
#endif
		PSORTGDAT sgp, int fd, LEAFPTR lp, int external)
#else  /* unix */ 
	 ) 
#endif /* unix */ 
;
CHAR_PTR memory;
int i, j;
long lll;
unsigned int nextblks; /* number of next blocks */
int leafcnt;	       /* # of leafs */
LEAFPTR leafptr;
long sizeleft;	/* size of unprocessed part of file in bytes */
unsigned int memsize; /* memory size */
long nextusiz; /* next unitsize */
int flag;  /* 1: do something   0: do nothing */
long sblksiz; /* blksize for sort */
NONLEAFPTR nleafptr;
int firstime; /* 1: 1st time in loop  0: not 1st time in loop */
long count;
CHAR_PTR cptr;

count = (long) 0;
firstime = 1; /* first try */

while (1)
{
    nextblks = 0;
    sizeleft = totitems * itemlen;
    nextusiz = 0;
    flag = 1;
    lll = (long) 0;

    if (blocks <= 1 && !firstime) break; /* done */
    firstime = 0;

    while (blocks)
    {
	/***** setup nodes *****/
	/* set up leafs */
	nextblks++;

	if (blocks == 1 && external)
	{
	    memsize = leafs[0].msize;
            memory = leafs[0].leafmem;
            if (TOUTMSIZ > memsize)
            {
                memsize = TOUTMSIZ;
                memory = sortbuf;
            }
	    while (sizeleft > (long) 0)
	    {
		if (sizeleft < (long) memsize)
		    memsize = (unsigned int) sizeleft;

		if (lseek(temp1fd, lll, 0) == (long) -1)
		{
#ifdef DEBUG
printf("\nM1\n");
#endif
		    d_report = 10661;
		    return(dIOERR);
		}
		if (_ACread(temp1fd, memory, memsize) != memsize)
		{
#ifdef DEBUG
printf("\nM2\n");
#endif
		    d_report = 10662;
		    return(dIOERR);
		}
		if (_ACwrite(temp2fd, memory, memsize) != memsize)
		{
#ifdef DEBUG
printf("\nM3\n");
#endif
		    d_report = 10663;
		    return(dIOERR);
		}
		sizeleft -= memsize;
		lll += memsize;
	    } /* end of while (sizeleft > (long) 0)..... */
	    break; /* get out of while(blocks) loop */
	} /* end of if (blocks == 1 && external).... */

	leafcnt = (blocks >= K) ? K : blocks;

	sblksiz = (long) 0;
	for (i=0, leafptr=leafs; i < leafcnt; i++, leafptr++)
	{
	    if (unitsize > sizeleft) unitsize = sizeleft;

	    leafptr->blkloc = lll;
	    leafptr->blksize = unitsize;

	    leafptr->valptr = leafptr->leafmem;
            leafptr->msize = initmsiz;
	    leafptr->items = initmsiz / itemlen;

	    if (leafptr->blksize < leafptr->msize)
	    {
		leafptr->msize = (unsigned int) leafptr->blksize;
		leafptr->items = leafptr->msize / itemlen;
	    }

            if (external)
            {
	        if (lseek(temp1fd, leafptr->blkloc, 0) == (long) -1)
	        {
#ifdef DEBUG
printf("\nM4\n");
#endif
		    d_report = 10664;
		    return(dIOERR);
	        }
	        if (_ACread(temp1fd, leafptr->leafmem, leafptr->msize)
		    != leafptr->msize)
	        {
#ifdef DEBUG
printf("\nM5\n");
#endif
		    d_report = 10665;
		    return(dIOERR);
	        }
            }

	    lll += unitsize;
	    if (flag) nextusiz += unitsize;
	    sblksiz += unitsize;
	    sizeleft -= unitsize;
	} /* end of for (i=0, leafptr=leafs; i < leafcnt; i++, leafptr++)
                    loop */
	flag = 0;

	/* setup non-leafs and some leafs */
	for (i = 0, leafptr = leafs; i < leafcnt; i++, leafptr++)
	{
	    leafptr->nonleaf = (unsigned char) (i >> 1);
	}
	/* pick first winner */
	for (i = 0, nleafptr = nonleafs;
	     i <= 6;
	     i++, nleafptr++)
	{
	    nleafptr->winner = (LEAFPTR) 0;
	}
	for (i = 0, leafptr=leafs, nleafptr = nonleafs;
	     i < 4;
	     i++, leafptr++, leafptr++, nleafptr++)
	{
	    j = i << 1;
	    if ( leafcnt <= j ) break;
	    if ( leafcnt == j + 1)
	    {
		nleafptr->loser = leafptr;
		nleafptr->winner = leafptr;
		break;
	    }
	    if ((*asyscmp) (sgp, (UCHAR_PTR) leafptr->valptr,
			         (UCHAR_PTR) (leafptr + 1)->valptr) > 0)
	    {
		nleafptr->loser = leafptr;
		nleafptr->winner = (leafptr + 1);
	    }
	    else
	    {
		nleafptr->loser = leafptr + 1;
		nleafptr->winner = leafptr;
	    }
	}
	for (i = 4, leafptr=leafs, nleafptr = nonleafs + 4;
	     i <= 6;
	     i++, leafptr++, leafptr++, nleafptr++)
	{
	    if (!(nonleafs[nleafptr->left].winner)) continue;

	    if (!(nonleafs[nleafptr->right].winner))
	    {
		nleafptr->winner = nonleafs[nleafptr->left].winner;
		nleafptr->loser  = nonleafs[nleafptr->left].loser;

		continue;
	    }
	    if ((*asyscmp)(sgp,
		    (UCHAR_PTR) (nonleafs[nleafptr->left].winner)->valptr,
		    (UCHAR_PTR) (nonleafs[nleafptr->right].winner)->valptr)
		 > 0)
	    {
		nleafptr->loser = nonleafs[nleafptr->left].winner;
		nleafptr->winner = nonleafs[nleafptr->right].winner;
	    }
	    else
	    {
		nleafptr->loser = nonleafs[nleafptr->right].winner;
		nleafptr->winner = nonleafs[nleafptr->left].winner;
	    }
	}

	/***** sort nodes *****/
        if (external)
        {
	    cptr = sortbuf;
	    memsize = 0;

            if (nextblks == 1 && blocks <= 8)
            {
                close(temp2fd);
                (void) _ACremove(temp2nm);
                temp2fd = -1;
            }
        }

	while (2)
	{
            if (external)
            {
	        if ( (    (memsize + itemlen > TOUTMSIZ)
                       || (sblksiz <= (long) 0) )  &&  (temp2fd != -1) )
	        {
		    if (_ACwrite(temp2fd, sortbuf, memsize) != memsize)
		    {
#ifdef DEBUG
printf("\nM6\n");
#endif
			d_report = 10666;
		        return(dIOERR);
		    }
		    memsize = 0;
		    cptr = sortbuf;
	        }
            }

	    if (sblksiz <= (long) 0) break; /* done */

            if (external)
            {

	        if (nextblks == 1 && blocks <= 8)
	        {
                    count++;

                    if (action == OP0)
		    {
			i = dDapprec((DBF) outptr, WINVAL);
			if (i != SUCCESS)
			{
			    d_report = 10667;
			    return(i);
			}
		    }
		    else
		    {
		        i = bldindex(
#ifdef ACDLL
				dbgptr,
#endif
				sgp, ((MDX) outptr)->handle, WINVAL);
                                        /* write to final output file */
			if (i != SUCCESS) return(i);
		    }
	        }
                else
                {
	            ACmemcpy(cptr, WINVAL, itemlen);
	            cptr += itemlen;
	            memsize += itemlen;
                }
            }
            else
            {
		i = dDapprec((DBF) outptr, WINVAL);
		if (i != SUCCESS) return(i);
		count++; /* internal sort: direct output */
            }
	    sblksiz -= itemlen;

	    i = rdnext(
#ifdef ACDLL
			dbgptr,
#endif
			sgp, temp1fd, WINLEAF, external);
	    if (i < 0) return(i); /* error */
	    if (i == EMPTY)
	    {
		nleafptr = &nonleafs[WINLEAF->nonleaf];
		if (nleafptr->loser == WINLEAF)
		{
		    leafptr = nleafptr->parent->loser;
		    if (leafptr == WINLEAF)
		    {
			WINLEAF = ROOT.loser;
			ROOT.loser
			    = (nonleafs[WINLEAF->nonleaf].parent)->loser;
		    }
		    else
		    {
			WINLEAF = leafptr;
			nleafptr->parent->loser
			    = nonleafs[leafptr->nonleaf].loser;
		    }
		}
		else
		{
		    WINLEAF = nleafptr->loser;
		}
	    } /* end of if (rdnext(...) ....... */

	    /* climb up in the decision tree */
	    nleafptr = &nonleafs[WINLEAF->nonleaf];
	    for (i = 1; i <= 3; i++, nleafptr = nleafptr->parent)
	    {
		if (nleafptr->loser == WINLEAF) continue;

	        if ((*asyscmp)(sgp,
			(UCHAR_PTR) nleafptr->loser->valptr, (UCHAR_PTR) WINVAL) < 0)
		{
		    leafptr = nleafptr->loser;
		    nleafptr->loser = WINLEAF;
		    WINLEAF = leafptr;
		}
		if (nleafptr->parent)
		{
		    if (nleafptr->parent->loser == WINLEAF)
			nleafptr->parent->loser = nleafptr->loser;
		    if (nleafptr->parent->parent)
		    {
			if (nleafptr->parent->parent->loser == WINLEAF)
			    nleafptr->parent->parent->loser
				= nleafptr->loser;
		    }
		}

	    } /* end of for (i=1; i <= 3; i++) .... */

	} /* end of while (2) ...... */

	/* final part of while (blocks)... */
	blocks -= leafcnt;

    } /* end of while (blocks)... */

    blocks = nextblks;
    unitsize = nextusiz;

    /* reverse role of temporary files */
    if (blocks > 1 && external)
    {
	i = temp2fd;
	temp2fd = temp1fd;
	temp1fd = i;
        ACmemcpy(tempnm, temp2nm, L_tmpnam);
        ACmemcpy(temp2nm, temp1nm, L_tmpnam);
        ACmemcpy(temp1nm, tempnm, L_tmpnam);

	if (lseek(temp2fd, (long) 0, 0) == (long) -1)
	{
#ifdef DEBUG
printf("\nM7\n");
#endif
	    d_report = 10668;
	    return(dIOERR);
	}
    }

} /* end of while (1).... */

if (action != OP0)
{
    i = setheader(
#ifdef ACDLL
		dbgptr,
#endif
		sgp, ((MDX) outptr)->handle );
    if (i != SUCCESS) return(i);
}

return(SUCCESS);
} /* end of dmerge() */


static int _DECLARE rdnext(
#ifndef unix 

	/* inputs */			/* d_report: 10670+ */
#ifdef ACDLL
	DBGDAT_PTR dbgptr,
#endif
	PSORTGDAT sgp,
	int     fd,
	LEAFPTR lp,
        int  external
		)
#else  /* unix */ 
	
 	sgp, fd, lp, external ) 
	PSORTGDAT sgp; 
	int fd; 
	LEAFPTR lp; 
	int external; 
#endif /* unix */ 

{
if (lp->blksize <= (long) 0)
{
#ifdef DEBUG
printf("\nM8\n");
#endif
    d_report = 10671;
    return(dERROR);
}

lp->blksize -= itemlen;
if (!lp->blksize) return(EMPTY);

    if (!(--(lp->items)))
    {
        if (!external) return(EMPTY);

	lp->blkloc += lp->msize;
	if (lseek(fd, lp->blkloc, 0) == (long) -1)
	{
#ifdef DEBUG
printf("\nM9\n");
#endif
	    d_report = 10672;
	    return(dIOERR);
	}
	lp->msize = _ACread(fd, lp->leafmem, lp->msize);
	if (lp->msize < 0)
	{
#ifdef DEBUG
printf("\nM10\n");
#endif
	    d_report = 10673;
	    return(dIOERR);
	}
	lp->items = lp->msize / itemlen;
	lp->valptr = lp->leafmem;
	return(OK);
    }
    lp->valptr += itemlen;
    return(OK);
} /* end of rdnext() */

