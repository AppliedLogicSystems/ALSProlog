/*******************
*    _dascnd.c	   *
********************/

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

#include <string.h>
#include "db4.h"
#include "d4def.h"


#ifndef unix
int _DECLARE goback(PINDEX pindex, int rc);
int _DECLARE updidx(int opcode, int ismdx, PCOMDAT comptr, PXDXNOD nodptr,
		unsigned short offset, long node1, CHAR_PTR key1);
void _DECLARE insidx(int ismdx, PCOMDAT comptr, PXDXNOD nodptr,
		unsigned short offset, CHAR_PTR key2, long node2);
#else 	/* unix */
int _DECLARE goback();
int _DECLARE updidx();
void _DECLARE insidx();
#endif 	/* unix */



/*
 * _dascnd() ascends an index from a leaf.
 * While ascending, it adds and updates the index file.
 */

#ifndef ACDLL
int _DECLARE _dascnd(
#else
int _DECLARE _dascnd(DBGDAT_PTR dbgptr,
#endif
	opcode,pindex,pio,pfreelst,key1,node1,key2,node2)
		/* inputs */      /* d_report arrangement: 10000 */
	int    		opcode;	 /* ACWRITE or REMOVE */
	PINDEX  	pindex;
	PADIOBUFF 	pio;
	PAVAILIST	pfreelst;
	CHAR_PTR 	key1;
	long 		node1;
	CHAR_PTR 	key2;
	long 		node2;
{
#ifdef ILYAS_NOT_IGNORE
	int _DECLARE updidx(int opcode, int ismdx, PCOMDAT comptr, PXDXNOD nodptr,
						unsigned short offset, long node1, CHAR_PTR key1);
	void _DECLARE insidx(int ismdx, PCOMDAT comptr, PXDXNOD nodptr,
						unsigned short offset, CHAR_PTR key2, long node2);
#endif 	/*ILYAS_NOT_IGNORE */

	long nodenum;
	unsigned short offset;
	int itemsize;
	unsigned half1, half2, whichalf;
	int level;
	PPOTATO potatptr;
	PXDXNOD nodptr, nodptr2;
	PCOMDAT comptr;
	int i;
	int keepupdt; /* 1 => keep updating 1st element in each node
		 			 0 => no update */
	CHAR_PTR gptr;
	PXDXVEC vecptr1, vecptr2;
	int ismdx;

	ismdx = pindex->indexid;
	comptr = &(pindex->xdxcdata);
	itemsize = comptr->itemlen;

	level = 0;
	keepupdt = 0;
	for (pindex->work--;pindex->xdxacc <= pindex->work; pindex->work--, level++)
	{
    	/* grab node in buffer */
    	i = _dgrbnod(
#ifdef ACDLL
			dbgptr,
#endif
			pindex, pio, pindex->work, pindex->work->nodeid, &nodptr);
    	if (i != SUCCESS) return(goback(pindex, i));

    	if (key1)
    	{   /* original key was changed; update it */
			if ((int) pindex->work->offset < 0) pindex->work->offset = 0;
			keepupdt = updidx(opcode, ismdx, comptr, nodptr,
			  				pindex->work->offset, node1, key1);
	     	/* keepupdt == 1 : will split && will be updated in 2nd half */
    	}
    	if (opcode == REMOVE)
    	{
        	if (pindex->work->offset == nodptr->has * itemsize)
			{
	    		node1 = nodptr->nodeid;
	    		continue;
			}
			return(SUCCESS);
    	}
    	if (!key2 && !keepupdt) return(SUCCESS);

    	if (!key2)
    	{
        	/* (!key2) condition only occurs when inserting to upper nodes */
			node1 = nodptr->nodeid; /* no use for erasing key */
        	continue;    
    	}

    	pindex->work->offset += itemsize; /* insert after */

    	/* split occurred; insert second key */
    	offset = pindex->work->offset / itemsize;

    	if (nodptr->has < comptr->maxitems)
    	{
        	if (level && (offset > nodptr->has))
			{
	    		/* intermediate node: inserting after the last key */
	    		nodenum = _4bytes(nodptr->nodebuf + (ismdx ? 8 : 4)
			      				+ itemsize * nodptr->has);
	    		i = _dhdnkey(
#ifdef ACDLL
						dbgptr,
#endif
						pindex, pio, nodenum, &node1, &key1);
	    		if (i != SUCCESS) return(goback(pindex, i));
	    		ACmemcpy(nodptr->nodebuf + (ismdx ? 8 : 4)
	    	   			+ pindex->work->offset - itemsize + 4, key1 + 4,
		   				itemsize - 4);
			}
			/* insert index */
        	insidx(ismdx, comptr, nodptr, offset, key2, node2);

			/* if (!keepupdt) */
			return(SUCCESS);
	
        	/* both new keys fit in this node */
			/*
			node1 = nodptr->nodeid;
        	key2 = (CHAR_PTR) 0;
        	continue;
			*/
    	}

    	/*
     	* no more space in this node: time to split
     	*/

    	half1 = (nodptr->has + 1) >> 1;
    	half2 = (nodptr->has + 1) - half1;

    	if ( (unsigned) offset <= half1)
    	{
        	whichalf = 1; /* 1st half */
    	}
    	else
    	{
        	if ((nodptr->has + 1) & 1)
        	{	
            	whichalf = half2; /* whichalf => temp. var */
            	half2 = half1;
            	half1 = whichalf;
        	}

        	whichalf = 2; /* 2nd half */
        	offset -= half1;
    	}

    	potatptr = _dgetptt( &(pio->mru), &(pio->lru));
    	nodptr2 = (PXDXNOD) potatptr->bufptr;
	
    	if (nodptr2->bufflag & ACWRITE)
    	{
			i = _dseekwt(comptr->handle, (long) nodptr2->nodeid << SNODSHIFT,
		     		nodptr2->nodebuf, (unsigned) comptr->nodesz);
			if (i != SUCCESS)
			{
	    		d_report = 10001;
	    		return(goback(pindex, i));
			}
			if (nodptr2->nodeid > pio->lstpnode) 
				pio->lstpnode = nodptr2->nodeid;
    	}

    	/* initialize new node elements (nodptr->....) */
    	if (pfreelst)
    	{
			nodptr2->nodeid = _dxgetnd(
#ifdef ACDLL
						dbgptr,
#endif
						comptr->handle, pfreelst);
			if (!(nodptr2->nodeid)) return(goback(pindex, dIOERR));
    	}
    	else
    	{
			nodptr2->nodeid = comptr->eofnode++;
    	}
    	comptr->changed = 1;

    	/* adjust count values in buffers */
    	/* (1) 2nd buffer */
    	_bytes4((long) (half2 - 1), nodptr2->nodebuf);
    	nodptr2->has = half2 - 1;

    	/* (2) 1st buffer */
    	_bytes4((long) half1 - 1, nodptr->nodebuf);
    	nodptr->has = half1 - 1;

    	/* (3) if MDX, fix node chain */
    	if (ismdx)
    	{
			(void) ACmemcpy(nodptr2->nodebuf + 4, nodptr->nodebuf + 4, 4);
			_bytes4(nodptr2->nodeid, nodptr->nodebuf + 4);
			if (!_4bytes(nodptr2->nodebuf + 4)) pindex->tail = nodptr2->nodeid;
			pindex->bnumglst += pfreelst->blksiz;
    	}

    	/* (4) copy 2nd half of 1st buffer to 2nd buffer */
    	i = half2 * itemsize + 4;
    	(void) ACmemcpy(nodptr2->nodebuf + (ismdx ? 8 : 4),
		  		nodptr->nodebuf + (ismdx ? 8 : 4) + half1 * itemsize,
		  		(unsigned) i);
    	nodptr->bufflag = ACWRITE;
    	nodptr2->bufflag = ACWRITE;

    	if (whichalf == 2)
    	{   /* new key in 2nd half */
			insidx(ismdx, comptr, nodptr2, offset, key2, node2);
			pindex->work->nodeid = nodptr2->nodeid;
			pindex->work->offset = offset * itemsize;
        	pindex->work->potatoid = (unsigned) (potatptr - pio->potatoes);
			/*
			keepupdt = 1;
			*/
			i = 1; /* i used to set key1 */
    	}
    	else
    	{   /* new key in 1st half */
        	insidx(ismdx, comptr, nodptr, offset, key2, node2);
			if (offset >= nodptr->has)
			{
				/*
				keepupdt = 1;
				*/
				keepupdt = keepupdt;
				i = 1;
			}
			else
			{
				key1 = (CHAR_PTR) 0;
				i = 1;
			}
    	}
    	if (keepupdt)
    	{
			(void) updidx(opcode, ismdx, comptr, nodptr2,
		      		pindex->work->offset - itemsize,
		      		node1, key1);
    	}
    	if (i || keepupdt)
    	{
			key1 = nodptr->nodebuf + (ismdx ? 8 : 4)
						+ nodptr->has * itemsize;
			node1 = nodptr->nodeid;
    	}
    	nodenum = _4bytes(nodptr2->nodebuf + (ismdx ? 8 : 4)
		      			+ itemsize * nodptr2->has);
    	i = _dhdnkey(
#ifdef ACDLL
				dbgptr,
#endif
				pindex, pio, nodenum, &node2, &key2);
    	if (i != SUCCESS) return(goback(pindex, i));
    	node2 = nodptr2->nodeid;
	} /* end of for-loop */

	if (opcode == REMOVE) return(SUCCESS);

	/* New root! New root! New root! */
	pindex->work = pindex->xdxacc;

	vecptr1 = vecptr2 = pindex->leaf;
	vecptr2++;
	while (pindex->xdxacc <= vecptr1)
	{
    	vecptr2->nodeid = vecptr1->nodeid;
    	vecptr2->potatoid = vecptr1->potatoid;
    	vecptr2->offset = vecptr1->offset;
    	vecptr1--;
    	vecptr2--;	    	    
	}

	pindex->leaf++;
	if (pfreelst)
	{
		pindex->root = _dxgetnd(
#ifdef ACDLL
							dbgptr,
#endif
							comptr->handle, pfreelst);
		if (!(pindex->root)) return(goback(pindex, dIOERR));

		i = _dgrbnod(
#ifdef ACDLL
				dbgptr,
#endif
				pindex, pio, pindex->work, node2, &nodptr2);
		if (i != SUCCESS) return(goback(pindex, i));
	}
	else
	{
		pindex->root = comptr->eofnode++;
	}
	comptr->changed = 1; /* ensure that IDX or NDX header will be updated */
	
	pindex->work->nodeid = pindex->root;
	pindex->work->potatoid = NIL;
	pindex->work->offset = itemsize;

	/* grab new root node in buffer */
	i = _dgrbnod(
#ifdef ACDLL
			dbgptr,
#endif
			pindex, pio, pindex->work, pindex->root, &nodptr);
	if (i != SUCCESS) return(goback(pindex, i));

	_bytes4((long) 1, nodptr->nodebuf);
	gptr = nodptr->nodebuf + (ismdx ? 8 : 4);
	(void) ACmemcpy(gptr, key1, itemsize);
	_bytes4(node1, gptr);
	_bytes4(node2, gptr + itemsize);

	nodptr->has = 1; /* 1st key will be updated with key1 */

	if (ismdx)
	{
    	/* Here, the relationship between nodptr & nodptr2 is reversed */
    	(void) ACmemcpy(nodptr->nodebuf + 4, nodptr2->nodebuf + 4, 4);
    	_bytes4(nodptr->nodeid, nodptr2->nodebuf + 4);
    	if (!_4bytes(nodptr->nodebuf + 4)) pindex->tail = nodptr->nodeid;
    	pindex->bnumglst += pfreelst->blksiz;
    	nodptr2->bufflag = ACWRITE;
	}

	nodptr->bufflag = ACWRITE;

	return(SUCCESS);

} /* end of _dascnd() */


static int _DECLARE updidx(opcode,ismdx,comptr,nodptr,offset,node1,key1)
	int    opcode;
	int    ismdx;
	PCOMDAT comptr;
	PXDXNOD nodptr;
	unsigned short offset;
	long   node1;
	CHAR_PTR key1;
{
	CHAR_PTR gptr;
	unsigned int len;

	if ((offset / comptr->itemlen) < comptr->maxitems)
	{
	    len = comptr->itemlen - 4;
	    gptr = nodptr->nodebuf + 4 + offset;
	    if (ismdx) gptr += 4;
	    (void) ACmemcpy(gptr + 4, key1 + 4, len);
	    _bytes4(node1, gptr);
	    nodptr->bufflag = ACWRITE;
	    return(0); /* no more updating */
	}
	if (opcode == REMOVE)
	{
	    gptr = nodptr->nodebuf + 4 + offset;
	    if (ismdx) gptr += 4;
	    _bytes4(node1, gptr);
	    nodptr->bufflag = ACWRITE;
	    return(0); /* no more updating */
	}	    
	return(1); /* keep updating more */

} /* end of updidx() */


static void _DECLARE insidx(ismdx,comptr,nodptr,offset,key,nodeid)
	int    ismdx;
	PCOMDAT comptr;
	PXDXNOD nodptr;
	unsigned short offset;
	CHAR_PTR key;
	long   nodeid;
{
	CHAR_PTR idxptr;
	unsigned len;
	int itemsize;

	itemsize = comptr->itemlen;

	/* (1) make a space by shifting down some records */

	idxptr = nodptr->nodebuf + (ismdx ? 8 : 4) + offset * itemsize;

	if (offset > nodptr->has)
	{
		/* split occured */
		/* _bytes4(nodeid, idxptr); */
		goto done;
	}

	len = ( (unsigned) nodptr->has - offset) * itemsize + 4;
	if (nodptr->has <= (comptr->maxitems - 2)) len += itemsize;
	
	(void) ACmemmove(idxptr + itemsize, idxptr, len);

done:

	/* (2) finally copy the second key to buffer */
	if (offset < comptr->maxitems)
	{
    	(void) ACmemcpy(idxptr, key, itemsize);
	}

	_bytes4(nodeid, idxptr);

	/* (3) increment key count */
	_bytes4((long) ++(nodptr->has), nodptr->nodebuf);

	nodptr->bufflag = ACWRITE;

} /* end of insidx() */


static int _DECLARE goback(pindex, rc)
	PINDEX 	pindex;
	int   	rc;
{
	PXDXVEC vecptr;

	for (vecptr = pindex->work; pindex->work <= vecptr; vecptr--)
    	if (vecptr->offset < 0) vecptr->offset = 0;

	return(rc);
} /* end of goback() */


