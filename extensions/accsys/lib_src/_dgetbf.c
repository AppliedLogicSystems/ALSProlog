/*************
 * _dgetbf.c *
 *************/

/*****************************************************
*                                                    *
* Copyright 1989, Billy Shakespeare & Company, Inc.  *
* All rights reserved.                               *
*                                                    *
******************************************************
*                                                    *
* Published by                                       *
*        Copia International, Inc.                   *
*        Wheaton, Illinois                           *
*        U. S. A.                                    *
*                                                    *
******************************************************/
 
#include <stdio.h>
#include "db4.h"
#include "d4def.h"

#ifndef unix
#ifdef STANDARD
#include <io.h>
#else
#include <fcntl.h>
#endif
#else 	/* unix */
#include <sys/types.h>
#include <unistd.h>
#endif 	/* unix */

#ifdef ACDLL
#undef d_report
#define d_report tblptr->pdbg->d_report
#endif

int _DECLARE _dgetbf(dbfptr,recno,iomode,nodenum,offset,tnode)
	/* inputs */      /* d_report arrangement: 10060 */
	DBF    dbfptr;
	long   recno;
	int    iomode; /* ACREAD or ACWRITE */
	long   nodenum;
	int    offset;

        /* output */
    pPTBLNOD tnode;
{
	unsigned potatoid;
	PPOTATO potatptr, potptr2;
	PTBLNOD nodptr, nodptr2;
	PCOMDAT comptr;
	unsigned int rlen; /* 'read' length */
	long ll;

	comptr = &(tblptr->tblcdata);

	if ((potatoid = tblptr->tblacc[nodenum & ACCMASK]) == NIL)
	{
    	/* uninitialized vector slot */
    	potatptr = _dgetptt( &(tblptr->mru), &(tblptr->lru));
    	potatoid = (unsigned) (potatptr - tblptr->potatoes);
    	tblptr->tblacc[nodenum & ACCMASK] = potatoid;
    	nodptr = (PTBLNOD) potatptr->bufptr;

    	if (nodptr->nodeid != nodenum && nodptr->bufflag & ACWRITE)
    	{
        	if (_dbfndwt(dbfptr, nodptr, comptr) != SUCCESS)
			{
	    		d_report = 10061;
	    		return(dIOERR);
			}
			nodptr->bufflag = ACREAD;
    	}
	}
	else
	{
    	potatptr = tblptr->potatoes + potatoid;
    	nodptr = (PTBLNOD) potatptr->bufptr;

    	if (nodptr->nodeid == nodenum)
    	{
        	_dcycptt( potatptr, &(tblptr->mru), &(tblptr->lru) );
        	*tnode = nodptr;
        	return(SUCCESS);
    	}

    	for (potatoid = 0; potatoid < tblptr->potatnum; potatoid++)
    	{
			potptr2 = tblptr->potatoes + potatoid;
			nodptr2 = (PTBLNOD) potptr2->bufptr;
			if (nodptr2->nodeid != nodenum) continue;
        
        	/* Lucky!  found the right buffer */
        	tblptr->tblacc[nodenum & ACCMASK] = potatoid;
        	_dcycptt( potptr2, &(tblptr->mru), &(tblptr->lru) );
        	comptr->curnode = nodenum;
        	*tnode = nodptr2;
        	return(SUCCESS);

    	} /* end of "for (potatoid = 0; potatoid < tblptr->potatnum;
                      potatoid++)" loop */

    	/* give up and go to read from disk */
		/*    _dcycptt( potatptr, &(comptr->mru), &(comptr->lru) ); */
        /* cycle buffer before going to (write and) read */

    	potatptr = _dgetptt( &(tblptr->mru), &(tblptr->lru));
    	potatoid = (unsigned) (potatptr - tblptr->potatoes);
    	tblptr->tblacc[nodenum & ACCMASK] = potatoid;
    	nodptr = (PTBLNOD) potatptr->bufptr;
	}

	/* The following code is executed only when the vector slot
   	is uninitialized or buffer exchange has occurred. */

	if (!comptr->size)
	{
    	nodptr->nodeid = nodenum;
    	nodptr->fstrec = recno - offset;
    	nodptr->bufflag = ACREAD; /* not written yet even if so */
    	nodptr->has = 0;
    	*tnode = nodptr;
    	return(SUCCESS);
	}

	if (nodptr->bufflag & ACWRITE)
	{
        if (_dbfndwt(dbfptr, nodptr, comptr) != SUCCESS)
		{
	    	d_report = 10062;
	    	return(dIOERR);
		}
	}

	if (comptr->size)
	{
		if (lseek(comptr->handle,
	    	(long) tblptr->stadrs + (long) (nodenum - 1) * comptr->nodesz,
	    	SEEK_SET)
	    	== (long) -1)
		{
	    	d_report = 10063;
	    	return(dIOERR);
		}

		rlen = _ACread(comptr->handle, nodptr->nodebuf,
		    	(unsigned) comptr->nodesz);
		if (rlen < (unsigned) comptr->itemlen && iomode != ACWRITE)
		{
	    	d_report = 10064;
	    	return(dIOERR);
		}
		nodptr->bufflag = ACREAD;
	}
	else
	{
		nodptr->bufflag = UNUSED; /* actually used but has no data */
		rlen = 0;
	}
	if (nodenum == comptr->eofnode) comptr->eofnode++;
	nodptr->nodeid = nodenum;
	nodptr->fstrec = recno - offset;
	nodptr->has = rlen / comptr->itemlen;
	ll = comptr->size - nodptr->fstrec + (long) 1;
	if (ll < (long) (nodptr->has)) nodptr->has = (int) ll;

	*tnode = nodptr;
	return(SUCCESS);

} /* end of _dgetbf() */
 
