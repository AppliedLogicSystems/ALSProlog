/*******************
*    _dfindbf.c	   *
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

#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

#ifdef ACDLL
#undef d_report
#define d_report tblptr->pdbg->d_report
#endif

/*
 * _dfindbf() locates a node buffer to be used by various calling
 * functions.  For a 'write' operation, it does not mean that the
 * located buffer contains a record that the calling function is
 * looking for.  It simply means that the node to be inserted or
 * added was found.
 *
 * It locates a buffer by a record number.
 * The potato associated with the buffer located is placed in the
 * most recently used position of the potato chain.
 */
int _DECLARE _dfindbf(dbfptr,recno,iomode,nodenum,offset,tnode)
	DBF	 dbfptr;    /* d_report arrangement: N/A */
	long     recno;
	int      iomode;   /* ACREAD or ACWRITE: to simplify algorithm */
		/* output */
	LONG_PTR nodenum;
	INT_PTR  offset;
    pPTBLNOD tnode;
{
	int rc;

	_dfindnd(dbfptr, recno, nodenum, offset);

	rc = _dgetbf(dbfptr, recno, iomode, *nodenum, *offset, tnode);

	return(rc);
} /* _dfindbf() */

