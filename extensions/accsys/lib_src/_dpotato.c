/**************
 * _dpotato.c *
 **************/

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
 

#include <stdlib.h>
#include "db4.h"
#include "d4def.h"

/*
 * _dgetptt() gets the least recently used potato from the chain
 * and places it in the most recently used position in the
 * potato chain specified.
 */
PPOTATO _DECLARE _dgetptt(mru,lru)  /* d_report arrangement: N/A */
	pPPOTATO mru;   /* ptr to most recently used potato */
	pPPOTATO lru;   /* ptr to least recently used potato */
{
	PPOTATO p1, p2;

	p1 = *mru;
	p2 = *lru;

	p1->prev = p2;
	p2->next = p1;

	if (p2->prev)
	{
    	p2->prev->next = (PPOTATO) 0;
    	*lru = p2->prev;
	}

	p2->prev = (PPOTATO) 0;
	*mru = p2;

	return(p2);

} /* end of _dgetptt() */

/*
 * _dcycptt() cycles the specified potato to the most recently used
 * position in the potato chain specified.
 */

void _DECLARE _dcycptt(cycle, mru, lru)  /* d_report arrangement: N/A */
	PPOTATO cycle;  /* potato to be cycled */
	pPPOTATO mru;   /* ptr to most recently used potato */
	pPPOTATO lru;   /* ptr to least recently used potato */
{
	if (cycle == *mru) return;

	if (cycle->prev)
	{
    	if (*lru == cycle) *lru = cycle->prev;
    	cycle->prev->next = cycle->next;
	}

	if (cycle->next) cycle->next->prev = cycle->prev;

	cycle->prev = (PPOTATO) 0;
	cycle->next = *mru;
	(*mru)->prev = cycle;

	*mru = cycle;

} /* end of _dcycptt() */
