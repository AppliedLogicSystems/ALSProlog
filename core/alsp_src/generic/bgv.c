/*=========================================================================*
 |		bgv.c
 |	Copyright (c) 1985 by Kevin A. Buettner
 |	Copyright (c) 1986-1995 by Applied Logic Systems
 |
 |		-- Global variable Prolog builtins defined in C.
 |
 | Program Author: K.A. Buettner
 | Creation:  11/14/84
 | Revision History: 
 | 06/28/85 - K.Buettner -- Conversion to wam and compiled prolog
 | 09/12/85 - K.Buettner -- arithmetic predicates moved to separate file.
 | 01/28/86 - K.Buettner -- IBM PC conversion
 *=========================================================================*/

#include "defs.h"

int
pbi_gv_alloc(void)
{				/* gv_alloc(vn) */
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

    if (w_unify(v, t, gv_alloc(), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}

int
pbi_gv_free(void)
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

    if (t != WTP_INTEGER || v <= 0 || v > (wm_gvbase - wm_trailbase))
	FAIL;

    gv_free(v);
    SUCCEED;
}

int
pbi_gv_get(void)
{
    PWord v1;
    int   t1;
    PWord v2;
    int   t2;
    PWord v;
    int   t;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 != WTP_INTEGER || v1 <= 0 || v1 > (wm_gvbase - wm_trailbase))
	FAIL;

    gv_get(&v, &t, v1);

    if (w_unify(v, t, v2, t2))
	SUCCEED;
    else
	FAIL;
}

int
pbi_gv_set(void)
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 != WTP_INTEGER || v1 <= 0 || v1 > (wm_gvbase - wm_trailbase))
	FAIL;

    gv_set(v2, t2, v1);
    SUCCEED;
}

/*
 * pbi_gv_alloc_init(Num,Value)
 *
 *	Attempts to allocate variable numbered Num and initialize it to
 *	Value.  If Num is not an integer or if Num is already allocated,
 *	this procedure will fail.
 */

int
pbi_gv_alloc_init(void)
{
    PWord v1, v2;
    int   t1, t2;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (t1 != WTP_INTEGER || v1 <= 0 || 
	v1 > (wm_gvbase - wm_trailbase) + ((wm_TR - wm_H) - wm_normal) ||
	gv_alloc_gvnum(v1) != v1)
	FAIL;
    
    gv_set(v2, t2, v1);
    SUCCEED;
    
}


/*
 * pbi_gv_isfree(Num)
 *
 *	Succeeds or Fails based upon whether the variable number Num is
 * 	free or not.
 */


int
pbi_gv_isfree(void)
{
    PWord v1;
    int   t1;

    w_get_An(&v1, &t1, 1);

    if (t1 != WTP_INTEGER || v1 <= 0 ||
	v1 > (wm_gvbase - wm_trailbase) + ((wm_TR - wm_H) - wm_normal) ||
	!gv_isfree(v1))
	FAIL;
    else
	SUCCEED;
}


/*
 * pbi_gv_maxpossible(Num)
 *
 *	Unifies Num with the current maximum number of possible global
 *	variables.  This is not necessarily the number of variable
 *	actually allocated, but is computed based upon the space allocated
 *	for global variables.  It will give us a valid upper bound for
 *	doing packaging.
 */

int
pbi_gv_maxpossible(void)
{
    PWord v1;
    int   t1;
    
    w_get_An(&v1, &t1, 1);
    
    if (w_unify(v1, t1, wm_gvbase - wm_trailbase, WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}
