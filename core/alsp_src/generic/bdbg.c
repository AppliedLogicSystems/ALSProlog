/*================================================================*
 |		bdbg.c
 |	Copyright (c) 1989-1995 Applied Logic Systems, Inc.
 |
 |		-- debugger builtins
 |
 | Author: K.A. Buettner
 | Creation: 05/5/89
 *================================================================*/

#include "defs.h"
#include "wintcode.h"

int
pbi_dbg_nospy(void)
{				/* dbg_nospy(M,P,A) */
    PWord m, p, a;
    int   mt, pt, at;

    w_get_An(&m, &mt, 1);
    w_get_An(&p, &pt, 2);
    w_get_An(&a, &at, 3);

    if (xform_uia(&m, &mt) && xform_uia(&p, &pt) && at == WTP_INTEGER) {
	int   status;

	dbprot_t odbrs = w_dbprotect(DBRS_WRITABLE);
	status = w_nospy(m, p, (int) a);
	(void) w_dbprotect(odbrs);
	if (status)
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}


int
pbi_dbg_spy(void)
{				/* dbg_spy(M,P,A) */
    PWord m, p, a;
    int   mt, pt, at;

    w_get_An(&m, &mt, 1);
    w_get_An(&p, &pt, 2);
    w_get_An(&a, &at, 3);

    if (xform_uia(&m, &mt) && xform_uia(&p, &pt) && at == WTP_INTEGER) {
	int   status;

	dbprot_t odbrs = w_dbprotect(DBRS_WRITABLE);
	status = w_spy(m, p, (int) a);
	(void) w_dbprotect(odbrs);
	if (status)
	    SUCCEED;
	else
	    FAIL;
    }
    else
	FAIL;
}


int
pbi_dbg_spyoff(void)
{				/* dbg_spyoff */
    wm_spying = 0;
    SUCCEED;
}

int
pbi_dbg_spyon(void)
{				/* dbg_spyon */
    wm_spying = 1;
    SUCCEED;
}

int
pbi_dbg_spying(void)
{				/* dbg_spying */
    if (wm_spying)
	SUCCEED;
    else
	FAIL;
}
