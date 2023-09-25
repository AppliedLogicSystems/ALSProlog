/*===============================================================*
 |			gv.c         
 |		Copyright (c) 1987-1993 Applied Logic Systems, Inc.
 |
 |			-- global variable management
 |
 | Author: Kevin A. Buettner
 | Creation: 9/18/87
 | Revision History:
 *===============================================================*/
#include "defs.h"

#define GV_SHIFT 32

static	void	fixptrs	(prolog_engine *);

PWord
gv_alloc(void)
{
    register int i;
    PWord vn;

prolog_engine_invariant(&current_engine);

    if (wm_gvfreelist != (PWord *) MMK_INT(-1)) {
	vn = (PWord) (wm_gvbase - wm_gvfreelist);
	wm_gvfreelist = (long *) *wm_gvfreelist;
    }
    else {
	long *tr;
	long  trval;


	/*
	 * Shift the trail.
	 */

	for (tr = wm_TR; tr < wm_trailbase; tr++) {
	    trval = *tr;
	    if (tr < ((long *) trval) && ((long *) trval) < wm_trailbase)
		*(long **) (tr - GV_SHIFT) = ((long *) (trval)) - GV_SHIFT;
	    else
		*(tr - GV_SHIFT) = trval;
	}
	
	
	/*
	 * Fix up the pointers from the C area into the trail
	 */

#if 0
	//fixptrs(wm_regidx);
#endif

	fixptrs(&current_engine);

	/*
	 * Initialize the new batch of variables;
	 */

	wm_trailbase_lvalue -= GV_SHIFT;
	current_engine.heap_size -= GV_SHIFT;
	
	for (i = 0; i < GV_SHIFT - 1; i++) {
	    *(long **) (wm_trailbase + i) = wm_gvfreelist;
	    wm_gvfreelist = wm_trailbase + i;
	}

	vn = (PWord) (wm_gvbase - (wm_trailbase + i));
   }

    *(wm_gvbase - vn) = MMK_INT(0);
prolog_engine_invariant(&current_engine);
    return vn;
}


/*
 * gv_alloc_gvnum		-- added by kev 8-9-93
 *
 * This function is called when initializing a package or whenever it is
 * essential to allocate a particular global variable number.  This procedure
 * does not allocate very smartly:  It repeatedly calls gv_alloc above in an
 * attempt to get the global variable in question. It will then re-free all
 * variables allocated in the process.
 *
 * In practice this should not be a problem (unless a complete moron writes
 * or rewrites the packaging code).  When an application is packaged, the
 * packaging routine should attempt to save the global variables in ascending
 * order starting from 1.  When the package is reloaded, only one call to
 * gv_alloc will need to be performed.
 *
 * When successful, gv_alloc_gvnum will return the number of the variable
 * which it allocated, otherwise it will return 0.
 *
 */

int
gv_alloc_gvnum(int gvnum)
{
    PWord *tofree;
    int num;

    if (gv_isfree(gvnum)) {
	tofree = 0;
	while ((num=gv_alloc()) != gvnum) {
	    *(wm_gvbase-num) = (PWord) tofree;
	    tofree = wm_gvbase-num;
	}
	while (tofree) {
	    PWord *temp = tofree;
	    tofree =  (PWord *) *tofree;
	    gv_free(wm_gvbase - temp);
	}
	return gvnum;
    }
    return 0;
}

/*
 * gv_isfree			-- added by kev 8-9-93
 *
 * gv_isfree determines whether or not a particular global variable is free
 * It will return 1 if the variable is free, 0 otherwise.
 */

int
gv_isfree(int gvnum)
{
    if (gvnum > wm_gvbase - wm_trailbase)
	return 1;
    else {
	PWord *p = wm_gvfreelist;
	while (p != (PWord *) MMK_INT(-1)) {
	    if (wm_gvbase - p == gvnum)
		return 1;
	    else
		p =  (PWord *) *p;
	}
	return 0;
    }
}


/*
 * fixptrs uses a cunning trick with macros.  Note that wm_regidx is both
 * a global variable and a parameter which is used in the definitions of
 * wm_TR and wm_B.
 */

#if 0
//static void
//fixptrs(wm_regidx)
//    int   wm_regidx;
//{
//    while (wm_regidx >= 0) {
//	if (wm_B)
//	    wm_B -= GV_SHIFT;
//	if (wm_TR)
//	    wm_TR -= GV_SHIFT;
//	wm_regidx--;
//    }
//}
#endif

static void fixptrs(prolog_engine *pe)
{
	register_set *s;
	
	if (pe->reg.B.ptr) pe->reg.B.ptr -= GV_SHIFT;
	if (pe->reg.TR.ptr) pe->reg.TR.ptr -= GV_SHIFT;
	
	
	for (s = pe->reg_stack_base; s < pe->reg_stack_top; s++) {
		if (s->B.ptr) s->B.ptr -= GV_SHIFT;
		if (s->TR.ptr) s->TR.ptr -= GV_SHIFT;
	}
}

void
gv_free(PWord vn)
{
    *(long **) (wm_gvbase - vn) = wm_gvfreelist;
    wm_gvfreelist = wm_gvbase - vn;
}

void
gv_get(PWord *vp, int *tp, PWord vn)
{
    w_get(vp, tp, *(wm_gvbase - vn));
}

void
gv_set(PWord v, int t, PWord vn)
{
    register PWord *newp;
    register PWord *b;
    register PWord *hbl;

    gv_setcnt++;

    /*
     * Install a new variable on the heap if the value is a stack variable.
     * Also update v and t to this new heap variable.
     */

    if (t == WTP_UNBOUND && ((PWord *) v) < wm_heapbase) {
	PWord hv;
	int   ht;

	w_mk_unbound(&hv, &ht);
	w_unify(hv, ht, v, t);
	v = hv;
	t = ht;
    }

    /*
     * Install the new object
     */

    w_install((PWord *) (wm_gvbase - vn), v, t);

    /*
     * Set up newp
     */

    switch (t) {
	case WTP_UNBOUND:
	case WTP_LIST:
	case WTP_STRUCTURE:
	    newp = (PWord *) MBIAS(v);
	    break;
	case WTP_UIA:
	    newp = (PWord *) MBIAS(M_FIRSTUIAWORD(v));
	    break;
	default:
	    return;
	    break;
    }

    /* Need to fix HB to protect the innards of the new object */

    b = wm_B;
    hbl = (PWord *) MBIAS(wm_H);

    while (b != (PWord *) 0 && chpt_HB(b) > newp) {
	hbl = chpt_HB(b);
	b = chpt_B(b);
    }

    while (b != (PWord *) 0) {
	chpt_HB(b) = hbl;
	b = chpt_B(b);
    }

    wm_HB = (wm_B) ? (PWord *) MUNBIAS(chpt_HB(wm_B)) : wm_H;
}
