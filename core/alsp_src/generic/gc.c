/*==================================================================*
 |			gc.c                 
 |		Copyright (c) 1987-1995 Applied Logic Systems, Inc.
 |
 |			-- garbage compactor for ALS Prolog
 |
 | Author:  Kevin A. Buettner
 | Creation Date: 5/24/87
 | Revision History:
 | 08/02/88 - kev -- 88k port
 | 04/25/89 - kev -- port back to sun
 | 10/04/90 - kev -- removed machine-specific designations in port to SPARC
 | 05/14/91 - raman -- amiga port (heap/stack high mem)
 | 04/03/93 - raman -- made it generic
 | 10/27/93 - C. Houpt -- Use ASCII Beep (7) for gcbeep on Mac.
 *==================================================================*/
#include "defs.h"
#include "machinst.h"
#include "wintcode.h"

#undef mask

static unsigned long *marks;

#define TOPTR(w) ((long *) (((long) (w)) & ~MTP_TAGMASK))
#define SETBIT(a,b) *((a)+((b)>>5)) |= 1<<((b)&037)
#define CLRBIT(a,b) *((a)+((b)>>5)) &= ~(1<<((b)&037))
#define TSTBIT(a,b) (*((a)+((b)>>5)) & (1<<((b)&037)))
#define MARK(h) SETBIT(marks,(h)-wm_heapbase),nmarked++
#define MARKED(h) TSTBIT(marks,(h)-wm_heapbase)
#define REV(loc) rev(heap_low,heap_high,(long *)(loc),(long)(loc))

#define FENCEVAL(v) MFENCE_VAL(v)



#define chpt_E(v) env_E(((long) chpt_SPB(v)) & ~1)

/*
 * Whether MTP_CONST is defined or not indicates whether we are using the
 * 88k tagging scheme or not.  If MTP_CONST is defined, then UIAs are
 * represented as offsets instead of real pointers.  The 88k tagging scheme
 * has the advantage in that all pointer objects are more easily represented
 * and in a more uniform manner.
 *
 * In much of the code which follows, there are ifdefs or ifndefs on MTP_CONST.
 * These should be taken to mean that we are using the 88k tagging scheme
 * if MTP_CONST is NOT defined.
 */

#ifndef MTP_CONST
#define ISPTR(h) (!(((long) (h)) & (MTP_INT | MTP_SYM) & ~MTP_BGND))
#define ISFENCE(v) (((v) & MTP_TAGMASK) == MTP_FENCE)
#define ISUIA(v) ((v) & ((MTP_DOUBLE & MTP_UIA) & ~MTP_BGND))
#define ISCONST(h) (((long) (h)) & (MTP_INT | MTP_SYM) & ~MTP_BGND)
#define REVBIT  0x00000001
#define BIAS	(EBIAS/4)
#else  /* MTP_CONST */
#define ISPTR(h) ((((long) (h)) & MTP_TAGMASK) != MTP_CONST)
#define ISFENCE(v) (((v) & MTP_CONSTMASK) == MTP_FENCE)
#define ISUIA(v) (((v) & MTP_CONSTMASK) == MTP_UIA)
#define UIAVAL(v) ((MUIA(v)) >> 2)
#define ISCONST(h) ((((long) (h)) & MTP_TAGMASK) == MTP_INT)
#define REVBIT 0x80000000
#define BIAS 0
#endif /* MTP_CONST */


#ifndef AmigaUNIX
#define LOWMEMORY 1
#endif

#ifdef LOWMEMORY
#define REVERSEIT(targ,v) (((targ) | ((v)&MTP_TAGMASK)) | REVBIT)
#define ISNORMAL(v)	   (!(v & REVBIT))
#define UNREVERSEIT(v)	((v) & ~REVBIT)
#else  /* LOWMEMORY */
/*
 * If the heap and the environment are in the high memory,
 * i.e the first bit of an address is always one,
 * Following macros should be used. -- Ilyas 10/25/89
 */
#define REVERSEIT(targ,v) ((targ | (v&MTP_TAGMASK)) & ~REVBIT)
#define ISNORMAL(v)	   (v <= 0)
#define UNREVERSEIT(v)	(v | REVBIT)
#endif /* LOWMEMORY */

#define ISNORMALUIA(v)	(!(v & REVBIT))



#define env_E(v)   (* (((long **) (v)) + BIAS))
#define env_CP(v)  (* (((Code **) (v)) + BIAS + 1))
#define val_at(v) (* (((long *) (v)) + BIAS))




/*
 * Variables:
 *
 *      mrccp           -- pointer to the Most Recently Compacted Choice Point
 *                         This variable will have its value set after phase 1.
 *      heap_low        -- pointer to lowest cell in uncompacted region of
 *                         heap.  This cell will not necessarily be marked.
 *      heap_high       -- pointer to the highest cell in the uncompacted
 *                         region.  We force this cell to be marked so that
 *                         the HB values may always be updated.
 *      oldest_ap       -- this is chpt_SPB(mrccp)
 *      nmarked         -- counter indicating the number of cells marked
 *      e               -- environment pointer for phase 2
 *      b               -- pointer to choice points in phase 2
 *      ap              -- argument pointer in phase 2
 *      apb             -- argument pointer corresponding to b for phase 2
 *
 */

static long *heap_low;
static long *heap_high;
static long nmarked;

static int gccallcount;		/* number of times gc has been called */

extern int gcbeep;

static	long *	mark_args	PARAMS(( long *, Code * ));
static	void	mark_from	PARAMS(( long * ));
static	void	rev		PARAMS(( long *, long *, long *, long ));
static	void	rev_update	PARAMS(( long *, long ));
static	void	init_marks	PARAMS(( void ));
static	void	mark		PARAMS(( long ));

#include <stdio.h>


int
gc()
{
    long *mrccp;
    long *e;
    long *b;
    long *ap;
    long *apb;
    long *tr;
    long *oldest_ap;
    Code *ra;			/* return address */
    register int i;
    register long *h;
    register unsigned long *mp;
    register unsigned long m;
    int   compactionbit;

    /*
     * Force certain external variables to be biased for gc purposes.  These
     * will be unbiased on exit from gc.
     */

    wm_heapbase -= BIAS;
    wm_H -= BIAS;
    wm_E -= BIAS;
    if (gcbeep) {
	putchar(007);
#ifdef MacOS
	putchar(007);
#else
	putchar(0200);
	putchar(0200);
#endif
	fflush(stdout);
    }

    gccallcount++;

#ifdef GCDEBUG
    printf("\nStarting GC: %d  ", gccallcount);
    fflush(stdout);
#endif /* GCDEBUG */



    /*
     * Phase 1:  Set mrccp, heap_high, heap_low, initialize nmarks and the mark
     *            bits and mark the cell at heap_high.
     *
     * Note on choice points      :
     *            On all implementations other than 386 and Port,
     *            the HB, SPB, and Fail values for the most recent
     *            choice point live in prolog registers. Which means
     *            that if you are staring at a choice point, then the
     *            actual HB, SPB and Fail values are in the choice
     *            point that is more recent that it. The top most
     *            choice point is a dummy that holds the HB, SPB and
     *            Fail for the one below it.
     *
     */

#if defined(arch_i386) || defined(Portable)
#define ChptBeforeTrail 1
#else
#define ChptAfterTrail  1
#endif

#ifdef ChptAfterTrail
    b =
#endif
    mrccp = wm_B;

    if (gv_setcnt) {

	while (chpt_B(mrccp) != (long *) 0) {
	    mrccp = chpt_B(mrccp);
	}

	heap_low = wm_heapbase;
	compactionbit = 0;
	gv_setcnt = 0;
    }
    else {

	while (chpt_B(mrccp) != (long *) 0 && !chpt_COMPACTED(mrccp)) {
#ifdef ChptAfterTrail
	    b = mrccp;
#endif
	    mrccp = chpt_B(mrccp);
	}

	if (chpt_COMPACTED(mrccp))
#ifdef ChptAfterTrail
	    heap_low = chpt_HB(b);
#else
	    heap_low = chpt_HB(mrccp);
#endif
	else
	    heap_low = wm_heapbase;

#ifdef ChptAfterTrail
	mrccp = b;
#endif

	compactionbit = 1;
    }

    oldest_ap = (long *) ((long) chpt_SPB(mrccp) & ~1);
    heap_high = wm_H;
    val_at(heap_high) = MTP_INT;	/* put constant in free heap location
					 */
    nmarked = 0;
    init_marks();
    mark((long) heap_high);		/* mark it                      */

    /*
     * Phase 2:   Mark from arguments, environments, trail cells, and
     *            cells in compacted region of the heap which point up to
     *            uncompacted region.  Also do pointer reversal for all of these
     *            areas.
     *
     */


    ap = wm_E;
    b = wm_B;
    tr = wm_TR;

    while (b <= mrccp) {
	/* nuke the compaction bit */
	apb = (long *) (((long) chpt_SPB(b)) & ~1);

#ifdef ChptAfterTrail
	tr = b + chpt_SIZE;	/* get tr and b set up for next iteration */
	b = chpt_B(b);
	if (b == (long *) 0)	/* exit early if necessary */
	    break;
#endif

	e = env_E(ap);

	if (apb <= e) {
	    for (ap += 2; ap < apb; ap++)
		mark_from(ap);
	}
	else {
	    ra = env_CP(ap);
	    for (ap += 2; ap < e; ap++)
		mark_from(ap);
	    while (e < apb) {
		ap = mark_args(e, ra);
		ra = env_CP(e);
		e = env_E(e);

		if (apb < e)
		    for (; ap < apb; ap++)
			mark_from(ap);
		else
		    for (; ap < e; ap++)
			mark_from(ap);
	    }
	}

	for (; tr < b; tr++) {
	    ap = (long *) *tr;	/* tr is not biased */
	    if (oldest_ap <= ap && ap < heap_low)
		mark_from(ap);
	}

	ap = apb;

#ifdef ChptBeforeTrail
	tr = b + chpt_SIZE;	/* get tr and b set up for next iteration */
	b = chpt_B(b);
	if (b == (long *) 0)	/* exit early if necessary */
	    break;
#endif
    }

    /*
     * Phase 2.5:
     *            Mark from global variables.
     *
     *            Note that wm_trailbase and wm_gvbase are unbiased so ap will
     *            also be unbiased.  Therefore the argument passed into mark_from
     *            must be explicitly biased.
     */

    for (ap = wm_trailbase; ap < wm_gvbase; ap++)
	mark_from(ap - BIAS);


    /*
     * Phase 3: Now everything should be marked.  We need to traverse the
     *            choice point chain and locate new positions for the HB
     *            values.  Due to the way that global variables work, trail
     *            entries which point into regions of the uncompacted heap
     *            can also be garbaged.  So we must compact the trail at the
     *            same time.  Unfortunately, we must also perform the usual
     *            trail compaction in cut to get rid of garbaged trail entries
     *            which point at the stack.  These cannot always be determined
     *            in the garbage collector.
     *
     *            In the following code:
     *                    b is the lead pointer.
     *                    tr is the follow pointer.
     *                    apb is use to keep track of where the previous chpt is
     *
     */

    b = tr = mrccp;
    apb = chpt_B(b);

#ifdef ChptBeforeTrail
    *(wm_TR - 1) = (PWord) mrccp;	/* Fake chpt_B entry */
#else
    if (apb != (long *) 0) {
	b = tr = apb;
	goto chpt_after_trail_entry;
    }
#endif

    for (;;) {

	/* Update the choice point */

	h = chpt_HB(b);		/* get hb */
	chpt_B(tr) = apb;	/* update the b value */
	ra = chpt_NextClause(b);	/* get Fail address */

	chpt_SPB(tr) = (long *) (((long) chpt_SPB(b)) | compactionbit);
	/* update SBP as compacted */
	chpt_NextClause(tr) = ra;	/* put Fail address */


	/* Compute new value of HB */

	i = 32 - ((h - wm_heapbase) & 037);	/* figure out where new  */
	mp = marks + ((h - wm_heapbase) >> 5);	/* HB is by looking for  */
	m = *mp++ >> (32 - i);	/* a marked heap location */
	/* which is greater or   */
	for (;; h += i, i = 32, m = *mp++)	/* equal to the HB    */
	    for (; m && i; i--, h++, m >>= 1)
		if (m & 1)
		    goto foundHB;

foundHB:

	chpt_HB(tr) = h;	/* set new HB value */
	REV(&chpt_HB(tr) - BIAS);	/* and reverse it */


	apb = tr;		/* this will be prev chpt */

#ifdef ChptAfterTrail
	if (b == wm_B)		/* break if processed top chpt */
	    break;		/* location of this exit is critical */

chpt_after_trail_entry:	/* entry point into for-loop */

#endif

	/* Process trail entries until we detect the bottom of a chpt */

	while ((ap = (long *) *--b) < b) {	/* while ap is a trail entry */
	    if (heap_low <= ap && ap <= heap_high) {
		if (MARKED(ap)) {
		    *--tr = (long) ap;	/* copy the trail entry */
		    REV(tr - BIAS);	/* and reverse it */
		}
	    }
	    else {		/* else in compacted heap or on stack */
		*--tr = (long) ap;	/* copy the trail entry */
	    }
	}

	b -= (chpt_SIZE - 1);	/* set b to start of chpt */

#ifdef ChptBeforeTrail
	if (b < wm_B)		/* break if reached fake chpt */
	    break;		/* location of this exit is critical */
#endif

	tr -= chpt_SIZE;	/* make room for chpt */

    }


    wm_B = apb;			/* update B */
    wm_TR = tr;			/* update TR */


    /* begin block for phases 4, 5, and 6 */
    {
	register unsigned long *mstop;
	register long *nh;
	long  v;

	/*
	 * Phase 4:     Sweep the heap from high to low in order to fix
	 *              the downward pointers and pointers from the outside.
	 */


	h = wm_heapbase + ((heap_high - wm_heapbase) | 037);
	nh = heap_low + nmarked - 1;
	mp = marks + ((heap_high - wm_heapbase) >> 5);
	mstop = marks + ((heap_low - wm_heapbase) >> 5);

	for (; mp >= mstop; mp--, h -= i)
	    for (i = 32, m = *mp; i && m; i--, h--, m <<= 1)
		if (((long) m) < 0) {	/* if high bit is set... */
		    if (ISFENCE(val_at(h))) {
			v = FENCEVAL(val_at(h));
			nh -= v;
			h -= v;
			i = 1 + ((h - wm_heapbase) & 037);
			mp = marks + ((h - wm_heapbase) >> 5);
			m = *mp << (32 - i);
		    }
		    rev_update(h, (long)nh);
		    rev(heap_low, h - 1, h, (long)h);
		    nh--;
		}


	/*
	 * Phase 5:     Sweep the heap from low to high in order to reverse
	 *              the upward pointers.  Perform actual compaction
	 *              at the same time.
	 */

	h = wm_heapbase + ((heap_low - wm_heapbase) & ~037);
	nh = heap_low;
	mp = marks + ((heap_low - wm_heapbase) >> 5);
	mstop = marks + ((heap_high - wm_heapbase) >> 5);

	for (; mp <= mstop; mp++, h += i)
	    for (i = 32, m = *mp; i && m; i--, h++, m >>= 1)
		if (m & 1) {	/* if low bit is set... */
		    rev_update(h, (long)nh);
		    if (ISFENCE(val_at(h))) {
			v = val_at(h);
			val_at(nh) = v;
			nh++;
			v = FENCEVAL(v);
			h++;
			for (; --v; nh++, h++)
			    val_at(nh) = val_at(h);
			val_at(nh) = val_at(h);
			i = 32 - ((h - wm_heapbase) & 037);
			mp = marks + ((h - wm_heapbase) >> 5);
			m = *mp >> (32 - i);
		    }
		    else {
			v = val_at(h);
			if (ISPTR(v) && h == TOPTR(v)) {
			    val_at(nh) = ((long) nh) | (v & MTP_TAGMASK);
			}
			else {
			    rev(h + 1, heap_high, h, (long)nh);
			    val_at(nh) = val_at(h);
			}
		    }
		    nh++;
		}


	/*
	 * Phase 6:     Set H and HB.  Compaction is complete.
	 *
	 *      Note:   On the 88k implementation, a choice point is created just prior
	 *              to calling gc so that H and HB should have the same value.
	 */

	wm_H = nh - 1 + BIAS;
#ifdef ChptBeforeTrail
	wm_HB = chpt_HB(wm_B);
#else
	wm_HB = wm_H;
#endif

    }				/* end of block for phases 4, 5, and 6 */

    /*
     * Unbias E and heapbase; note that H and HB were done above.
     */

    wm_E += BIAS;
    wm_heapbase += BIAS;

#ifdef GCDEBUG
    printf("Compaction complete: nmarked=%d \n", nmarked);
#endif /* GCDEBUG */
    return 1;
}




static long *
mark_args(e, ra)
    long *e;
    Code *ra;			/* return address */
{
    long  mask;			/* mask           */
    int   nargs;
    int   cutoff;

#ifdef GCMASK
    nargs = *(ra - 2) & 0xffff;
    mask = *(ra - 1);

    if ((*(ra - 3) & GCMASK) != GCMAGIC) {
	fprintf(stderr, "gc: Return address doesn't point at GCMAGIC\n");
	fprintf(stderr, "nargs=%x mask=%lx ra=%lx\n", nargs, mask, (long) ra);
	als_exit(1);
    }

#else  /* GCMASK */
#ifdef MacOS
    if ((*ra != GCMAGIC) && (*ra != 0x303c)) {
	fprintf(stderr, "gc: Return address doesn't point at GCMAGIC\n");
	als_exit(1);
    }
#else
    if (*ra != GCMAGIC) {
	fprintf(stderr, "gc: Return address doesn't point at GCMAGIC\n");
	als_exit(1);
    }
#endif


    /* Currently GCMAGIC has a long argument. */
    mask = *((long *) (ra + GCMAGICVal(ra)) + 1);
    nargs = (*(long *) (ra + GCMAGICVal(ra))) & 0xffff;
#endif /* GCMASK */

    cutoff = nargs - 32;

    for (e += 2; nargs--; e++) {
	if ((mask & 1) || nargs < cutoff)
	    mark_from(e);
	mask >>= 1;
    }
    return e;
}

static void
mark_from(loc)
    long *loc;
{
    mark(val_at(loc));
    REV(loc);
}


/*
 * rev takes a single parameter, loc, which is assumed to contain a pointer
 * to a marked heap location.  It "reverses" the pointer (provided it falls
 * in between lo and hi inclusive) by transferring the contents of
 * the heap location to loc and installing in the heap location
 * a pointer back to targ.  The tag bits are transferred to this new pointer
 * and the pointer is marked as reversed.
 *
 */

static void
rev(lo, hi, loc, targ)
    long *lo, *hi, *loc, targ;
{
    long *ptr;
    long  v;

    v = val_at(loc);
    if (ISPTR(v)
#ifndef MTP_CONST
	|| ISUIA(v)
#endif /* MTP_CONST */
	) {
	ptr = TOPTR(v);
	if (lo <= ptr && ptr <= hi) {
	    val_at(loc) = val_at(ptr);
	    val_at(ptr) = REVERSEIT(targ, v);
	}
    }
#ifdef MTP_CONST
    else if ((v & MTP_CONSTMASK) == MTP_UIA) {
	ptr = wm_heapbase + UIAVAL(v);
	if (lo <= ptr && ptr <= hi) {
	    val_at(loc) = val_at(ptr);
	    val_at(ptr) = MMK_UIA(((char *) targ) - ((char *) wm_gvbase));
	}
    }
#endif /* MTP_CONST */
}


/*
 * rev_update is called from the compaction phases of gc.  It is responsible
 * for updating a list of reversed pointers to the new location (targ).  loc
 * is the heap location that may (or may not) contain a list of reversed
 * pointers.
 */

static void
rev_update(loc, targ)
    long *loc, targ;
{
    long  v, tags, *ptr, temp;


    v = val_at(loc);		/* get initial value out of loc */
    for (;;) {
	tags = v & MTP_TAGMASK;	/* get the tags                 */
#ifndef MTP_CONST
	if (ISCONST(tags) && !(ISUIA(tags))) {
	    break;
	}
#else  /* MTP_CONST */
	if (ISCONST(v)) {
	    if (!ISUIA(v) || ISNORMALUIA(v))
		break;
	    ptr = wm_gvbase + (UIAVAL(v));
	    v = val_at(ptr);
	    val_at(ptr) = MMK_UIA(((char *) targ) - ((char *) wm_heapbase));

	}
#endif /* MTP_CONST */
	else {
	    if (ISNORMAL(v))
		break;		/* break if not reversed        */
	    temp = UNREVERSEIT(v);	/* strip reverse bits and tags  */
	    ptr = TOPTR(temp);	/* off of v and put in ptr      */
	    /* (in 2 lines to avoid HighC bug)  */

	    v = val_at(ptr);	/* put contents of "ptr" in v   */
	                        /* for next pass around the loop */
	    val_at(ptr) = targ | tags;	/* fix up contents of "ptr" to  */
	                                /* the new location             */
	}
    }
    val_at(loc) = v;		/* put value at end of chain    */
    /* of reversed pointers back in */
    /* loc                          */
}


static void
init_marks()
{
    register unsigned long *m;

    marks = (unsigned long *) prs_area;
    m = marks + ((wm_H - wm_heapbase) / 32) + 1;

    while (m > marks) {
	*--m = 0;
    }

}

static void
mark(val)
    register long val;
{
    register long *ptr, *bptr;
    register long tag, btag;
    int   arity;


    bptr = (long *) 0;
    btag = 0;

mark_top:
    tag = val & MTP_TAGMASK;

    if (ISCONST(val)) {
	if (ISUIA(val)) {
#ifndef MTP_CONST
	    ptr = TOPTR(val);
#else  /* MTP_CONST */
	    ptr = wm_heapbase + UIAVAL(val);
#endif /* MTP_CONST */
	    if (ptr < heap_low || ptr > heap_high)
		goto mark_backup;
	    if (MARKED(ptr))
		goto mark_backup;
	    MARK(ptr);		/* mark front fence */
	    tag = FENCEVAL(val_at(ptr));	/* get distance between
						 * fences
						 */
	    nmarked += tag - 1;	/* update nmarked for stuff between */
	    ptr += tag;		/* move to back fence */
	    MARK(ptr);		/* mark back fence */
	}
	goto mark_backup;
    }

    ptr = TOPTR(val);
    if (ptr < heap_low || ptr > heap_high)
	goto mark_backup;

    switch (tag) {

	case MTP_UNBOUND:
	    if (MARKED(ptr))
		goto mark_backup;
mark_follow:
	    MARK(ptr);
	    val = val_at(ptr);
	    val_at(ptr) = ((long) bptr) | btag;
	    btag = tag;
	    bptr = ptr;
	    goto mark_top;

	case MTP_STRUCT:
	    if (MARKED(ptr))
		goto mark_backup;
	    MARK(ptr);
	    arity = MFUNCTOR_ARITY(val_at(ptr));
#ifdef BigStruct
	    if (arity == ESCAPE_ARITY) {
		ptr += 1;
		arity = MINTEGER(val_at(ptr));
	    }
#endif
	    ptr += arity;
mark_args:
	    val = val_at(ptr);
	    if (M_ISSYM(val) && (MFUNCTOR_ARITY(val) != 0)) {
		val = ((long) ptr) | MTP_STRUCT;
		goto mark_backup;
	    }
	    if (MARKED(ptr)) {
		ptr--;
		goto mark_args;
	    }
	    MARK(ptr);
	    val_at(ptr) = ((long) bptr) | btag;
	    btag = tag;
	    bptr = ptr;
	    goto mark_top;

	case MTP_LIST:
	    if (MARKED(ptr)) {
mark_cdr:
		if (MARKED(ptr + 1)) {
		    val = ((long) ptr) | MTP_LIST;
		    goto mark_backup;
		}
		ptr++;
		tag = MTP_INT;
	    }
	    goto mark_follow;
    }

    /* We should never fall through */

mark_backup:
    ptr = bptr;
    if (ptr == (long *) 0)
	return;			/* done marking */
    tag = btag;
    btag = val_at(ptr);
    bptr = TOPTR(btag);
    btag &= MTP_TAGMASK;
    val_at(ptr) = val;

    switch (tag) {
	case MTP_UNBOUND:
	    val = ((long) ptr) | tag;
	    goto mark_backup;
	case MTP_STRUCT:
	    ptr--;
	    goto mark_args;
	case MTP_LIST:
	    goto mark_cdr;
	case MTP_INT:
	    val = ((long) (ptr - 1)) | MTP_LIST;
	    goto mark_backup;
    }
}

#if 0
pwrite(pv)
    long  pv;
{
    long  v, t;

    w_get(&v, &t, pv);
    printf("\n");
    prolog_writeq(v, t);
    printf("\n");
}



int
gcstats()
{
    return gccallcount;
}


void
print_chpts(b)
    register long *b;
{
    while (b) {
	printf("%lx:%10lx%10lx%10lx%10lx\n",
	       (long) b,
	       (long) chpt_NextClause(b),
	       (long) chpt_HB(b),
	       (long) chpt_SPB(b),
	       (long) chpt_B(b));
	b = chpt_B(b);
    }
}
#endif
