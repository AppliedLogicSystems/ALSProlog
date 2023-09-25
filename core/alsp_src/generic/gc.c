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
#ifdef FREEZE
#include "freeze.h"
#endif

#ifdef DEBUGSYS /*--------------------------------------------------*/
#include <time.h>
#endif /* ---------------------------------------------- DEBUGSYS --*/

#if 0 /* RH6 */
#if defined(HAVE_SIGACTION) && defined(SA_SIGINFO)
extern void stack_overflow  ( int, siginfo_t *, ucontext_t * );
#elif defined(HAVE_SIGVEC) || defined(HAVE_SIGVECTOR)
extern void    stack_overflow  ( int, int, struct sigcontext *, caddr_t );
#elif defined(Portable)
extern void   stack_overflow  ( int );
#else
#error
#endif
#endif

#undef mask

static unsigned long *marks;

#define TOPTR(w)    ((long *) (((long) (w)) & ~MTP_TAGMASK))
#define SETBIT(a,b) *((a)+((b)>>5)) |= 1<<((b)&037)
#define CLRBIT(a,b) *((a)+((b)>>5)) &= ~(1<<((b)&037))
#define TSTBIT(a,b) (*((a)+((b)>>5)) & (1<<((b)&037)))
#define MARK(h)     SETBIT(marks,(h)-wm_heapbase),nmarked++
#define MARKED(h)   TSTBIT(marks,(h)-wm_heapbase)
#define REV(loc)    rev(heap_low,heap_high,(long *)(loc),(long)(loc))

#define FENCEVAL(v) MFENCE_VAL(v)

#define chpt_E(v) env_E(((long) chpt_SPB(v)) & ~1)

/*---------------------------------------------------------------------------------*
 | Whether MTP_CONST is defined or not indicates whether we are using the
 | 88k tagging scheme or not.  If MTP_CONST is defined, then UIAs are
 | represented as offsets instead of real pointers.  The 88k tagging scheme
 | has the advantage in that all pointer objects are more easily represented
 | and in a more uniform manner.
 |
 | In much of the code which follows, there are ifdefs or ifndefs on MTP_CONST.
 | These should be taken to mean that we are using the 88k tagging scheme
 | if MTP_CONST is NOT defined.
 *---------------------------------------------------------------------------------*/

#ifndef MTP_CONST
		/* 88K */
#define ISPOINTER(h)   (!(((long) (h)) & (MTP_INT | MTP_SYM) & ~MTP_BGND))
#define ISFENCE(v) (((v) & MTP_TAGMASK) == MTP_FENCE)
#define ISUIA(v)   ((v) & ((MTP_DOUBLE & MTP_UIA) & ~MTP_BGND))
#define ISCONST(h) (((long) (h)) & (MTP_INT | MTP_SYM) & ~MTP_BGND)
#define REVBIT     0x00000001
#define BIAS	   (EBIAS/4)

#else  /* MTP_CONST */
#define ISPOINTER(h)   ((((long) (h)) & MTP_TAGMASK) != MTP_CONST)
#define ISFENCE(v) (((v) & MTP_CONSTMASK) == MTP_FENCE)
#define ISUIA(v)   (((v) & MTP_CONSTMASK) == MTP_UIA)
#ifdef __LP64__
#define UIAVAL(v)  ((MUIA(v)) >> 3)
#else
#define UIAVAL(v)  ((MUIA(v)) >> 2)
#endif
#define ISCONST(h) ((((long) (h)) & MTP_TAGMASK) == MTP_INT)
#ifdef __LP64__
#define REVBIT     0x8000000000000000
#else
#define REVBIT     0x80000000
#endif
#define BIAS       0

#endif /* MTP_CONST */

/* Reverse pointers are tagged by having their most significant bit inverted.
   In order for this to work correctly, heap memory must completely reside in
   either the lower or upper half of address space.
 */


#ifdef __LP64__
#define HIBIT_MASK 0x8000000000000000
#define ADDR_MASK  0x7FFFFFFFFFFFFFFF
#else
#define HIBIT_MASK 0x80000000
#define ADDR_MASK  0x7FFFFFFF
#endif
#define REVERSEIT(targ, v) (~((targ) | ADDR_MASK) | ((targ) & ADDR_MASK) | ((v) & MTP_TAGMASK))
#define ISNORMAL(ptr, v) ((((unsigned long)ptr) & HIBIT_MASK) == ((v) & HIBIT_MASK))
/*#define ISNORMAL(ptr, v) (!(~(((unsigned long)ptr) | ADDR_MASK) & v))*/
#define UNREVERSEIT(v) (~((v) | ADDR_MASK) | ((v) & ADDR_MASK))

#define ISNORMALUIA(v)	(!(v & REVBIT))

#define env_E(v)   (* (((long **) (v)) + BIAS))
#define env_CP(v)  (* (((Code **) (v)) + BIAS + 1))
#define val_at(v)  (* (((long *) (v)) + BIAS))

/*---------------------------------------------------------------------------------*
 | Variables:
 |
 |      mrccp           -- pointer to the Most Recently Compacted Choice Point
 |                         This variable will have its value set after phase 1.
 |      heap_low        -- pointer to lowest cell in uncompacted region of
 |                         heap.  This cell will not necessarily be marked.
 |      heap_high       -- pointer to the highest cell in the uncompacted
 |                         region.  We force this cell to be marked so that
 |                         the HB values may always be updated.
 |      oldest_ap       -- this is chpt_SPB(mrccp)
 |      nmarked         -- counter indicating the number of cells marked
 |      e               -- environment pointer for phase 2
 |      b               -- pointer to choice points in phase 2
 |      ap              -- argument pointer in phase 2
 |      apb             -- argument pointer corresponding to b for phase 2
 *---------------------------------------------------------------------------------*/

static long *heap_low;
static long *heap_high;
static long nmarked;

#ifdef DEBUGSYS /*--------------------------------------------------*/
static int gccallcount;		/* number of times gc has been called */
extern int gcbeep;
#endif /* ---------------------------------------------- DEBUGSYS --*/

static	long *	mark_args	( long *, Code * );
static	void	mark_from	( long * );
static	void	rev		    ( long *, long *, long *, long );
static	void	rev_update	( long *, long );
static	void	init_marks	( void );
static	void	mark		( long );

#ifdef INTCONSTR
static	int	core_gc		( void );
#endif

#include <stdio.h>

	/*-----------------------------------------------------
	 | core_gc()
	 *----------------------------------------------------*/

#ifdef INTCONSTR
int
core_gc(void)
#else
int
gc(void)
#endif
{
    long *mrccp;
    long *e;
    long *b;
    long *ap;
    long *apb;
    long *tr;
#ifdef TRAILVALS
    long *tr1;
#endif
    long *oldest_ap;
    Code *ra;			/* return address */
#ifdef __LP64__
    register long i;
#else
    register int i;
#endif
    register long *h;
    register unsigned long *mp;
    register unsigned long m;
#ifdef __LP64__
    long   compactionbit;
#else
    int   compactionbit;
#endif

// TODO LP64: GC is not 64-bit yet
#ifdef __LP64__
printf("gc not 64-bit yet\n");
return 1;
#endif

#ifdef DEBUGSYS /*--------------------------------------------------*/
    unsigned long start_tick = 0, finish_tick;
#endif /* ---------------------------------------------- DEBUGSYS --*/

	/* For demos and hardware-key protected versions, check copy protection. */
    ASSERT(prolog_engine_invariant(&current_engine));

    /*---------------------------------------------------------------*
     | Force certain external variables to be biased for gc purposes.
     | These will be unbiased on exit from gc.
     *---------------------------------------------------------------*/

    wm_heapbase_lvalue -= BIAS;
    wm_H -= BIAS;
    wm_E -= BIAS;

#ifdef DEBUGSYS /*--------------------------------------------------*/
	if (debug_system[GCBEEP]) {
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

	if (debug_system[GCINFO]) {
    	printf("\nStarting GC: %d  \n", gccallcount);
		pbi_cptx();
/*		pbi_walk_cps(); */
/*		pbi_swp_tr();  */
    	fflush(stdout);
    	start_tick = clock();
	}
#endif /* ---------------------------------------------- DEBUGSYS --*/

    /*-------------------------------------------------------------------------*
     | Phase 1: Set mrccp, heap_high, heap_low, initialize nmarks and the mark
     |          bits and mark the cell at heap_high.
     |
     | Note on choice points:
     |          On all implementations other than 386 and Port,
     |          the HB, SPB, and Fail values for the most recent
     |          choice point live in prolog registers. Which means
     |          that if you are staring at a choice point, then the
     |          actual HB, SPB and Fail values are in the choice
     |          point that is more recent that it. The top most
     |          choice point is a dummy that holds the HB, SPB and
     |          Fail for the one below it.
     *-------------------------------------------------------------------------*/

#if defined(arch_i386) || defined(Portable)
#define ChptBeforeTrail 1

#else  /* not-arch_i386 & not-Portable */
#define ChptAfterTrail  1

#endif

#ifdef ChptAfterTrail
    b =
#endif
    mrccp = wm_B;

			/*-------------------------------------------------------------------*
			 | If any global variable has been set since the last gc, set mrccp
			 | to be the very oldest choice point.
			 *-------------------------------------------------------------------*/
    if (gv_setcnt) {
		while (chpt_B(mrccp) != (long *) 0) {
	    	mrccp = chpt_B(mrccp);
		}
	heap_low = wm_heapbase;
	compactionbit = 0;
	gv_setcnt = 0;
    }
    else 	/* !gv_setcnt */
	{
			/*-------------------------------------------------------------------*
			 | If no global variable has been set since the last gc, set mrccp
			 | to be the most recent choice point which has been compacted
			 | (which, of course, could turn out to be the very oldest cp)
			 *-------------------------------------------------------------------*/
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

    }	/* if(gv_setcnt)-else */

    oldest_ap =         (long *) ((long) chpt_SPB(mrccp) & ~1);
    heap_high =         wm_H;
    val_at(heap_high) = MTP_INT;	/* put constant in free heap location */
    nmarked =           0;

    init_marks();
    mark((long) heap_high);		    /* mark it  */



    /*-----------------------------------------------------------------------*
     | Phase 2:   Mark from arguments, environments, trail cells, and
     |            cells in compacted region of the heap which point up to
     |            uncompacted region.  Also do pointer reversal for all of
     |            these areas.
     *-----------------------------------------------------------------------*/

    ap = wm_E;
    b =  wm_B;
    tr = wm_TR;

    while (b <= mrccp) {
			/* nuke the compaction bit */
		apb = (long *) (((long) chpt_SPB(b)) & ~1);

#ifdef ChptAfterTrail
		tr =  b + chpt_SIZE;	/* get tr and b set up for next iteration */
		b =   chpt_B(b);
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
	    	}  /* while (e < apb)  */
		}      /* if (apb <= e)-else */

#ifdef TRAILVALS
	/*------------------------------------------------------------------*
	 | mark_from(L) expects L to be (an ordinary C) pointer to a location
	 | holding a Prolog (heap) value; since tr points at a trail location,
	 | then *tr is such a value for the ordinary (non-TRAILVALS) situtation;
	 | However, in the TRAILVALS case, this is only true for the (upper)
	 | one of each pair of entries on the trail.  In the lower member
	 | of a trail pair, we have copied the contents of a heap location into
	 | that trail position; hence *tr is incorrect for such a location;
	 | instead, we want to just pass tr to mark_from in that case.
	 |		Note that in this case, the contents of tr, being a heap
	 |	value, could be biased; hence we use val_at(tr) in this case.
	 |
	 |     Now, tr is sweeping from lower (more recent) to upper locations
	 | on the trail; thus it encounters the lower member (copied value
	 | to be reset) first, then the upper regular type of trail location
	 | second.  Note that any of these copied (2nd pair member) values
	 | might be needed on backtracking, and so must be preserved under
	 | gc.
	 *------------------------------------------------------------------*/

		for (; tr < b; tr++) {
			/* hold onto the copied (2nd pair element) value): */
		tr1 = tr;
			/* move up one element; now an "ordinary" trail value: */ 
	    ap = (long *) *++tr;	/* tr is not biased */
	    if (oldest_ap <= ap && ap < heap_low)
			{
/*			    mark_from(ap);
   			mark(val_at(tr1));            */
    			    mark_from(tr);
    			    mark_from(tr1);
			}
		}

#else /* no-TRAILVALS */

		for (; tr < b; tr++) {
	    	ap = (long *) *tr;			/* tr is not biased */
	    	if (oldest_ap <= ap && ap < heap_low)
				mark_from(ap);
		}

#endif    /* TRAILVALS */

		ap = apb;

#ifdef ChptBeforeTrail
		tr = b + chpt_SIZE;	/* get tr and b set up for next iteration */
		b =  chpt_B(b);
		if (b == (long *) 0)	/* exit early if necessary */
	    	break;
#endif
    }	/* while (b <= mrccp) */

    /*----------------------------------------------------------------------*
     | Phase 2.5: Mark from global variables.
     |     Note that wm_trailbase and wm_gvbase are unbiased so ap will
     |     also be unbiased.  Therefore the argument passed into mark_from
     |     must be explicitly biased.
     *----------------------------------------------------------------------*/
    for (ap = wm_trailbase; ap < wm_gvbase; ap++)
	{
		mark_from(ap - BIAS);
	}

    /*-----------------------------------------------------------------------*
     | Phase 3: Now everything should be marked.  We need to traverse the
     |          choice point chain and locate new positions for the HB
     |          values.  Due to the way that global variables work, trail
     |          entries which point into regions of the uncompacted heap
     |          can also be garbaged.  So we must compact the trail at the
     |          same time.  Unfortunately, we must also perform the usual
     |          trail compaction in cut to get rid of garbaged trail entries
     |          which point at the stack.  These cannot always be determined
     |          in the garbage collector.
     |
     |          In the following code:
     |             b is the lead pointer.
     |             tr is the follow pointer.
     |             apb is use to keep track of where the previous chpt is
	 |
	 |	The primary for-loop steps from choice point to choice point, working
	 |	its way down from mrccp to the most recent (topmost) choice point.
	 |  At each step of the loop, b initially points to the lowest (start)
	 |  point of a cp, so we can do things like apply chpt_HB() to b;
	 |  During the course of one interation, we decrement b (-- or two --'s)
	 |  until it hits the top of the next choice point (or steps off the
	 |  cp_stack/trail.
     *-----------------------------------------------------------------------*/

    b = tr = mrccp;
    apb = chpt_B(b);

#ifdef ChptBeforeTrail
/* printf("Fake chpt_B entry: wmTR-1=%0x mrccp=%0x\n",wm_TR - 1,mrccp); */
    *(wm_TR - 1) = (PWord) mrccp;	/* Fake chpt_B entry */
#else
    if (apb != (long *) 0) {
		b = tr = apb;
		goto chpt_after_trail_entry;
    }
#endif

    for (;;) {

		/* -- Update the (current) choice point */

		h = chpt_HB(b);				/* get hb */
		chpt_B(tr) = apb;			/* update the b value */
		ra = chpt_NextClause(b);	/* get Fail address */

		chpt_SPB(tr) = (long *) (((long) chpt_SPB(b)) | compactionbit);

		/* -- Update SBP as compacted */

		chpt_NextClause(tr) = ra;	/* put Fail address */

		/* -- Compute new value of HB */

		i = 32 - ((h - wm_heapbase) & 037);		/* figure out where new  	*/
		mp = marks + ((h - wm_heapbase) >> 5);	/* HB is by looking for  	*/
		m = *mp++ >> (32 - i);	 				/* a marked heap location	*/
												/* which is greater or   	*/
		for (;; h += i, i = 32, m = *mp++)		/* equal to the HB    		*/
	    	for (; m && i; i--, h++, m >>= 1)
				if (m & 1)
		    		goto foundHB;

foundHB:

		chpt_HB(tr) = h;			/* set new HB value */
		REV(&chpt_HB(tr) - BIAS);	/* and reverse it */

		apb = tr;					/* this will be prev chpt */

#ifdef ChptAfterTrail
		if (b == wm_B)		/* break if processed top chpt */
	    	break;			/* location of this exit is critical */

chpt_after_trail_entry:	/* entry point into for-loop */

#endif

			/* -- Process trail entries until we detect the bottom of a chpt */
    		/* long *b,  *tr */

			/*---------------------------------------------------------------*
			 | The pair (b, tr) sweeps down from the bottom of one cp to the
			 | top of the next (younger) cp; at each step, b is advanced
			 | first, and then tr.
			 *---------------------------------------------------------------*/
#ifdef TRAILVALS
			/*---------------------------------------------------------------*
			 | Since we are moving physically down the cp_stack/trail, b
			 | encounters the ordinary trail entry part of a pair first; then
			 | the copied value part lies below that.
			 *---------------------------------------------------------------*/
		for (ap = (long *) *--b; ap < b ; ap = (long *) *--b) {		/* while b not the top word in next cp */
		/* was: while ((ap = ((long *) *--b)) < b) see: http://c-faq.com/expr/evalorder2.html */
	    	if (heap_low <= ap && ap <= heap_high) {
				if (MARKED(ap)) {
		    		*--tr = (long) ap;			/* copy the trail entry */
		    		REV(tr - BIAS);				/* and reverse it 		*/
					b -= 1;
		    		*--tr = (long) *b;			/* copy the trail value entry */
		    		REV(tr - BIAS);				/* and reverse it 		*/
				}
				else
					b -= 1;
	    	}
	    	else {								/* else in compacted heap or on stack */
				*--tr = (long) ap;				/* copy the trail entry */
				b -= 1;
		    	*--tr = (long) (long *) *b;		/* copy the trail value entry */
	    	}
		}	/* for(ap = ...) */


#else /* no-TRAILVALS */

		for (ap = (long *) *--b; ap < b ; ap = (long *) *--b) {		/* while b not the top word in next cp */
		/* was: while ((ap = ((long *) *--b)) < b) see: http://c-faq.com/expr/evalorder2.html */
	    	if (heap_low <= ap && ap <= heap_high) {
				if (MARKED(ap)) {
		    		*--tr = (long) ap;			/* copy the trail entry */
		    		REV(tr - BIAS);				/* and reverse it 		*/
				}
	    	}
	    	else {								/* else in compacted heap or on stack */
				*--tr = (long) ap;				/* copy the trail entry */
	    	}
		}	/* for(ap = ...) */

#endif    /* TRAILVALS */

		b -= (chpt_SIZE - 1);					/* set b to start of chpt */

#ifdef ChptBeforeTrail
		if (b < wm_B)							/* break if reached fake chpt */
	    	break;								/* location of this exit is critical */
#endif

		tr -= chpt_SIZE;						/* make room for chpt */

    }	/* for (;;) */


    wm_B  = apb;		/* update B */
    wm_TR = tr;			/* update TR */

		/* ---------------------------------------- */
    	/* -- Begin block for phases 4, 5, and 6 -- */
		/* ---------------------------------------- */
    {
	register unsigned long *mstop;
	register long *nh;
	long  v;

	/*---------------------------------------------------------------*
	 | Phase 4:  Sweep the heap from high to low in order to fix
	 |           the downward pointers and pointers from the outside.
	 *---------------------------------------------------------------*/

	h     = wm_heapbase + ((heap_high - wm_heapbase) | 037);
	nh    = heap_low + nmarked - 1;
	mp    = marks + ((heap_high - wm_heapbase) >> 5);
	mstop = marks + ((heap_low - wm_heapbase) >> 5);

	for (; mp >= mstop; mp--, h -= i)
	    for (i = 32, m = *mp; i && m; i--, h--, m <<= 1)
		if (((long) m) < 0) {				/* if high bit is set... */
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
		}	/* if (((long m) < 0) */

	/*-------------------------------------------------------------------*
	 | Phase 5: Sweep the heap from low to high in order to reverse
	 |          the upward pointers.  Perform actual compaction
	 |          at the same time.
	 *-------------------------------------------------------------------*/

	h     = wm_heapbase + ((heap_low - wm_heapbase) & ~037);
	nh    = heap_low;
	mp    = marks + ((heap_low - wm_heapbase) >> 5);
	mstop = marks + ((heap_high - wm_heapbase) >> 5);

	for (; mp <= mstop; mp++, h += i)
	    for (i = 32, m = *mp; i && m; i--, h++, m >>= 1)
		if (m & 1) {						/* if low bit is set... */
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
				if (ISPOINTER(v) && h == TOPTR(v)) {
			    	val_at(nh) = ((long) nh) | (v & MTP_TAGMASK);
				}
				else {
			    	rev(h + 1, heap_high, h, (long)nh);
			    	val_at(nh) = val_at(h);
				}
		    }	/* if(ISFENCE...) - else */
		    nh++;
		}	/* if (m & 1) */

	/*------------------------------------------------------------------------------*
	 | Phase 6: Set H and HB.  Compaction is complete.
	 |
	 |    Note: On the 88k implementation, a choice point is created just prior
	 |          to calling gc so that H and HB should have the same value.
	 *------------------------------------------------------------------------------*/

	wm_H = nh - 1 + BIAS;
#ifdef ChptBeforeTrail
	wm_HB = chpt_HB(wm_B);
#else
	wm_HB = wm_H;
#endif

    }	/* End of block for phases 4, 5, and 6 */

    /*-----------------------------------------------------------------*
     | Unbias E and heapbase; note that H and HB were done above.
     *-----------------------------------------------------------------*/

    wm_E += BIAS;
    wm_heapbase_lvalue += BIAS;

#ifdef DEBUGSYS /*--------------------------------------------------*/
	if (debug_system[GCINFO]) {
    	finish_tick = clock();
    	printf("Compaction complete: nmarked=%ld time=%lu ticks\n", 
					nmarked, finish_tick - start_tick);
		pbi_cptx();
/*		pbi_walk_cps(); */
/*		pbi_swp_tr();  */
    	printf("new_gap=%d normal_safety = %ld\n",(wm_TR - wm_H),wm_normal);
    	fflush(stdout);
	}
#endif /* ---------------------------------------------- DEBUGSYS --*/

	if ((wm_TR - wm_H) < wm_normal)
	  {
	    printf("expanding heap from: %ld cells\nto: %ld cells\n",
		   current_engine.heap_size, current_engine.heap_size+0x40000);

	    if (!size_prolog_engine(&current_engine, current_engine.stack_size,
			       current_engine.heap_size+0x40000)) heap_overflow();
	  }

#ifdef undef 
		/**** WARNING!!!!!
		 The 1st 2 #ifdef cases below need work (declarations, etc):::: */

#if defined(HAVE_SIGACTION) && defined(SA_SIGINFO)
		stack_overflow(ALSSIG_HEAP_OVERFLOW, struct siginfo *siginf, struct ucontext *sigcon);
#elif defined(HAVE_SIGVEC) || defined(HAVE_SIGVECTOR)
		stack_overflow(ALSSIG_HEAP_OVERFLOW, int code, struct sigcontext *scp, caddr_t addr);
#elif defined(Portable)
		stack_overflow(ALSSIG_HEAP_OVERFLOW);
#else
#error
#endif   /* SIGACTION...*/
	else
    	return 1;

#endif /* #ifdef undef ----------------*/

    ASSERT(prolog_engine_invariant(&current_engine));

	return 1;

}	/* gc() */


	/*-----------------------------------------------------
	 | mark_args(e, ra)
	 *----------------------------------------------------*/

static long *
mark_args(long *e, Code *ra)
    /* ra: return address */
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
#if defined(MacOS) && defined(arch_m68k)
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

}	/* mark_args(e, ra) */


	/*-----------------------------------------------------
	 | mark_from(loc)
	 *----------------------------------------------------*/

static void
mark_from(long *loc)
{
    mark(val_at(loc));
    REV(loc);
}
	/*--------------------------*
	 | #define REV(loc) rev(heap_low,heap_high,(long *)(loc),(long)(loc))
	 *--------------------------*/

/*-------------------------------------------------------------------------------*
 | rev(lo, hi, loc, targ)
 | rev takes a parameter, loc, which is assumed to contain a pointer
 | to a marked heap location.  It "reverses" the pointer (provided it falls
 | in between lo and hi inclusive) by transferring the contents of
 | the heap location to loc and installing in the heap location
 | a pointer back to targ.  The tag bits are transferred to this new pointer
 | and the pointer is marked as reversed.
 *-------------------------------------------------------------------------------*/

static void
rev(long *lo, long *hi, long *loc, long targ)
{
    long *ptr;
    long  v;

    v = val_at(loc);
    if (ISPOINTER(v)
#ifndef MTP_CONST
				|| ISUIA(v)
#endif /* MTP_CONST */
							) {
		ptr = TOPTR(v);
		if (lo <= ptr && ptr <= hi) {
	    	val_at(loc) = val_at(ptr);
	    	val_at(ptr) = REVERSEIT(targ, v);
		}
    } /* if (ISPOINTER(v) ...) */
#ifdef MTP_CONST
    else if ((v & MTP_CONSTMASK) == MTP_UIA) {
		ptr = wm_heapbase + UIAVAL(v);
		if (lo <= ptr && ptr <= hi) {
	    	val_at(loc) = val_at(ptr);
	    	val_at(ptr) = MMK_UIA(((char *) targ) - ((char *) wm_gvbase));
		}
    }
#endif /* MTP_CONST */

} /* rev(lo, hi, loc, targ) */


/*------------------------------------------------------------------------------*
 | rev_update is called from the compaction phases of gc.  It is responsible
 | for updating a list of reversed pointers to the new location (targ).  loc
 | is the heap location that may (or may not) contain a list of reversed
 | pointers.
 *------------------------------------------------------------------------------*/

static void
rev_update(long *loc, long targ)
{
    long  v, tags, *ptr, temp;

    v = val_at(loc);					/* get initial value out of loc */
    for (ptr = loc;;) {
		tags = v & MTP_TAGMASK;			/* get the tags                 */
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
		  /* if (ISNORMAL(v)) */
		  if (ISNORMAL(ptr, v))
				break;					/* break if not reversed        */
	    	temp = UNREVERSEIT(v);		/* strip reverse bits and tags  */
	    	ptr = TOPTR(temp);			/* off of v and put in ptr      */
	    								/* (in 2 lines to avoid HighC bug)  */

	    	v = val_at(ptr);			/* put contents of "ptr" in v   */
	                        			/* for next pass around the loop */
	    	val_at(ptr) = targ | tags;	/* fix up contents of "ptr" to  */
	                                	/* the new location             */
		}	/* if(ISCONST) - else */ 
    } 		/* for (;;) */

    val_at(loc) = v;					/* put value at end of chain of */
    									/* reversed pointers back in loc */
}	/* rev_update(loc, targ) */


static void
init_marks(void)
{
    register unsigned long *m;

#if 0
    //marks = (unsigned long *) prs_area;
#endif

    marks = (unsigned long *) current_engine.mark_area;
    m = marks + ((wm_H - wm_heapbase) / 32) + 1;

    while (m > marks) {
	*--m = 0;
    }
}


	/*-----------------------------------------------------
	 | mark(val)
	 *----------------------------------------------------*/

static void
mark(register long val)
{
    register long *ptr, *bptr;
    register long tag, btag;
    int arity;

#ifdef FREEZE
    long xval;
#endif


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
	    	MARK(ptr);						/* mark front fence 			*/
	    	tag = FENCEVAL(val_at(ptr));	/* get distance between fences 	*/
	    	nmarked += tag - 1;				/* update nmarked for stuff between */
	    	ptr += tag;						/* move to back fence 			*/
	    	MARK(ptr);						/* mark back fence 				*/
		}	/* ISUIA(val) */
		goto mark_backup;
    }	/* -- end -- ISCONST(val) ----- */

    ptr = TOPTR(val);
    if (ptr < heap_low || ptr > heap_high)
		goto mark_backup;

    switch (tag) {

	case MTP_UNBOUND:


#ifdef FREEZE
				/* If the var is a delay var, mark the 
				   whole delay term containing it:
				 */
		if (CHK_DELAY((PWord *)val))
			{
#ifdef DEBUGSYS /*--------------------------------------------------*/
				if (debug_system[GCFREEZEINFO]) {
    				printf("\ngc: Delay-val=%x tag=%d..",(int)val,(int)tag);
    				fflush(stdout);
				}
#endif /* ---------------------------------------------- DEBUGSYS --*/
				xval = (long) MMK_STRUCTURE( ((PWord *)val)-1 );
				mark(xval);   
#ifdef DEBUGSYS /*--------------------------------------------------*/
				if (debug_system[GCFREEZEINFO]) {
    				printf("--xval=%x marked\n",(int)xval);
    				fflush(stdout);
				}
#endif /* ---------------------------------------------- DEBUGSYS --*/
			}
#endif /* FREEZE */


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

    }	/* switch */

    /* We should never fall through */

mark_backup:
    ptr = bptr;
    if (ptr == (long *) 0)
		return;					/* done marking */
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
}	/* mark(val) */



#ifdef INTCONSTR
int
gc(void)
{
    /*-----------------------------------------------------------------------*
     | Create the string of delay terms before running normal gc
     *-----------------------------------------------------------------------*/
#ifdef FREEZE
    long *this_dt;
    long *oldestcp;
    long *b;
    long *ap;
    long *tr;
				/* printf("Tr_b= %x  B= %x  TR= %x  H= %x  HB= %x  H_b= %x\n",
					(int)wm_trailbase,(int)wm_B,(int)wm_TR,
					(int)wm_H,(int)wm_HB,(int)wm_heapbase);  */
    b =  wm_B;
    tr = wm_TR;
    oldestcp = wm_B;

	/* Locate the oldest choicepoint */
    while (chpt_B(oldestcp) != (long *) 0) {
        oldestcp = chpt_B(oldestcp);
    }
				/* printf("oldestcp= %x\n", (int)oldestcp);  */
	/* walk the choicepoints */
    while (b <= oldestcp) {
				/* printf("walk b=%x oldestcp=%x\n", (int)b, (int)oldestcp);  */
#ifdef ChptAfterTrail
		tr =  b + chpt_SIZE;	/* get tr and b set up for next iteration */
		b =   chpt_B(b);
		if (b == (long *) 0)	/* exit early if necessary */
	    	break;
#endif /* ChptAfterTrail */

	/* walk trail entries between choicepoints */
#ifdef TRAILVALS
        for (; tr < b; tr++) {
				/* printf("    tr= %x  ", (int)tr); */
                    /* move up one element; tr is now an "ordinary" trail value: 
                       tr points at trail locations; so ap = *++tr is a heap variable */ 
            ap = (long *) *++tr;	/* tr is not biased */
				/* printf("    ap= %x\n", (int)ap); */

                    /* does the heap variable (location) ap contain a pointer (*ap) 
                       to a delay thing variable ? */
            if (M_ISVAR(*ap) && CHK_DELAY(*ap)) {   
                    /* this_dt is a C pointer to the TK_DELAY word of the delay thing structure */
                this_dt = (PWord *)((PWord *)(*ap) -1);
					printf(" - dt: this_dt=%x ap=%x *ap=%x\n", 
									(int)this_dt, (int)ap, (int)*ap ); 
                    /* convert the contents *ap of location ap to be a prolog struct pointer to this_dt */
                    /* w_install(addr, val, tag) PWord *addr; PWord val; int   tag;  */
                w_install(ap, (PWord)this_dt, WTP_STRUCTURE);
            }
        }

#else /* no-TRAILVALS */

        for (; tr < b; tr++) {
            ap = (long *) *tr;			/* tr is not biased */
/*            if (M_ISVAR(*ap) && CHK_DELAY(*ap)) {     */
            if (CHK_DELAY(*ap)) {
            this_dt = (PWord *)((PWord *)(*ap) -1);
            }
        }
#endif    /* TRAILVALS */


#ifdef ChptBeforeTrail
		tr = b + chpt_SIZE;	/* get tr and b set up for next iteration */
		b =  chpt_B(b);
		if (b == (long *) 0)	/* exit early if necessary */
	    	break;
#endif /* ChptBeforeTrail */
    }	/* while (b <= oldestcp) */

#endif   /* FREEZE */

						/* printf("CALL core_gc  "); */
core_gc();
						/* printf("EXIT core_gc\n"); */

#ifdef FREEZE
	/* walk the choicepoints, undoing the previous conversions;
           since the trail and CP's may have moved, we have to recacl
           from the beginning */
    b =  wm_B;
    tr = wm_TR;
    oldestcp = wm_B;
				/* printf("Tr_b= %x  B= %x  TR= %x  H= %x  HB= %x  H_b= %x\n",
					(int)wm_trailbase,(int)wm_B,(int)wm_TR,
					(int)wm_H,(int)wm_HB,(int)wm_heapbase);  */

	/* Locate the oldest choicepoint */
    while (chpt_B(oldestcp) != (long *) 0) {
        oldestcp = chpt_B(oldestcp);
    }
				/* printf("oldestcp= %x\n", (int)oldestcp);  */
	/* walk the choicepoints */
    while (b <= oldestcp) {
				/* printf("walk b=%x oldestcp=%x\n", (int)b, (int)oldestcp);  */
#ifdef ChptAfterTrail
		tr =  b + chpt_SIZE;	/* get tr and b set up for next iteration */
		b =   chpt_B(b);
		if (b == (long *) 0)	/* exit early if necessary */
	    	break;
#endif /* ChptAfterTrail */

	/* walk trail entries between choicepoints */
#ifdef TRAILVALS
        for (; tr < b; tr++) {
				/* printf("    tr= %x  ", (int)tr); */
                    /* move up one element; tr is now an "ordinary" trail value: 
                       tr points at trail locations; so ap = *++tr is a heap variable */ 
            ap = (long *) *++tr;	/* tr is not biased */
				/* printf("    ap= %x\n", (int)ap); */

                    /* does the heap variable (location) ap contain a prolog structure pointer (*ap) 
                       to the funct/arity word (**ap) of a delay thing? */
            if ( M_ISSTRUCT(*ap) ) {
                this_dt = MSTRUCTADDR(*ap);
                if ((MFUNCTOR_TOKID(*(PWord *)this_dt) == TK_DELAY) && (MFUNCTOR_ARITY(*(PWord *)this_dt) == 4)) 
                {
                        /* So one word up is the original var of the delay thing to which *ap should point */
                    this_dt = (PWord *)((PWord *)this_dt + 1);
		/*			printf(" # dt: this_dt=%x ap=%x *ap=%x\n", 
									(int)this_dt, (int)ap, (int)*ap );  */
                        /* convert the contents *ap of location ap to be a prolog var-var pointer to this_dt */
                        /* w_install(addr, val, tag) PWord *addr; PWord val; int   tag;  */

                    w_install(ap, (PWord)this_dt, WTP_REF);
                }
            }
        }

#else /* no-TRAILVALS */

        for (; tr < b; tr++) {
            ap = (long *) *tr;			/* tr is not biased */
            if ( M_ISSTRUCT(*ap) ) {
                this_dt = MSTRUCTADDR(*ap);
                if ((MFUNCTOR_TOKID(*(PWord *)this_dt) == TK_DELAY) && (MFUNCTOR_ARITY(*(PWord *)this_dt) == 4)) 
                {
                    this_dt = (PWord *)((PWord *)this_dt + 1);
                    w_install(ap, (PWord)this_dt, WTP_REF);
                }

            }
        }

#endif    /* TRAILVALS */

#ifdef ChptBeforeTrail
		tr = b + chpt_SIZE;	/* get tr and b set up for next iteration */
		b =  chpt_B(b);
		if (b == (long *) 0)	/* exit early if necessary */
	    	break;
#endif /* ChptBeforeTrail */

    }	/* while (b <= oldestcp) */

	return 1;
}
#endif    /* FREEZE */

#endif 		/* INTCONSTR  */

