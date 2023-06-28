/*=======================================================================*
 |			bsystem.c
 |		Copyright (c) 1986-95 Applied Logic Systems, Inc.
 |
 |			Prolog internal system builtins defined in C.
 |
 | Program Author:  Kevin A. Buettner
 | Creation:  11/14/84
 | Revision History: (fixes, not addition of new builtins)
 | 06/28/85 - K. Buettner -- Conversion to wam and compiled prolog
 | 09/12/85 - K. Buettner -- arithmetic predicates moved to separate file.
 | 01/28/86 - K. Buettner -- IBM PC conversion
 | 10/26/94 - C. Houpt -- Modernized Mac pbi_debugger call 
 |						  and various UCHAR* casts.
 *=======================================================================*/
#include "defs.h"
#include "module.h"
#include "limits.h"

#ifdef MacOS
/* For the Debugger trap. */
#include <Types.h>
#endif

static	void	abortmessage	( void );

int
pbi_ouch()
{
    wm_safety = wm_trigger;
    SUCCEED;
}

int
pbi_forceCtlC()
{
    wm_interrupt_caught = SIGINT;
    wm_safety = wm_trigger;
    SUCCEED;
}

int
pbi_forcePrologError()
{
    wm_interrupt_caught = ALSSIG_ERROR;
    wm_safety = wm_trigger;
    FAIL;
}

/*
 * pbi_reset_wm_normal should be called after a heap overflow has been
 * caught.  When a heap overflow occurs, we temporarily halve the value
 * of wm_normal so that execution may proceed.  We expect that the
 * execution which proceeds will throw back to a catch to handle the
 * heap overflow.
 */

int
pbi_reset_wm_normal()
{
    wm_normal = DEFAULT_SAFETY;
    /*
     * We should put in some code to cause delivery of signals to be held
     * up around the following if statement to ensure atomicity of the
     * following test and assignment.
     */

    /* block signals (need to add here) */
    if (wm_safety > 0)
	wm_safety = wm_normal;
    /* reenable signals (need to add here) */

    SUCCEED;
}

#ifdef OLDSHELL

int
pbi_showanswers()
{
    int   ch;
    PWord nv, av, ev;
    int   nt, at, et;
    int   nprinted = 0;

    w_get_An(&nv, &nt, 1);
    w_get_An(&av, &at, 2);

    while (nt == WTP_LIST) {
	if (at != WTP_LIST) {
	    PI_oprintf("Error in showanswers, at=%d.\n", at);
	    als_exit(1);
	}

	w_get_car(&ev, &et, nv);

	if (!xform_uia(&ev, &et) && et != WTP_UIA) {
	    PI_oprintf("Error in showanswers, et=%d\n", et);
	    als_exit(1);
	}

	if (et == WTP_SYMBOL && ev == TK_UNDERSCORE) {
	    w_get_cdr(&nv, &nt, nv);
	    w_get_cdr(&av, &at, av);
	}
	else {
	    PI_oprintf("\n");
	    prolog_write(ev, et);
	    PI_oprintf(" = ");
	    w_get_car(&ev, &et, av);
	    prolog_writeq(ev, et);
	    w_get_cdr(&nv, &nt, nv);
	    w_get_cdr(&av, &at, av);
	    nprinted++;
	}
    }

    if (nprinted) {
	ch = getchar();
	if (ch == EOF)
	    als_exit(0);	/* exit on eof */

	/* Keep going until get \n */
	if (ch != '\n') {
	    int   nch;

	    while ((nch = getchar()) != '\n')
		if (nch == EOF)
		    als_exit(0);
	}
	if (ch == ';')
	    FAIL;

    }

    PI_oprintf("\nyes.\n");
    SUCCEED;
}

#endif /* OLDSHELL */

static void
abortmessage()
{
    switch (wm_aborted) {
	case 0:		/* do nothing ... no abort */
	    break;

	case 2:
	    PI_oprintf("\nHeap or trail/choice point stack overflow.  Execution aborted.\n");
	    break;
	case 3:
	    PI_oprintf("\nStack overflow. Execution aborted.\n");
	    break;
	default:		/* normal abort (usually 1) */
	    PI_oprintf("\nExecution aborted.\n");
	    break;

    }
}

int
pbi_halt()
{
    if (wm_aborted)
	abortmessage();
    als_exit(0);

    /* Never returns */
    SUCCEED;			/* but we put return in to appease -Wall */
}



#ifdef OLDSHELL
int
pbi_printno()
{
    if (wm_aborted) {
	/* If we are at the top level, print message;
	 * otherwise be silent.
	 */
	if (wm_regidx == 1)
	    abortmessage();
    }
    else
	PI_oprintf("\nno.\n");
    SUCCEED;
}
#endif /* OLDSHELL */

int
pbi_printwarning()
{
    if (wm_aborted) {
	/* If we are at the top level, print message;
	 * otherwise be silent.
	 */
#if 0
	//if (wm_regidx == 1)
#endif
	if (current_engine.reg_stack_top == current_engine.reg_stack_base+1)
	    abortmessage();
    }
    else if (curfd == stdin)
	PI_oprintf("\nWarning: Command failed.\n");
    else
	PI_oprintf("\nWarning: Command in file `%s' near line %d has failed.\n",
		   TOKNAME(seetbl[cur_si].tkidx), linenum);
    SUCCEED;
}


int
pbi_stack_overflow()
{
    PWord v;
    int   t;

    w_get_An(&v, &t, 1);

    if (t == WTP_INTEGER && v > 0 &&
	((long) ((long) wm_SP - v) < (long) wm_stackbot))
	SUCCEED;
    else
	FAIL;
}

int
pbi_stack_info()
{
    PWord v1;
    int   t1;

    w_get_An(&v1, &t1, 1);

    if (PI_unify(v1, t1, (PWord) ((long) wm_SP - (long) wm_stackbot), WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}



/*
int pbi_limits_info()
{
    PWord v1,v2,v3;
    int   t1,t2,t3;
	int MaxArity, MinInt,MaxInt=1, i,p=1;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);
    w_get_An(&v3, &t3, 3);

	for (i=1; i<= 28; ++i)
		 p = p*2;

	MaxInt = p-1;
	MinInt = - MaxInt;

printf("Max=%d Min=%d MA=%d\n",MaxInt,-MaxInt,MaxArity);

	if( PI_unify(v1,t1,(PWord)(-MaxInt), WTP_INTEGER) &&
		PI_unify(v2,t2,(PWord)MaxInt, WTP_INTEGER) &&
		PI_unify(v3,t3,(PWord)ULONG_MAX, WTP_INTEGER) )
		SUCCEED;
	else
		FAIL;
}
*/


#ifdef MacOS

int
pbi_debugger(void)
{
    Debugger();

    SUCCEED;
}

#endif /* MacOS */

/* #ifdef OLDSHELL */
int
pbi_statistics()
{
    PWord v;
    int   t;
    PWord item, lv, tv, numv;
    int   itemtype, lt, tt, numt;
    long  used, total;

    w_get_An(&v, &t, 1);

    tv = TK_NIL;
    tt = WTP_SYMBOL;

    /*
     * code_area(used,total)
     */
    w_freecount(&total, &used);
    w_mk_term(&item, &itemtype, (PWord) find_token((UCHAR *)"code_area"), 2);
    w_install_argn(item, 1, (PWord) (used), WTP_INTEGER);
    w_install_argn(item, 2, (PWord) (total), WTP_INTEGER);

    w_mk_list(&lv, &lt);
    w_install_car(lv, item, itemtype);
    w_install_cdr(lv, tv, tt);
    tv = lv;
    tt = lt;

    /*
     * heap(heap_left,heap_used,trail_used,gv_allocated,heap_size)
     */
    w_mk_term(&item, &itemtype, (PWord) find_token((UCHAR *)"heap"), 5);
    w_install_argn(item, 1,
       (PWord) ((unsigned long) wm_TR - (unsigned long) wm_H), WTP_INTEGER);
    w_install_argn(item, 2,
		   (PWord) ((unsigned long) wm_H - (unsigned long) wm_heapbase), WTP_INTEGER);
    w_install_argn(item, 3,
		   (PWord) ((unsigned long) wm_trailbase - (unsigned long) wm_TR), WTP_INTEGER);
    w_install_argn(item, 4,
	 (PWord) ((unsigned long) wm_gvbase - (unsigned long) wm_trailbase),
		   WTP_INTEGER);
    w_install_argn(item, 5,
	  (PWord) ((unsigned long) wm_gvbase - (unsigned long) wm_heapbase + 4),
		   WTP_INTEGER);

    w_mk_list(&lv, &lt);
    w_install_car(lv, item, itemtype);
    w_install_cdr(lv, tv, tt);
    tv = lv;
    tt = lt;

    /*
     * stack(stack_left,stack_used,stack_size)
     */
    w_mk_term(&item, &itemtype, (PWord) find_token((UCHAR *)"stack"), 3);
    w_install_argn(item, 1,
		   (PWord) ((unsigned long) wm_SP - (unsigned long) wm_stackbot), WTP_INTEGER);
    w_install_argn(item, 2,
		   (PWord) ((unsigned long) wm_heapbase - (unsigned long) wm_SP), WTP_INTEGER);
    w_install_argn(item, 3,
	(PWord) ((unsigned long) wm_heapbase - (unsigned long) wm_stackbot),
		   WTP_INTEGER);

    w_mk_list(&lv, &lt);
    w_install_car(lv, item, itemtype);
    w_install_cdr(lv, tv, tt);

    tv = lv;
    tt = lt;

    /*
     * wm_regs(SP,E,SPB,HB,H,TR,B)
     */

    w_mk_term(&item, &itemtype, (PWord) find_token((UCHAR *)"wm_regs"), 7);

    make_number(&numv, &numt, (double) (unsigned long) wm_SP);
    w_install_argn(item, 1, numv, numt);
    make_number(&numv, &numt, (double) (unsigned long) wm_E);
    w_install_argn(item, 2, numv, numt);
    make_number(&numv, &numt, (double) (unsigned long) wm_SPB);
    w_install_argn(item, 3, numv, numt);
    make_number(&numv, &numt, (double) (unsigned long) wm_HB);
    w_install_argn(item, 4, numv, numt);
    make_number(&numv, &numt, (double) (unsigned long) wm_H);
    w_install_argn(item, 5, numv, numt);
    make_number(&numv, &numt, (double) (unsigned long) wm_TR);
    w_install_argn(item, 6, numv, numt);
    make_number(&numv, &numt, (double) (unsigned long) wm_B);
    w_install_argn(item, 7, numv, numt);

    w_mk_list(&lv, &lt);
    w_install_car(lv, item, itemtype);
    w_install_cdr(lv, tv, tt);

    if (w_unify(v, t, lv, lt))
	SUCCEED;
    else
	FAIL;
}
/* #endif  OLDSHELL */

#ifdef	IProfile
int
pbi_init_iprofile()
{
    init_iprofile();
    SUCCEED;
}

int
pbi_dump_iprofile()
{
    dump_iprofile();
    SUCCEED;
}
#endif	/* IProfile */
