/*=================================================================*
 |			wdisp.c
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-1995 by Applied Logic Systems, Inc.
 |
 |			-- dispatch table & print routines for prolog data types
 |
 | Author: Kevin A. Buettner
 | Creation: 6/15/85
 | Revision History:
 | 02/??/86 - kev -- alspro port
 | 05/14/86 - kev -- parenthesis problem on prefix ops
 | 10/26/94 - C. Houpt -- char* cast in wr_sym.
 *=================================================================*/
#include "defs.h"

#ifndef KERNAL
/*
 * write and company
 *
 *    We need to concern ourselves with the following issues:
 *      1.  Getting write to work as people expect it to.
 *      2.  Getting a quoted write to work properly, i.e, the parser
 *          should be able to read back everything that writeq spits out
 *      3.  A facility is needed to write (and read) to (from) an arbitrarily
 *          sized buffer, possibly connected to the screen.
 *      4.  Files should also be provided for.
 *      5.  Finally, in order to do any sort of editing properly, we
 *          need to have another version of a quoted write, namely one
 *          which takes a list of variables and the term to be written
 *          and puts the variables in UNQUOTED.
 *
 *      To this end, we either need separate procedures or zillions of
 *      mode bits.  Neither option is attractive, but for the time being
 *      I am opting for the mode bits.
 *
 *
 */

static	void	wr_trm		( PWord, int, int );
static	void	wr_int		( PWord, int, int );
static	void	wr_lst		( PWord, int, int );
static	void	wr_sym		( PWord, int, int );
static	void	wr_var		( PWord, int, int );
static	void	wr_uia		( PWord, int, int );

static	int	wr_op2		( PWord, int, long, int, int );
static	int	wr_op		( PWord, long, int, int );
static	void	write_quoted_symbol ( char * );

#ifdef DoubleType
static	void	wr_dbl		( PWord, int, int );
static	int	check_vector	( long *, long * );
static	void	print_vector	( long *, long );
static	void	craig_get_double ( double *, long * );
#endif /* DoubleType */

/* This order has to match up with the WTP_type constants in winter.h */

static void (*wr_tbl[]) ( PWord, int, int ) = {
	wr_var,
	wr_lst,
	wr_trm,
	wr_sym,
	wr_int,
	wr_uia
#ifdef DoubleType
	,wr_dbl
#endif /* DoubleType */
};

#define WQUOTE 0x01		/* for use with writeq */
#define WDISP  0x02

#define WRITE(v,t,flags,depth) ((*wr_tbl[(t)])(v,flags,depth))

static int spaceneeded;		/* indicates that a space is needed when
				 * writing a unary postfix or prefix op
				 */

void
prolog_write(PWord v, int t)
{
    spaceneeded = 0;
    WRITE(v, t, 0, 0);
}

void
prolog_writeq(PWord v, int t)
{
    spaceneeded = 0;
    WRITE(v, t, WQUOTE, 0);
}

void
prolog_display(PWord v, int t)
{ 
    spaceneeded = 0;
    WRITE(v, t, WDISP | WQUOTE, 0);
}

#define MK_OP(prec) ((long) OP_XFX(prec))
#define WRITEDEPTH 5000


static int
wr_op2(PWord p, int t, long inprec, int flags, int depth)
{
    if (t != WTP_STRUCTURE || depth > WRITEDEPTH)
	return (0);
    else
	return (wr_op(p, inprec, flags, depth));
}

static int
wr_op(PWord w, long inprec, int flags, int depth)
{
    int   parenthesized;	/* indicates if expression is parenthesized */
    int   argn;			/* argument counter */
    int   tp;			/* type */
    PWord vl;			/* Value */
    PWord ft;			/* functor */
    int   ar;			/* arity */
    unsigned short opprec;	/* operator precedence/associativity info */


    w_get_functor(&ft, w);	/* get token id of functor */
    w_get_arity(&ar, w);	/* get arity of structured term */


    /*
     * Get operator precedence if possible.  If not return failure.
     */

    if (TOKUNOP(ft) && ar == 1)
	opprec = TOKUNOP(ft);
    else if (TOKBINOP(ft) && ar == 2)
	opprec = TOKBINOP(ft);
    else
	return (0);		/* can not write out as an operator */

    /* decide if ()'s are needed */
    parenthesized = (PREC_ONLY(opprec) >= inprec);

    if (parenthesized) {
	PI_oprintf("(");	/* write out ( if needed */
	spaceneeded = 0;
    }
    argn = 1;			/* set the argument counter */

    /* If we have a postfix or infix operator, then write out expression
     * with operators in it.  If that fails, the write out the expression
     * normally with write_val.
     */
    if (!(opprec & ASSC_PREFIX)) {
	w_get_argn(&vl, &tp, w, argn);
	if (!wr_op2(vl, tp, (long) PREC_LEFT(opprec), flags, depth + 1)) {
	    WRITE(vl, tp, flags, depth + 1);
	    spaceneeded = 0;
	}

	argn++;

	if (!qtok((int) ft))
	    spaceneeded = 1;

    }

    if (spaceneeded)
	PI_oprintf(" ");

    WRITE(ft, WTP_SYMBOL, flags & ~WQUOTE, depth);

    /* write out the operator */

    spaceneeded = 1;

    /*
     * Write out right side of expression for infix and prefix operators
     */

    if (argn == ar) {
	if (!qtok((int) ft)) {
	    PI_oprintf(" ");
	    spaceneeded = 0;
	}

	w_get_argn(&vl, &tp, w, argn);

	if (!wr_op2(vl, tp, PREC_RIGHT(opprec), flags, depth + 1)) {
	    WRITE(vl, tp, flags, depth + 1);
	    spaceneeded = 0;
	}
    }
    /* Put the final paren if needed. */
    if (parenthesized) {
	PI_oprintf(")");
	spaceneeded = 0;	/* won't need space after a ) */
    }
    return (1);			/* successful completion */
}

static void
wr_trm(PWord w, int flags, int depth)
{
    int   n;
    PWord vl;
    int   tp;

    if (depth > WRITEDEPTH)
	PI_oprintf(" ... ");
    else if ((flags & WDISP) || !wr_op(w, MK_OP(1000), flags, depth)) {
	w_get_functor(&vl, w);
	w_get_arity(&n, w);

	if (vl == TK_DDOUBLE && n == 4) {
	    double d;
	    int   i;

	    for (i = 0; i < 4; i++) {
		w_get_argn(&vl, &tp, w, i + 1);
		((short *) &d)[i] = vl;
	    }

	    if (spaceneeded && d < 0)
		PI_oputchar(' ');
	    PI_oprintf("%1.10g", d);
	}
	else {
	    WRITE(vl, WTP_SYMBOL, flags, depth);

	    if (n > 0) {
		int i;
		PI_oputchar('(');
		for (i = 1; i <= n; i++) {
		    w_get_argn(&vl, &tp, w, i);
		    if ((flags & WDISP) ||
			!wr_op2(vl, tp, MK_OP(1000), flags, depth + 1))
			WRITE(vl, tp, flags, depth + 1);
		    if (i != n)
			PI_oprintf(",");
		}
		PI_oputchar(')');
	    }
	}
    }
}

static void
wr_int(PWord w, int flags, int depth)
{
    if (spaceneeded && w < 0)
	PI_oprintf(" %ld", w);
    else
	PI_oprintf("%ld", w);
}

static void
wr_lst(PWord w, int flags, int depth)
{
    PWord vl;
    int   tp;
    PWord ww;

    PI_oputchar('[');
    tp = WTP_LIST;
    ww = w;
    while (tp == WTP_LIST) {
	if (depth++ > WRITEDEPTH) {
	    PI_oprintf(" ... ");
	    tp = WTP_SYMBOL;
	    ww = TK_NIL;
	    break;
	}

	w_get_car(&vl, &tp, ww);
	if ((flags & WDISP) || !wr_op2(vl, tp, MK_OP(1000), flags, depth))
	    WRITE(vl, tp, flags, depth);

	w_get_cdr(&ww, &tp, ww);
	if (tp == WTP_LIST)
	    PI_oputchar(',');
    }

    if (tp != WTP_SYMBOL || ww != TK_NIL) {
	PI_oprintf(" | ");
	if ((flags & WDISP) || !wr_op2(ww, tp, MK_OP(1000), flags, depth))
	    WRITE(ww, tp, flags, depth);
    }
    PI_oputchar(']');
}

static void
wr_sym(PWord w, int flags, int depth)
{
    if ((flags & WQUOTE) && qtok((int) w))
	write_quoted_symbol((char *)TOKNAME(w));
    else
	PI_oprintf("%s", TOKNAME(w));
}

void
write_quoted_symbol(char *s)
{
    PI_oputchar('\'');
    while (*s) {
	if (*s == '\'') {
	    PI_oputchar(*s);
	    PI_oputchar(*s);
	}
	else
	    PI_oputchar(*s);
	s++;
    }
    PI_oputchar('\'');
}

static void
wr_var(PWord w, int flags, int depth)
{
    PI_oprintf("_%lu", (long) ((PWord *)w - wm_heapbase));
}

static void
wr_uia(PWord w, int flags, int depth)
{
    if (flags & WQUOTE)
	write_quoted_symbol((char *)M_FIRSTUIAWORD(w));
    else
	PI_oprintf("%s", M_FIRSTUIAWORD(w));
}

#ifdef DoubleType
/*
 * Stuff to print out vectors added by
 * Craig Thornley       April 11, 1989
 * Vectors are simply floats with fences greater than 3
 */

static void
wr_dbl(PWord w, int flags, int depth)
{
    double d;
    long  count;

    if (check_vector((long *)w, &count))
	print_vector((long *)w, count);
    else {
	w_get_double(&d, w);
	if (spaceneeded && d < 0)
	    PI_oprintf(" ");
	PI_oprintf("%1.10g", d);
    }
}

static int
check_vector(long *ptr, long *count)
{
    *count = (MFENCE_VAL(*ptr) - 1) / 2;
    return (*count > 1);
}

static void
print_vector(long *w, long count)
{
    int   i;
    double d;

    w += 1;
    for (i = 0; i < count; i++) {
	craig_get_double(&d, w);
	PI_oprintf("%1.10g", d);
	w += 2;
    }
}


static void
craig_get_double(double *dbl, long *ptr)
{
    *(long *) dbl = *ptr;
    *(((long *) dbl) + 1) = *(ptr + 1);
}

#endif /* DoubleType */
#endif
