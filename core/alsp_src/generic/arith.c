/*===================================================================*
 |			arith.c   
 |		Copyright (c) 1985 by Kevin A. Buettner
 |		Copyright (c) 1986-95 Applied Logic Systems, Inc.
 |
 | 			-- arithmetic builtins defined in C.
 |
 | Program Author:  Kevin A. Buettner
 | Creation:  11/14/84
 | 09/12/85 - K. Buettner -- arithmetic predicates moved from builtins.c
 | 02/28/86 - K. Buettner -- PC port (for integer stuff only)
 | 11/08/90 - K. Buettner -- Put longjmp in for error handling
 | 06/03/92 - R. DiNapoli -- Added #ifdefs for Mac include files
 | 10/26/94 - C. Houpt -- Added clock_ticks_per_second for Macintosh
 |						and removed unneeded Mac header.
 *===================================================================*/
#include "defs.h"
#include <math.h>
#include <errno.h>

#if defined(DOS)
#include <stdef.h>

#elif defined(VMS)  /* DOS */
#include <types.h>

#elif defined(MacOS)
#ifdef HAVE_GUSI
#include <GUSI.h>
#else
#include <Events.h>
#ifndef MPW_TOOL
#include <unix.h>
#endif
#endif

#elif defined(UNIX)
#include <sys/types.h>
#include <sys/param.h>

#endif

#include <time.h>
#include <setjmp.h>

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

#ifdef HAVE_MACHINE_PARAM_H
#include <machine/param.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

extern	double	als_cputime	PARAMS(( void ));
extern	double	als_realtime	PARAMS(( void ));
extern	double	als_random	PARAMS(( void ));
#ifndef HAVE_AINT
extern	double	aint		PARAMS(( double ));
#endif
#ifndef HAVE_RINT
extern	double	rint		PARAMS(( double ));
#endif
#ifndef HAVE_EXP10
extern	double	exp10		PARAMS(( double ));
#endif
static	double	do_is		PARAMS(( PWord, int, int * ));

long  gensym_start_time;	/* starting time for gensym facility */
static double start_time;
static int clock_ticks_per_second;	/* for times() */
static jmp_buf is_error;

/* Define how to handle random numbers */

#ifdef HAVE_SRAND48
#define drandom drand48
#define srandom srand48
extern	void	srand48		PARAMS(( long ));
extern	double	drand48		PARAMS(( void ));

#elif defined(HAVE_SRANDOM)
#define RANDRANGE 0x7fffffff
#ifndef __DJGPP__
extern	long	random		PARAMS(( void ));
extern	void	srandom		PARAMS(( int ));
#endif

#elif defined(HAVE_SRAND)
#define random rand
#define srandom srand
#define RANDRANGE 0x7fffffff
#endif


#ifdef DOS
#define currentTime (double)((double)clock() / CLOCKS_PER_SEC)
#else
#ifdef MacOS
/*
 * The MPW C 3.1 compiler has some problems emitting proper code for
 * certain operations involving floating point arithmetic.  The Mac version
 * should be very similar to the DOS version in that the "clock" function
 * (TickCount) returns the number of machine cycles ("ticks") since the
 * system was last booted.  A tick is approximately 1/60 second.  However,
 * defining currentTime in a similar way to the DOS version caused the
 * C-compiler to generate faulty code.  To get around this, I had to define
 * currentTime to be a function , defined below:
 */
#define currentTime (double)(((double)TickCount()) / ((double)CLOCKS_PER_SEC))
#if 0
#define currentTime current_time()
static double
current_time(void)
{
    double double_ticks;
    long  long_ticks;

    long_ticks = TickCount();
    double_ticks = (double) long_ticks;
    double_ticks = double_ticks / 60.0;
    return (double) (double_ticks - start_time);
}
#endif
#else
#define currentTime time(0L)
#endif /* MacOS */
#endif /* DOS */

void
init_time()
{
    start_time = currentTime;
    srandom((long) start_time);
    gensym_start_time = (long) time(0L);
#ifdef	_SC_CLK_TCK
    clock_ticks_per_second = sysconf(_SC_CLK_TCK);
#else	/* HAVE_UNISTD_H */
#if defined(MacOS) || defined(MSWin32)
    clock_ticks_per_second = CLOCKS_PER_SEC;
#else	/* MacOS */
    clock_ticks_per_second = HZ;
#endif	/* MacOS */
#endif	/* HAVE_UNISTD_H */
}



#ifdef HAVE_TIME
int
pbi_time()
{				/* real system time */
    PWord vsec, vmin, vhour, vmday, vmon, vyear, vwday, vyday, visdst;
    int   tsec, tmin, thour, tmday, tmon, tyear, twday, tyday, tisdst;
    time_t tv;
    struct tm *tp;

    w_get_An(&vsec, &tsec, 1);
    w_get_An(&vmin, &tmin, 2);
    w_get_An(&vhour, &thour, 3);
    w_get_An(&vmday, &tmday, 4);
    w_get_An(&vmon, &tmon, 5);
    w_get_An(&vyear, &tyear, 6);
    w_get_An(&vwday, &twday, 7);
    w_get_An(&vyday, &tyday, 8);
    w_get_An(&visdst, &tisdst, 9);

    tv = time(0L);
    tp = localtime(&tv);

    if (w_unify(vsec, tsec, (PWord) tp->tm_sec, WTP_INTEGER) &&
	w_unify(vmin, tmin, (PWord) tp->tm_min, WTP_INTEGER) &&
	w_unify(vhour, thour, (PWord) tp->tm_hour, WTP_INTEGER) &&
	w_unify(vmday, tmday, (PWord) tp->tm_mday, WTP_INTEGER) &&
	w_unify(vmon, tmon, (PWord) tp->tm_mon, WTP_INTEGER) &&
	w_unify(vyear, tyear, (PWord) tp->tm_year, WTP_INTEGER) &&
	w_unify(vwday, twday, (PWord) tp->tm_wday, WTP_INTEGER) &&
	w_unify(vyday, tyday, (PWord) tp->tm_yday, WTP_INTEGER) &&
	w_unify(visdst, tisdst, (PWord) tp->tm_isdst, WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}
#endif /* HAVE_TIME */


#ifdef HAVE_TIMES
double
als_cputime()
{
#ifdef VMS
    tbuffer_t t;

    times(&t);
    return (double) (t.proc_user_time) / 100;
#else  /* VMS */
    struct tms t;

    times(&t);
    return (double) (t.tms_utime + t.tms_stime) 
			/ (double) clock_ticks_per_second;
#endif /* VMS */
}
#else /* HAVE_TIMES */
double
als_cputime()
{
    return (double) (currentTime - start_time);
}
#endif /* HAVE_TIMES */


double
als_realtime()
{
    return (double) (currentTime - start_time);
}

double
als_random()
{
#if MacOS
    return ((double) random()) / ((double) RAND_MAX);
#else
#ifdef	RANDRANGE
    return ((double) random()) / ((double) RANDRANGE);
#else	/* RANDRANGE */
    return drandom();
#endif	/* RANDRANGE */
#endif
}

int
pbi_srandom()
{
    PWord v1;
    int t1;
    double seed;

    w_get_An(&v1,&t1,1);

    if (get_number(v1,t1,&seed)) {
	srandom((long) seed);
	SUCCEED;
    }
    else
	FAIL;
}


#ifndef HAVE_AINT
/* aint rounds towards zero */
double
aint(d)
    double d;
{
    if (d < 0)
	return (ceil(d));
    else
	return (floor(d));
}
#endif /* HAVE_AINT */

#ifndef HAVE_RINT
double
rint(d)
    double d;
{
    return (floor(d + 0.5));
}
#endif

#ifndef HAVE_EXP10
double exp10(d)
    double d;
{
    return pow((double) 10, d);
}

#endif

#if defined(HAVE_LGAMMA) && !defined(HAVE_GAMMA)
/*
 * NeXT manual page documentation suggests the following to get the gamma
 * function.
 */

double
gamma(x)
    double x;
{
    return lgamma(x);
}

#endif

/*--------------------------------------------------------------------------------*
 | do_is recursively descends the expression to evaluate and returns
 | the value of the tree at each stage.   If an error is encountered, a long
 | jump is performed back to the original caller. 
 | In the implementations with no coercion (#ifndef COERCE2INT), the third argument 
 | is a pointer to an int variable which is used to return the computed
 | type of the expression, primarily either WTP_INT or WTP_STRUCTURE 
 | (= representation of doubles in most of the implementations; WTP_DOUBLE
 | in the implementations which support it: #ifdef DoubleType).  The
 | int type and float type are kept separate, but the "contaigon" model of
 | arithmetic is adoped: If any component of an arithmetic expression is a
 | float, then the entire result is a float; the result is an int (for normal
 | arithmetic expressions) only if every component is an int.  Consequently,
 | a float component recursively deep in the expression being evaluated could
 | force the entire expression to become a float.  But since do_is only returns
 | C data types, it uses a double for the return, and so we cannot distinguish
 | between a "true" double being returned and an "int as double" being returned.
 | [Under coercion, it didn't matter.]  
 |     The model is that the type of the returned value is assumed to be
 | int unless it is contaminted at some point by a float.  Thus, it is the
 | responsibility of the routines calling do_is/3 to allocate the variable ty,
 | and to initialize it to WTP_INTEGER. At this time (5/28/95), the only calls
 | to do_is/3 are:
 | 		pbi_is
 |		name
 |		recursive calls on itself by do_is;
 | These are all contained in this file; since they are so few, we leave the
 | responsibility for allocation and initialization of ty to them; should this
 | situation change, a wrapper function should be used.
 *--------------------------------------------------------------------------------*/

static double
do_is(v, t, ty)
    PWord v;
    int   t;
    int   *ty;
{
    int   arity;
    PWord functor;
    PWord v1, v2;
    int   t1, t2;
    double rv;

    errno = 0;

    switch (t) {
	case WTP_UNBOUND:
	    longjmp(is_error, 1);

	case WTP_SYMBOL:
	    switch (v) {
		case TK_HEAPUSED:
		    return (double) (((long) wm_H) - ((long) wm_heapbase));
		case TK_CPUTIME:
#ifdef HAVE_TIMES
		    return als_cputime();
#endif /* HAVE_TIMES */
		case TK_REALTIME:
		    return (double) (currentTime - start_time);
		case TK_RANDOM:
		    return als_random();
		default:
		    longjmp(is_error, 1);
	    }

	case WTP_LIST:
	    w_get_car(&v1, &t1, v);
	    w_get_cdr(&v2, &t2, v);
	    if (t2 != WTP_SYMBOL && v2 != TK_NIL)
		longjmp(is_error, 1);
	    else
		return do_is(v1, t1, ty);

	case WTP_INTEGER:
	    return (double) (v);

	case WTP_STRUCTURE:
	    w_get_arity(&arity, v);
	    w_get_functor(&functor, v);
	    if (arity == 1) 
		{
		int ty1;

		w_get_argn(&v1, &t1, v, 1);
		ty1 = WTP_INTEGER;
		rv = do_is(v1, t1, &ty1);
		errno = 0;
		*ty = WTP_DOUBLE;
		switch ((int) functor) {
		    case TK_PLUS: 
			*ty = ty1;
			return (rv); 
		    case TK_MINUS: 
			*ty = ty1; 
			rv = -rv; 
			break; 
		    case TK_BACKSLASH:
		    case TK_NOT:
			*ty = ty1;
			rv = (double) ~(long) rv;
			break;
		    case TK_EXP:
			rv = exp(rv);
			break;
		    case TK_EXP10:
			rv = exp10(rv);
			break;
		    case TK_LOG:
			rv = log(rv);
			break;
		    case TK_LOG10:
			rv = log10(rv);
			break;
		    case TK_SIN:
			rv = sin(rv);
			break;
		    case TK_COS:
			rv = cos(rv);
			break;
		    case TK_TAN:
			rv = tan(rv);
			break;
		    case TK_ASIN:
			rv = asin(rv);
			break;
		    case TK_ACOS:
			rv = acos(rv);
			break;
		    case TK_ATAN:
			rv = atan(rv);
			break;
		    case TK_ROUND: 
			*ty = WTP_INTEGER; 
			rv = rint(rv);
			break;
		    case TK_TRUNC: 
			*ty = WTP_INTEGER; 
			rv = aint(rv);
			break;
		    case TK_FLOOR: 
			*ty = WTP_INTEGER; 
			rv = floor(rv);
			break;
		    case TK_FLOAT: 
			*ty = WTP_DOUBLE; 
			break;
		    case TK_SQRT:
			rv = sqrt(rv);
			break;
		    case TK_ABS:
			if (rv < 0)
			    rv = -rv;
			break;
		    default:
			longjmp(is_error, 1);
		}

		if (errno != EDOM && errno != ERANGE)
		    return (rv);
		else
		    longjmp(is_error, 1);
	    }	/**** Arith 1 *****/
	    else if (arity == 2) 
		{
		double rv1, rv2;
		int ty1, ty2;

		w_get_argn(&v1, &t1, v, 1);
		ty1 = WTP_INTEGER;
		rv1 = do_is(v1, t1, &ty1);
		w_get_argn(&v2, &t2, v, 2);
		ty2 = WTP_INTEGER;
		rv2 = do_is(v2, t2, &ty2);

		switch ((int) functor) {
		    case TK_PLUS: 
			*ty = max(ty1, ty2); 
			return rv1 + rv2;
		    case TK_MINUS: 
			*ty = max(ty1, ty2); 
			return rv1 - rv2;
		    case TK_STAR:
			*ty = max(ty1, ty2); 
			return rv1 * rv2;
		    case TK_BAND:
			*ty = max(ty1, ty2); 
			return (double) ((long) rv1 & (long) rv2);
		    case TK_BOR:
			*ty = max(ty1, ty2); 
			return (double) ((long) rv1 | (long) rv2);
		    case TK_BXOR:
			*ty = max(ty1, ty2); 
			return (double) ((long) rv1 ^ (long) rv2);
		    case TK_LSHFT:
			*ty = max(ty1, ty2); 
			return (double) ((long) rv1 << (long) rv2);
		    case TK_RSHFT:
			*ty = max(ty1, ty2); 
			return (double) ((long) rv1 >> (long) rv2);
		    case TK_SLASH:
			if (rv2 == 0)
			    longjmp(is_error, 1);
			else
				*ty = WTP_DOUBLE;
			    return rv1 / rv2;
		    case TK_SLASHSLASH:
		    case TK_DIV:
			if (rv2 == 0)
			    longjmp(is_error, 1);
			else
				*ty = WTP_INTEGER;
			    return aint(rv1 / rv2);
		    case TK_MOD:
			if (rv2 == 0)
			    longjmp(is_error, 1);
			else
				*ty = WTP_INTEGER;
			    return rv1 - aint(rv1 / rv2) * rv2;
		    case TK_HAT:
			*ty = max(ty1, ty2); 
			rv = pow(rv1, rv2);
			if (errno == EDOM)
			    longjmp(is_error, 1);
			else
			    return (rv);
		    default:
			longjmp(is_error, 1);
		}
	    }
#ifndef DoubleType
	    else if (arity == 4 && functor == TK_DDOUBLE) {
		int   i;

		for (i = 0; i < 4; i++) {
		    w_get_argn(&v1, &t1, v, i + 1);
		    *(((short *) &rv) + i) = v1;
		}
		*ty = WTP_DOUBLE;
		return rv;
	    }
#endif /* DoubleType */
	    else
		longjmp(is_error, 1);
	    break;

#ifdef DoubleType
	case WTP_DOUBLE:
		ty = WTP_DOUBLE;
	    w_get_double(&rv, v);
	    return (rv);
#endif /* DoubleType */

	default:
	    longjmp(is_error, 1);
    }

    /*
     * We should never get here.
     */

    return 1;
}



void
make_number(v, t, d)
    PWord *v;
    int  *t;
    double d;
{
	make_numberx(v,t,d,WTP_INTEGER);
}

/*---------------------------------------------------------------
 | make_numberx(v, t, d, TP)
 | 
 | If TP == WTP_INTEGER and floor(d) == d
 |		and (MINPROLOGINT <= floor(d) <= MAXPROLOGINT),
 |		set: v -> floor(d); t --> WTP_INTEGER;
 | Else
 |		set:	v-> double repr. of d; 
 |				t -> correct type for v (depends on #ifdef DoubleType)
 *--------------------------------------------------------------*/

void
make_numberx(v, t, d, TP)
    PWord *v;
    int  *t, TP;
    double d;
{
	int fl;

if (TP == WTP_INTEGER) {
    if (MINPROLOGINT <= d && d <= MAXPROLOGINT) {
		fl = floor(d);
		if (fl == d) {
			*v = (PWord) floor(d);
			*t = WTP_INTEGER;
		}
		else /* d is not an integer, so return it as a double */
			goto return_as_double;
    }
	else /* d cannot be represented as an integer, so return it as a double */
		goto return_as_double;
	}
else	/* TP != WTP_INTEGER */
	{
#ifndef DoubleType
	int   i;

return_as_double:
	w_mk_term(v, t, (PWord) TK_DDOUBLE, 4);
	for (i = 0; i < 4; i++)
	    w_install_argn(*v, i + 1, (PWord) (*(((short *) &d) + i)), WTP_INTEGER);
#else
return_as_double:
    w_mk_double(v, t, d);
#endif
    }
}


int
pbi_is()
{
    PWord v1, v2;
    int   t1, t2, ty;
	double is_res;

    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    if (setjmp(is_error)) {
	FAIL;
    }

	ty = WTP_INTEGER;
	is_res = do_is(v2, t2, &ty);

/*    make_number(&v2, &t2, do_is(v2, t2)); */

    make_numberx(&v2, &t2, is_res, ty);

    if (w_unify(v1, t1, v2, t2))
	SUCCEED;
    else
	FAIL;
}




#define NUMCOMP(name,rel)		\
int								\
name()						  	\
{							   	\
    PWord v1,v2;				\
    int t1,t2;					\
    int ty1,ty2;				\
								\
    w_get_An(&v1,&t1,1);		\
    w_get_An(&v2,&t2,2);		\
								\
    if (setjmp(is_error)) {		\
	FAIL;						\
    }							\
								\
	ty1 = WTP_INTEGER;			\
	ty2 = WTP_INTEGER;			\
								\
    if (do_is(v1,t1,&ty1) rel do_is(v2,t2,&ty2)) {			\
	SUCCEED;					\
    }							\
    else {						\
	FAIL;						\
    }							\
}


NUMCOMP(pbi_less, <)
NUMCOMP(pbi_greater, >)
NUMCOMP(pbi_equalorless, <=)
NUMCOMP(pbi_greaterorequal, >=)
NUMCOMP(pbi_arithequal, ==)
NUMCOMP(pbi_arithnotequal, !=)
