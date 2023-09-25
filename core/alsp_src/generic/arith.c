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
#endif

#include "fpbasis.h"

#include <time.h>
#include <setjmp.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#include "random.h"

extern	double	als_random	( void );
#ifndef HAVE_AINT
extern	double	aint		( double );
#endif
/*****
#ifndef HAVE_RINT
extern	double	rint		( double );
#endif
****/
#ifndef HAVE_EXP10
extern	double	exp10		( double );
#endif
static	double	do_is		( PWord, int, int * );

static jmp_buf is_error;

enum {
	IS_INSTANTIATION_ERROR = 1,
	IS_EVALUABLE_TYPE_ERROR,
	IS_INTEGER_TYPE_ERROR,
	IS_FLOAT_OVERFLOW_ERROR,
	IS_INT_OVERFLOW_ERROR,
	IS_UNDERFLOW_ERROR,
	IS_ZERO_DIVISOR_ERROR,
	IS_UNDEFINED_ERROR
};

PWord error_functor; int error_arity;

/* Define how to handle random numbers */

#ifdef HAVE_SRAND48
#define drandom drand48
#define srandom srand48
extern	void	srand48		( long );
extern	double	drand48		( void );

#elif defined(HAVE_SRANDOM)
#define RANDRANGE 0x7fffffff
#ifndef __DJGPP__
extern	long	random		( void );
extern	void	srandom		( int );
#endif

#elif defined(HAVE_SRAND)
#define random rand
#define srandom srand
#define RANDRANGE 0x7fffffff
#endif




#ifdef HAVE_TIME
int
pbi_time(void)
{				/* real system time */
    PWord vsec, vmin, vhour, vmday, vmon, vyear, vwday, vyday, visdst, vkind;
    int   tsec, tmin, thour, tmday, tmon, tyear, twday, tyday, tisdst, tkind;
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
    w_get_An(&vkind, &tkind, 10);

    if (tkind != WTP_INTEGER) 
	FAIL;

    tv = time(0L);
	if (vkind == 0)
    	    tp = localtime(&tv);
	else {
	    if (vkind == 1)
    		tp = gmtime(&tv);
	    else
		FAIL;
	}

    if (w_unify(vsec, tsec, (PWord) tp->tm_sec, WTP_INTEGER) &&
	w_unify(vmin, tmin, (PWord) tp->tm_min, WTP_INTEGER) &&
	w_unify(vhour, thour, (PWord) tp->tm_hour, WTP_INTEGER) &&
	w_unify(vmday, tmday, (PWord) tp->tm_mday, WTP_INTEGER) &&
	w_unify(vmon, tmon, (PWord) tp->tm_mon, WTP_INTEGER) &&
	w_unify(vyear, tyear, (PWord) (1900+tp->tm_year), WTP_INTEGER) &&
	w_unify(vwday, twday, (PWord) tp->tm_wday, WTP_INTEGER) &&
	w_unify(vyday, tyday, (PWord) tp->tm_yday, WTP_INTEGER) &&
	w_unify(visdst, tisdst, (PWord) tp->tm_isdst, WTP_INTEGER))
	SUCCEED;
    else
	FAIL;
}
#endif /* HAVE_TIME */

int
pbi_srandom(void)
{
    PWord v1;
    int t1;
    double seed;

    w_get_An(&v1,&t1,1);

    if (get_number(v1,t1,&seed)) {
	minimal_standard_rand_init((long) seed);
	SUCCEED;
    }
    else
	FAIL;
}


#ifndef HAVE_AINT
/* aint rounds towards zero */
double
aint(double d)
{
    if (d < 0)
	return (ceil(d));
    else
	return (floor(d));
}
#endif /* HAVE_AINT */

/********
#ifndef HAVE_RINT
double
rint(double d)
{
    return (floor(d + 0.5));
}
#endif
*******/
static double
rint0(double d)
{
    return (floor(d + 0.5));
}

#ifndef HAVE_EXP10
double exp10(double d)
{
    return pow((double) 10, d);
}

#endif

#if 0
#if defined(HAVE_LGAMMA) && !defined(HAVE_GAMMA)
/*
 * NeXT manual page documentation suggests the following to get the gamma
 * function.
 */

double gamma(double x);
double
gamma(double x)
{
    return lgamma(x);
}

#endif
#endif

/*------------------------------------------------------------------------------*
 |	Symbolic floating point constants (also used in interval arithmetic)
 *------------------------------------------------------------------------------*/
double sym_f_cnst[] = {
	D_PI,			/* pi         */
	D_PI_2,    		/* pi/2       */ 
	D_E,       		/* e          */ 
	D_PI_4, 		/* pi/4       */ 
	D_1_PI,  		/* 1/pi       */
	D_2_PI,  		/* 2/pi       */
	D_2_SQRTPI,		/* 2/sqrt(pi) */
	D_LOG2E, 		/* log2(e)    */
	D_LOG10E,		/* log10(e)   */
	D_LN2,  		/* ln(2)      */
	D_LN10,			/* ln(10)     */
	D_SQRT2,        /* sqrt(2)    */
	D_SQRT1_2       /* sqrt(1/2)  */
};

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
do_is(PWord v, int t, int *ty)
{
    int   arity;
    PWord functor;
    PWord v1, v2;
    int   t1, t2;
    double rv;

    errno = 0;

    switch (t) {
	case WTP_UNBOUND:
	    longjmp(is_error, IS_INSTANTIATION_ERROR);

	case WTP_SYMBOL:
	    switch (v) {
		case TK_HEAPUSED:
		    return (double) (((long) wm_H) - ((long) wm_heapbase));
		case TK_CPUTIME:
		    return os_cputime();
		case TK_REALTIME:
		    return os_realtime();
		case TK_RANDOM:
		    return minimal_standard_random();
		case TK_PI:
		    return D_PI;
		case TK_E:
		    return D_E;
		default: 
		    error_functor = v; error_arity = 0;
		    longjmp(is_error, IS_EVALUABLE_TYPE_ERROR);
	    }

	case WTP_LIST:
	    w_get_car(&v1, &t1, v);
	    w_get_cdr(&v2, &t2, v);
	    if (t2 != WTP_SYMBOL && v2 != TK_NIL)
		longjmp(is_error, IS_INSTANTIATION_ERROR);
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
			rv = rint0(rv);
			break;
		    case TK_TRUNCATE: 
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
		    case TK_CEILING:
		    	*ty = WTP_INTEGER;
		    	rv = ceil(rv);
		    	break;
		    case TK_SQRT:
			rv = sqrt(rv);
			break;
		    case TK_ABS:
		    	*ty = ty1;
			if (rv < 0)
			    rv = -rv;
			break;
		    case TK_SIGN:
		    	*ty = ty1;
		    	if (rv < 0) rv = -1;
		    	else if (rv > 0) rv = 1;
		    	else rv = 0;
		    	break;
		    case TK_FLOAT_INTEGER_PART:
		    	*ty = WTP_DOUBLE;
		    	rv = aint(rv);
		    	break;
		    case TK_FLOAT_FRACTIONAL_PART:
		    	*ty = WTP_DOUBLE;
		    	rv = rv - aint(rv);
		    	break;
		    default:
			error_functor = functor; error_arity = arity;
			longjmp(is_error, IS_EVALUABLE_TYPE_ERROR);
		}

		if (errno != EDOM && errno != ERANGE)
		    return (rv);
		else
		    longjmp(is_error, IS_FLOAT_OVERFLOW_ERROR);
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
			    longjmp(is_error, IS_ZERO_DIVISOR_ERROR);
			else
			    *ty = WTP_DOUBLE;
			return rv1 / rv2;
		    case TK_SLASHSLASH:
		    case TK_DIV:
			if (rv2 == 0)
			    longjmp(is_error, IS_ZERO_DIVISOR_ERROR);
			else
			    *ty = WTP_INTEGER;
			return aint(rv1 / rv2);
		    case TK_MOD:
		    	if (ty1 != WTP_INTEGER || ty2 != WTP_INTEGER) {
		    	    error_functor = functor; error_arity = arity;
		    	    longjmp(is_error, IS_INTEGER_TYPE_ERROR);
		    	} if (rv2 == 0)
			    longjmp(is_error, IS_ZERO_DIVISOR_ERROR);
			else
			    *ty = WTP_INTEGER;
			return rv1 - floor(rv1 / rv2) * rv2;
		    case TK_REM:
			if (rv2 == 0)
			    longjmp(is_error, IS_ZERO_DIVISOR_ERROR);
			else
			    *ty = WTP_INTEGER;
			return rv1 - aint(rv1 / rv2) * rv2;
		    case TK_HAT:
		    case TK_2ST:
			*ty = WTP_DOUBLE;
			rv = pow(rv1, rv2);
			if (errno == EDOM)
			    longjmp(is_error, IS_UNDEFINED_ERROR);
			else
			    return (rv);
		    default:
			error_functor = functor; error_arity = arity;
			longjmp(is_error, IS_EVALUABLE_TYPE_ERROR);
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
	    else {
		error_functor = functor; error_arity = arity;
		longjmp(is_error, IS_EVALUABLE_TYPE_ERROR);
	    }
	    break;

#ifdef DoubleType
	case WTP_DOUBLE:
	    ty = WTP_DOUBLE;
	    w_get_double(&rv, v);
	    return (rv);
#endif /* DoubleType */

	default:
	    longjmp(is_error, IS_INSTANTIATION_ERROR);
    }

    /*
     * We should never get here.
     */

    return 1;
}



void
make_number(PWord *v, int *t, double d)
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
make_numberx(PWord *v, int *t, double d, int TP)
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
	return_as_double:
	w_mk_double(v, t, d);
    }
}


/*---------------------------------------------------------------
	make_ieee_nan(v, t)
	make_ieee_inf(v, t)
 *--------------------------------------------------------------*/

void
make_ieee_nan(PWord *v, int *t)
{
    w_mk_double(v, t, NAN);
}

void
make_ieee_inf(PWord *v, int *t)
{
    w_mk_double(v, t, INFINITY);
}

/*---------------------------------------------------------------
 *--------------------------------------------------------------*/

static int handle_is_error(int status)
{
    PWord s;
    int st;
    
    switch (status) {
    	case IS_INSTANTIATION_ERROR:
    		PERR_INSTANTIATION(TK_IS,2);
    		break;
    	case IS_EVALUABLE_TYPE_ERROR:
   		w_mk_term(&s, &st, TK_SLASH, 2);
   		w_install_argn(s, 1, error_functor, WTP_SYMBOL);
   		w_install_argn(s, 2, error_arity, WTP_INTEGER);
    		PERR_TYPE(TK_IS, 2, find_token((UCHAR *)"evaluable"), s, st);
    		break;
    	case IS_INTEGER_TYPE_ERROR:
   		w_mk_term(&s, &st, TK_SLASH, 2);
   		w_install_argn(s, 1, error_functor, WTP_SYMBOL);
   		w_install_argn(s, 2, error_arity, WTP_INTEGER);
    		PERR_TYPE(TK_IS, 2, find_token((UCHAR *)"integer"), s, st);
    		break;
    	case IS_FLOAT_OVERFLOW_ERROR:
    		PERR_EVALUATION(TK_IS, 2, find_token((UCHAR *)"float_overflow"));
   		break;
    	case IS_INT_OVERFLOW_ERROR:
    		PERR_EVALUATION(TK_IS, 2, find_token((UCHAR *)"int_overflow"));
   		break;
    	case IS_UNDERFLOW_ERROR:
    		PERR_EVALUATION(TK_IS, 2, find_token((UCHAR *)"underflow"));
   		break;
    	case IS_ZERO_DIVISOR_ERROR:
    		PERR_EVALUATION(TK_IS, 2, find_token((UCHAR *)"zero_divisor"));
   		break;
    	case IS_UNDEFINED_ERROR:
    		PERR_EVALUATION(TK_IS, 2, find_token((UCHAR *)"undefined"));
   		break;
   	default:
   		FAIL;
    }
}

int
pbi_is(void)
{
    PWord v1, v2;
    int   t1, t2, ty;
    double is_res;
    int status;
    
    w_get_An(&v1, &t1, 1);
    w_get_An(&v2, &t2, 2);

    status = setjmp(is_error);
    if (status) 
    	return handle_is_error(status);
    	

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
name(void)						\
{							   	\
    PWord v1,v2;				\
    int t1,t2;					\
    int ty1,ty2;				\
    int status;					\
								\
    w_get_An(&v1,&t1,1);		\
    w_get_An(&v2,&t2,2);		\
								\
    status = setjmp(is_error);					\
    if (status) {		\
	return handle_is_error(status);						\
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



int pbi_fpconst_val	( void );

int
pbi_fpconst_val(void)
{
    PWord cnst, value, fv;
    int   cnst_t, value_t, fv_t;
	double the_val = 0;

    w_get_An(&cnst, &cnst_t, 1);
    w_get_An(&value, &value_t, 2);

	if (cnst_t != WTP_SYMBOL) 
		FAIL;

	switch ((int)cnst) {
		FPCCASE(the_val)
	}
	make_number(&fv, &fv_t, the_val);

	if (w_unify(value, value_t, fv, fv_t))
		SUCCEED;
	else
		FAIL;
}

int pbi_uia_poke_fpconst 	( void );

int
pbi_uia_poke_fpconst(void)
{ 			/* uia_poke_fpconst(V,Sgn,UIA,Off) */
    PWord Val, Sgn, UIABuf, Off;
    int   Val_t, Sgn_t, UIABuf_t, Off_t;
	double the_val;

    w_get_An(&Val,		&Val_t, 1);
    w_get_An(&Sgn,		&Sgn_t, 2);
    w_get_An(&UIABuf,	&UIABuf_t, 3);
    w_get_An(&Off,		&Off_t, 4);

    if (UIABuf_t == WTP_UIA && Off_t == WTP_INTEGER && Sgn_t == WTP_INTEGER) {
		switch ((int)Val) {
			FPCCASE(the_val)
		}
		if ((int)Sgn < 0) { the_val = -the_val; }
	
		if (w_uia_poke(UIABuf, (int) Off, (UCHAR *) &the_val, sizeof (double)))
			SUCCEED;
		else
			FAIL;
	}
	else
		FAIL;
}
