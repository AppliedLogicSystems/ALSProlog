/*================================================================*
 |		compmath.c                   
 |	Copyright (c) 1987-1995 Applied Logic Systems, Inc.
 |
 |		-- arithmetic compiler
 |
 | Author: Kevin A. Buettner
 | Creation: 3/22/87
 *================================================================*/

#include "defs.h"
#include "varproc.h"
#include "compile.h"
#include "module.h"
#include "icode.h"
#include "icodegen.h"

#ifdef InMath

#ifndef NewMath

#ifdef FMath
extern int IsFloat;

#endif

static int comp_exp();

/*
 * comp_math is called from compile.c with three arguments.
 *
 *      goal            -- the goal to compile
 *      islastgoal      -- 1 if goal is the last, 0 otherwise
 *      isenv           -- 1 if an environment has been allocated, 0 otherwise
 */

comp_math(goal, islastgoal, deallocneeded)
    pword goal;
    int   islastgoal;
    int   deallocneeded;
{
    int   functor;
    int   rv;

    functor = FUNCTOR_TOKID(TERM_FUNCTOR(goal));
    rv = 0;

    if (functor == TK_IS) {
#ifdef FMath
	IsFloat = 0;		/* Don't know what we have yet */
#endif

	if (rv = comp_exp(TERM_ARGN(goal, 2))) {
	    if (TYPEOF(TERM_ARGN(goal, 1)) == TP_VO) {
		int   i = VO_VAL(TERM_ARGN(goal, 1));
		int   loc = vtbl[i].home;

		if (vtbl[i].usecnt)
		    icode(I_MTH_GETVAL, index_of(loc), disp_of(loc), 0, 0);
		else if (loc) {
		    icode(I_MTH_PUTINT, index_of(loc), disp_of(loc), 0, 0);

		    vtbl[i].unsafe = 0;
		}
		else {
		    /*
		     * We can optimize out the arithmetic,
		     * but for now get rid of it.
		     */
		    rv = 0;
		}
	    }
	    else if (rv = comp_exp(TERM_ARGN(goal, 1)))
		icode(I_MTH_EQ, 0, 0, 0);
	}
    }
    else {
	rv = (comp_exp(TERM_ARGN(goal, 1)) && comp_exp(TERM_ARGN(goal, 2)));
	if (rv)
	    switch (functor) {
		case TK_LESS:
		    icode(I_MTH_LT, 0, 0, 0);
		    break;
		case TK_GRT:
		    icode(I_MTH_GT, 0, 0, 0);
		    break;
		case TK_LEQ:
		    icode(I_MTH_LE, 0, 0, 0);
		    break;
		case TK_GEQ:
		    icode(I_MTH_GE, 0, 0, 0);
		    break;
		case TK_ZEBRA:
		    icode(I_MTH_EQ, 0, 0, 0);
		    break;
		case TK_ZEBRA2:
		    icode(I_MTH_NE, 0, 0, 0);
		    break;
		default:
		    rv = 0;
		    break;
	    }
    }

    if (rv && islastgoal) {
	if (deallocneeded)
	    icode(I_INLINE_PROCEED, 0, 0, 0, 0);
	else
	    icode(I_PROCEED, index_of(vtbl[RETIDX].home),
		  disp_of(vtbl[RETIDX].home), 0, 0);
    }

    return (rv);
}

static int
comp_exp(e)
    pword e;
{
    int   id, arity, loc;

    switch (TYPEOF(e)) {
	case TP_INT:
	    icode(I_MTH_PUSHINT, INT_VAL(e), 0, 0, 0);
	    return (1);
	    break;
	case TP_VO:
	    id = VO_VAL(e);
	    loc = vtbl[id].home;

	    if (vtbl[id].usecnt) {
		icode(I_MTH_GETINT, index_of(loc), disp_of(loc), 0, 0);
		return (1);
	    }
	    else
		return (0);
	    break;
	case TP_TERM:
	    arity = TERM_ARITY(e);
	    id = FUNCTOR_TOKID(TERM_FUNCTOR(e));

	    if (arity == 1) {
		if (!comp_exp(TERM_ARGN(e, 1)))
		    return 0;

		switch (id) {
		    case TK_MINUS:
			icode(I_MTH_NEG, 0, 0, 0, 0);
			break;
		    case TK_BACKSLASH:
		    case TK_NOT:
			icode(I_MTH_NOT, 0, 0, 0);
			break;
#ifdef FMath
		    case TK_ABS:
			icode(I_MTH_ABS, 0, 0, 0);
			break;
		    case TK_SIN:
			icode(I_MTH_SIN, 0, 0, 0);
			break;
		    case TK_COS:
			icode(I_MTH_COS, 0, 0, 0);
			break;
		    case TK_TAN:
			icode(I_MTH_TAN, 0, 0, 0);
			break;
		    case TK_ASIN:
			icode(I_MTH_ASIN, 0, 0, 0);
			break;
		    case TK_ACOS:
			icode(I_MTH_ACOS, 0, 0, 0);
			break;
		    case TK_ATAN:
			icode(I_MTH_ATAN, 0, 0, 0);
			break;
		    case TK_SQRT:
			icode(I_MTH_SQRT, 0, 0, 0);
			break;
		    case TK_EXP:
			icode(I_MTH_EXP, 0, 0, 0);
			break;
		    case TK_EXP10:
			icode(I_MTH_EXP10, 0, 0, 0);
			break;
		    case TK_LOG:
			icode(I_MTH_LOG, 0, 0, 0);
			break;
		    case TK_LOG10:
			icode(I_MTH_LOG10, 0, 0, 0);
			break;
		    case TK_FLOOR:
			icode(I_MTH_FLOOR, 0, 0, 0);
			break;
		    case TK_ROUND:
			icode(I_MTH_ROUND, 0, 0, 0);
			break;
		    case TK_TRUNCATE:
			icode(I_MTH_TRUNC, 0, 0, 0);
			break;
#endif
		    default:
			return 0;
		}

		return (1);
	    }
	    else if (arity == 2) {
		if (!(comp_exp(TERM_ARGN(e, 1)) && comp_exp(TERM_ARGN(e, 2))))
		    return (0);

		switch (id) {
		    case TK_PLUS:
			icode(I_MTH_ADD, 0, 0, 0, 0);
			break;
		    case TK_MINUS:
			icode(I_MTH_SUB, 0, 0, 0, 0);
			break;
		    case TK_STAR:
			icode(I_MTH_MUL, 0, 0, 0, 0);
			break;
		    case TK_SLASHSLASH:
		    case TK_DIV:
			icode(I_MTH_DIV, 0, 0, 0, 0);
			break;
		    case TK_MOD:
			icode(I_MTH_MOD, 0, 0, 0);
			break;
		    case TK_BAND:
			icode(I_MTH_BAND, 0, 0, 0);
			break;
		    case TK_BOR:
			icode(I_MTH_BOR, 0, 0, 0);
			break;
#ifndef arch_m88k
		    case TK_BXOR:
			icode(I_MTH_BXOR, 0, 0, 0);
			break;
#endif /* arch_m88k */
		    case TK_LSHFT:
			icode(I_MTH_LSHFT, 0, 0, 0);
			break;
		    case TK_RSHFT:
			icode(I_MTH_RSHFT, 0, 0, 0);
			break;
#ifdef FMath
		    case TK_SLASH:
			icode(I_MTH_FDIV, 0, 0, 0);
			break;
		    case TK_HAT:
			icode(I_MTH_POWER, 0, 0, 0);
			break;
#endif
		    default:
			return (0);
		}
		return (1);
	    }
	    else
		return (0);

	    break;
	case TP_LIST:
	    if (TYPEOF(LIST_CDR(e)) == TP_SYM &&
		FUNCTOR_TOKID(LIST_CDR(e)) == TK_NIL)
		return (comp_exp(LIST_CAR(e)));
	    else
		return 0;

	    break;
	default:
	    return (0);
	    break;
    }
}


#else  /* NewMath */

/*
 * The following is the new math code.
 *
 * xyzzy
 */

/*
 * The icm macro maps tokens to icode numbers.  The tokens must appear in the
 * order listed here in tokini.h.
 */

#define icm(t) cmictab[(t)-TK_HEAPUSED]
int   cmictab[] =
{
    I_MTH_HEAPUSED,
    I_MTH_CPUTIME,
    I_MTH_REALTIME,
    I_MTH_RANDOM,
    I_MTH_NOT,
    I_MTH_NOT,
    I_MTH_ABS,
    I_MTH_SIN,
    I_MTH_SINH,
    I_MTH_COS,
    I_MTH_COSH,
    I_MTH_TAN,
    I_MTH_TANH,
    I_MTH_ASIN,
    I_MTH_ACOS,
    I_MTH_ATAN,
    I_MTH_SQRT,
    I_MTH_EXP,
    I_MTH_EXP10,
    I_MTH_LOG,
    I_MTH_LOG10,
    I_MTH_FLOOR,
    I_MTH_ROUND,
    I_MTH_CEIL,
    I_MTH_ERF,
    I_MTH_ERFC,
    I_MTH_GAMMA,
    I_MTH_J0,
    I_MTH_J1,
    I_MTH_Y0,
    I_MTH_Y1,
    I_MTH_TRUNC,
    I_MTH_ADD,			/* unary plus and minus handled separately */
    I_MTH_SUB,
    I_MTH_ATAN2,
    I_MTH_FMOD,
    I_MTH_HYPOT,
    I_MTH_JN,
    I_MTH_YN,
    I_MTH_MUL,
    I_MTH_DIV,			/* // */
    I_MTH_DIV,			/* div */
    I_MTH_MOD,
    I_MTH_BAND,
    I_MTH_BOR,
    I_MTH_BXOR,
    I_MTH_LSHFT,
    I_MTH_RSHFT,
    I_MTH_FDIV,			/* / */
    I_MTH_POWER
};



static	void	comp_exp	( pword );

/*
 * comp_math is called from compile.c with four arguments.
 *
 *      goal            -- the goal to compile
 *      isonlygoal      -- 1 if goal is the only one in the clause, 0 otherwise
 *      regmask         -- 32 bit mask with bits set to 1 if that register
 *                         is in use.
 *      stackadj        -- number of words (positive) between SP and E.
 */

void
comp_math(goal, isonlygoal, regmask, stackadj)
    pword goal;
    int   isonlygoal;
    long  regmask;
    long  stackadj;
{
    int   functor;

    functor = FUNCTOR_TOKID(TERM_FUNCTOR(goal));

    icode(I_MTH_INIT1, isonlygoal, regmask, 0, 0);
    gccallinfo();		/* we need the call info if we call out */
    icode(I_MTH_INIT2, regmask, stackadj, 0, 0);
    if (functor == TK_IS) {

	comp_exp(TERM_ARGN(goal, 2));
	if (TYPEOF(TERM_ARGN(goal, 1)) == TP_VO) {
	    int   i = VO_VAL(TERM_ARGN(goal, 1));
	    int   loc = vtbl[i].home;

	    if (vtbl[i].usecnt) {
		icode(I_MTH_GETVAL, index_of(loc), disp_of(loc), 0, 0);
	    }
	    else {
		if (!loc) {
		    loc = find_temp();
		    vtbl[i].home = loc;
		}

		icode(I_MTH_PUTNUM, index_of(loc), disp_of(loc), 0, 0);

		vtbl[i].unsafe = 0;
	    }
	    vtbl[i].usecnt++;
	}
	else {
	    comp_exp(TERM_ARGN(goal, 1));
	    icode(I_MTH_EQ, 0, 0, 0, 0);
	}
    }
    else {
	comp_exp(TERM_ARGN(goal, 2));
	comp_exp(TERM_ARGN(goal, 1));
	switch (functor) {
	    case TK_LESS:
		icode(I_MTH_LT, 0, 0, 0, 0);
		break;
	    case TK_GRT:
		icode(I_MTH_GT, 0, 0, 0, 0);
		break;
	    case TK_LEQ:
		icode(I_MTH_LE, 0, 0, 0, 0);
		break;
	    case TK_GEQ:
		icode(I_MTH_GE, 0, 0, 0, 0);
		break;
	    case TK_ZEBRA:
		icode(I_MTH_EQ, 0, 0, 0, 0);
		break;
	    case TK_ZEBRA2:
		icode(I_MTH_NE, 0, 0, 0, 0);
		break;
	    default:
		break;
	}
    }

    icode(I_MTH_FIN, 0, 0, 0, 0);
}

static void
comp_exp(e)
    pword e;
{
    int   id, arity, loc;

    switch (TYPEOF(e)) {
	case TP_INT:
	    icode(I_MTH_PUSHINT, INT_VAL(e), 0, 0, 0);
	    break;
	case TP_VO:
	    id = VO_VAL(e);
	    loc = vtbl[id].home;

	    if (!vtbl[id].usecnt) {
		if (!loc) {
		    loc = find_temp();
		    vtbl[id].home = loc;
		}
		icode(I_P_XVAR, index_of(loc), disp_of(loc), 0, 0);
	    }
	    icode(I_MTH_GETNUM, index_of(loc), disp_of(loc), 0, 0);
	    vtbl[id].usecnt++;
	    break;
	case TP_SYM:
	    id = FUNCTOR_TOKID(e);
	    if (TK_HEAPUSED <= id && id <= TK_RANDOM)
		icode(icm(id), 0, 0, 0, 0);
	    else
		comp_math_struct(e);
	    break;
	case TP_TERM:
	    arity = TERM_ARITY(e);
	    id = FUNCTOR_TOKID(TERM_FUNCTOR(e));

	    if (arity == 1) {
		if (TK_NOT <= id && id <= TK_TRUNCATE) {
		    comp_exp(TERM_ARGN(e, 1));
		    icode(icm(id), 0, 0, 0, 0);
		}
		else if (TK_PLUS == id)
		    comp_exp(TERM_ARGN(e, 1));
		else if (TK_MINUS == id) {
		    comp_exp(TERM_ARGN(e, 1));
		    icode(I_MTH_NEG, 0, 0, 0, 0);
		}
		else
		    comp_math_struct(e);
	    }
	    else if (arity == 2) {
		if (TK_PLUS <= id && id <= TK_HAT) {
		    comp_exp(TERM_ARGN(e, 1));

		    /*
		     * Optimize when second argument is an integer
		     */
		    if (TYPEOF(TERM_ARGN(e, 2)) == TP_INT) {
			if (id == TK_PLUS) {
			    icode(I_MTH_ADDI, INT_VAL(TERM_ARGN(e, 2)), 0, 0, 0);
			    return;
			}
			else if (id == TK_MINUS) {
			    icode(I_MTH_SUBI, INT_VAL(TERM_ARGN(e, 2)), 0, 0, 0);
			    return;
			}
		    }

		    comp_exp(TERM_ARGN(e, 2));
		    icode(icm(id), 0, 0, 0, 0);
		}
		else
		    comp_math_struct(e);
	    }
#ifndef DoubleType
	    else if (arity == 4 && id == TK_DDOUBLE) {
		double dbl;
		int   i;

		for (i = 0; i < 4; i++)
		    ((short *) &dbl)[i] = (short) INT_VAL(TERM_ARGN(e, i + 1));
#ifdef COERCE2INTS
		if (dbl == (double) (long) dbl)
		    icode(I_MTH_PUSHINT, (long) dbl, 0, 0, 0);
		else
#endif
		    icode(I_MTH_PUSHDBL, ((long *) &dbl)[0],
			  ((long *) &dbl)[1], 0, 0);
	    }
#endif
	    else {
		comp_math_struct(e);
	    }

	    break;
	case TP_LIST:
	    if (TYPEOF(LIST_CDR(e)) == TP_SYM &&
		FUNCTOR_TOKID(LIST_CDR(e)) == TK_NIL)
		comp_exp(LIST_CAR(e));
	    else
		comp_math_struct(e);
	    break;
#ifdef DoubleType
	case TP_DOUBLE:{
		double dbl = double_val(e);

#ifdef COERCE2INTS
		if (dbl == (double) (long) dbl)
		    icode(I_MTH_PUSHINT, (long) dbl, 0, 0, 0);
		else
#endif
		    icode(I_MTH_PUSHDBL, DOUBLE_VAL1(e), DOUBLE_VAL2(e), 0, 0);
	    }
	    break;
#endif
	default:
	    comp_math_struct(e);
	    break;
    }
}


#endif /* NewMath */
#endif /* InMath */
