/*================================================================
 |         intrv.h
 |	Copyright (c) 1995 Applied Logic Systems, Inc.
 |
 |	Include file for the generated file intrv.c and int_net.c
 *===============================================================*/

#include "defs.h"

#if defined(INTCONSTR)

#include <math.h>

#include "fpbasis.h"

#define EQ      ==
#define NE      !=
#define LT      <
#define LE      <=
#define GT      >
#define GE      >=
#define AND     &&
#define OR      ||

#define BOOLEANKIND 0
#define INTEGERKIND 1
#define REALKIND    2

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif
#ifndef M_PI_2
#define M_PI_2 1.57079632679489661923
#endif
#ifndef M_E
#define M_E 2.7182818284590452354
#endif

#undef pi
#define pi()                M_PI

#define ln(x)               log(x)
#define ceiling(x)          ceil(x)

#define xlchng()            status |= xlchange
#define xhchng()            status |= xhchange
#define ylchng()            status |= ylchange
#define yhchng()            status |= yhchange
#define zlchng()            status |= zlchange
#define zhchng()            status |= zhchange
#define xflipped()          status |= xflip
#define yflipped()          status |= yflip
#define zflipped()          status |= zflip

#define xflip       (1 << 0)
#define xlchange    (1 << 1)
#define xhchange    (1 << 2)
#define yflip       (1 << 3)
#define ylchange    (1 << 4)
#define yhchange    (1 << 5)
#define zflip       (1 << 6)
#define zlchange    (1 << 7)
#define zhchange    (1 << 8)
#define redonode    (1 << 9)
#define link    	(1 << 10)
#define xcoalesce  	(1 << 11)
#define ycoalesce  	(1 << 12)
#define zcoalesce  	(1 << 13)

#define x_changed (xflip | xlchange | xhchange)
#define y_changed (yflip | ylchange | yhchange)
#define z_changed (zflip | zlchange | zhchange)

#define unflip(T)   if (status & T ## flip) { \
				int_fp t = T ## l; \
				status ^= T ## flip; \
				T ## l = (T ## h NE 0.0) ? (- T ## h) : 0.0; \
				T ## h = (t NE 0.0) ? (- t) : 0.0; \
				switch (status & (T ## lchange | T ## hchange)) { \
					case T ## lchange: \
					case T ## hchange: \
	/* Flips the Tlchange & Thchange*/  status ^= (T ## lchange | T ## hchange); \
			} \
	}

/*++++++++++++++++++ Instatiation for T = z,x,y ++++++++++++++++++++++++++++
#define unflip_z  if (status & zflip) { \
				int_fp t = zl; \
				status ^= zflip; \
				zl = (zh NE 0.0) ? (- zh) : 0.0; \
				zh = (t NE 0.0) ? (- t) : 0.0; \
				switch (status & (zlchange | zhchange)) { \
					case zlchange: \
					case zhchange: \
	--Flips the zlchange & zhchange:  status ^= (zlchange | zhchange); \
			} \
	}

#define unflip_x  if (status & xflip) { \
				int_fp t = xl; \
				status ^= xflip; \
				xl = (xh NE 0.0) ? (- xh) : 0.0; \
				xh = (t NE 0.0) ? (- t) : 0.0; \
				switch (status & (xlchange | xhchange)) { \
					case xlchange: \
					case xhchange: \
	--Flips the xlchange & xhchange:  status ^= (xlchange | xhchange); \
			} \
	}

#define unflip_y  if (status & yflip) { \
				int_fp t = yl; \
				status ^= yflip; \
				yl = (yh NE 0.0) ? (- yh) : 0.0; \
				yh = (t NE 0.0) ? (- t) : 0.0; \
				switch (status & (ylchange | yhchange)) { \
					case ylchange: \
					case yhchange: \
	--Flips the ylchange & yhchange:  status ^= (ylchange | yhchange); \
			} \
	}
 ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/

#define GET_DBL_VAL(TYP, TRM, VAL) \
		if (TYP == WTP_STRUCTURE) { \
			w_get_arity(&arity, TRM); \
            w_get_functor(&functor, TRM); \
            if (arity == 4 && functor == TK_DDOUBLE){ \
				for (i = 0; i < 4; i++){ \
					w_get_argn(&vv, &tt, TRM, i + 1); \
					*(((short *) VAL) + i) = (short) vv; }  } \
			else \
				iaerror();  }

		/******************************************************
		 |	BOOLEAN STUFF
		 ******************************************************/
#define FIRSTBOOLEAN	23

#define booleanZero     0x00
#define booleanOne      0x01

/*--------- declared in int_net.c: ------------------------*/

extern long booleanInput;
extern long booleanOutput;


/* --------------- Masks for boolean encodings -------------* 
Encodings:
----------
booleanInput:   |0, 0, ydef, y, xdef, x, zdef, z|

booleanOutput:  |0, disable, ychg, y, xchg, x, zchg, z|
                |1, 0, 0, 0, 0, 0, 0, 0|      ---> error
                |1, 1, 1, 1, 1, 1, 1, 1|      ---> failure
 *----------------------------------------------------------*/


#define booleanError    0x80        /*booleanOutput*/
#define booleanFail     0xff        /*booleanOutput*/
#define booleanDisable  0x40        /*booleanOutput*/

#define boolean_z       0x01        /*booleanInput and booleanOutput*/
#define boolean_zdef    0x02        /*booleanInput*/
#define boolean_zchg    0x02        /*booleanOutput*/

#define boolean_x       0x04        /*booleanInput and booleanOutput*/
#define boolean_xdef    0x08        /*booleanInput*/
#define boolean_xchg    0x08        /*booleanOutput*/

#define boolean_y       0x10        /*booleanInput and booleanOutput*/
#define boolean_ydef    0x20        /*booleanInput*/
#define boolean_ychg    0x20        /*booleanOutput*/

#define boolean_chg    0x2A        /*booleanOutput*/

		/******************************************************
		 |	END BOOLEAN STUFF
		 ******************************************************/



/*********************************************************************
	NEED DFNS or EXTERN DECLS:
deact  -- comes from macro defn of persistent;  needs change
		-- only in "support" routines (e.g., higher)

iaerror		-- 

round	--
	only called at 2 places in wrap

ALSO clarify: w_Error (Error) label in wrap
***********************************************************************/


#define fp			double
#define int_fp		fp

	/* Move/change following externs when defined: */
extern void iaerror    (void);
extern void deact    (void);
	/* Dummy */
extern fp int_round    ( fp );

#define LOWER_BOUND 0
#define UPPER_BOUND 1

typedef union fpoverlay {
		long l[2] ;
		fp f ;
		} fpoverlay;

#ifdef REVERSE_ENDIAN
#define FIRST   1
#define SECOND  0

/* This is fix for a common interaction between optimizing compilers and
   the intel floating point instructions.
  
   a = <very very small number, for example 0x00000000 00000001>;
   b = a * a; <* b will have the value 0.0, but underflow and percision
   		  exception flags are set - this will cause the if statement
   		  to produce incorrect results *>
   
   if (b > 0) printf("greater\n");
   else if (b < 0) printf("less than\n");
   else if (b == 0) printf("equal\n");
   else printf("all compares failed\n");
  
   The workaround solution is to insert a noop function call between the
   calculation and the test.  This will cause the compiler to reload the
   numbers and clear the exception flags.  
*/
void noop(void);
#define INTEL_CLEAR_EXCEPTIONS noop()
#else
#define FIRST   0
#define SECOND  1
#define INTEL_CLEAR_EXCEPTIONS
#endif

	/* This may be wrong for SunOS (or others) -- check for iszero defined in include */
    /* This may be unused:-----------------
#define iszero(x)	       (x EQ -0.0)
#define int_fp		  fp
    -------------------------------------*/

#ifdef HAVE_NEXTAFTER

#define next(x)		 x = nextafter(x, maxfp)
#define prev(x)		 x = nextafter(x, -maxfp)

#ifdef HAVE_FPSETROUND

#define upperbd(y, x)   fpsetround(FP_RP); x = y
#define lowerbd(y, x)   fpsetround(FP_RM); x = y
#define upperbd2(y, x)  fpsetround(FP_RN); x = nextafter(y, maxfp)
#define lowerbd2(y, x)  fpsetround(FP_RN); x = nextafter(y, -maxfp)
#define initfpu()	       maxfp = DBL_MAX
#define resetfpu()	      fpsetround(FP_RN)

#else  /* not-HAVE_FPSETROUND */

#define fpu(a,b,c)	      { char *out; (void)ieee_flags(a, b, c, &out); }
#define upperbd(y, x)   fpu("set", "direction", "positive"); x = y
#define lowerbd(y, x)   fpu("set", "direction", "negative"); x = y
#define upperbd2(y, x)  fpu("set", "direction", "nearest"); x = nextafter(y, maxfp)
#define lowerbd2(y, x)  fpu("set", "direction", "nearest"); x = nextafter(y, -maxfp)
#define initfpu()	       fpu("set", "precision", "double"); maxfp = DBL_MAX
#define resetfpu()	      fpu("clearall", "", "")

#endif 
/* HAVE_FPSETROUND */

#else /* not-HAVE_NEXTAFTER */

				/*------- THIS IS THE COMMON PORTABLE VERSION ---------*/
#define next(x)	 INTEL_CLEAR_EXCEPTIONS; \
			if (x GE 0.0) { \
				if (++(((fpoverlay *)&x)->l[SECOND]) EQ 0) \
					++(((fpoverlay *)&x)->l[FIRST]); \
				} \
			else if (x LT 0.0) { \
				if (--(((fpoverlay *)&x)->l[SECOND]) EQ -1) \
					--(((fpoverlay *)&x)->l[FIRST]); \
				}
#define prev(x)	 INTEL_CLEAR_EXCEPTIONS; \
			if (x GT 0.0) { \
				if (--(((fpoverlay *)&x)->l[SECOND]) EQ -1) \
					--(((fpoverlay *)&x)->l[FIRST]); \
				} \
			else if (x LT 0.0) { \
				if (++(((fpoverlay *)&x)->l[SECOND]) EQ 0) \
					++(((fpoverlay *)&x)->l[FIRST]); \
				} \
			else { \
				((fpoverlay *)&x)->l[SECOND] = 1; \
				((fpoverlay *)&x)->l[FIRST] = 0x80000000; \
				}

#ifdef HAVE_FPSETROUND

#define upperbd(y, x)   fpsetround(FP_RP); x = y
#define lowerbd(y, x)   fpsetround(FP_RM); x = y
#define upperbd2(y, x)  fpsetround(FP_RN); x = nextafter(y, maxfp)
#define lowerbd2(y, x)  fpsetround(FP_RN); x = nextafter(y, -maxfp)
#define initfpu()	       maxfp = DBL_MAX
#define resetfpu()	      fpsetround(FP_RN)

#elif HAVE_SWAPRM /* HAVE_SWAPRM AND not-HAVE_FPSETROUND */

#define upperbd(y, x)   (void)swapRM(ROUND_TO_PLUS_INFINITY); x = y
#define lowerbd(y, x)   (void)swapRM(ROUND_TO_MINUS_INFINITY); x = y
#define upperbd2(y, x)  upperbd(y, x)
#define lowerbd2(y, x)  lowerbd(y, x)
#define initfpu()	       BNRP_oldRM = swapRM(ROUND_TO_PLUS_INFINITY)
#define resetfpu()	      (void)swapRM(BNRP_oldRM)
int BNRP_oldRM;

#else /* not-HAVE_FPSETROUND,  not-HAVE_SWAPRM */

				/*------- THIS IS THE COMMON PORTABLE VERSION ---------*/
#define upperbd(y, x)   x = y; next(x)
#define lowerbd(y, x)   x = y; prev(x)
#define upperbd2(y, x)  x = y; next(x)
#define lowerbd2(y, x)  x = y; prev(x)
#define initfpu()
#define resetfpu()

#endif  /* HAVE_SWAPRM */

#endif /* HAVE_NEXTAFTER */

#define swap(x)	{ int_fp temp = x ## l; \
					x ## l = (x ## h NE 0.0) ? (- x ## h) : 0.0; \
					x ## h = (temp NE 0.0) ? (- temp) : 0.0; \
				} \
				x ## flipped();

#ifndef DoubleType
#define DblRtrnType WTP_STRUCTURE
#else
#define DblRtrnType WTP_DOUBLE
#endif


/* QUEUE STUFF */

#define MAXITERS 10000

typedef struct {
	int    opcd;
	PWord  *z, *x, *y, *goal;
	int    zt, xt, yt;
	} primop;

#define TYPE_POSITION		1
#define USED_BY_POSITION	3
#define UIA_POSITION		4
#define LINK_POSITION		5

#define UIA_DIMENSION		24
#define UIA_FIRST_POS		0
#define UIA_SECOND_POS		8
#define UIA_THIRD_POS		16

typedef struct {
	long    pad;
	double lower,upper;
	} uia_dbls;

	/*--------------------------------------------------------------* 
	 |	Macro to check whether an incoming constraint argument 
	 |	(one of Z, X, Y) is ok
	 *--------------------------------------------------------------*/

#ifndef DoubleType
#define OK_CSTR_ARG(AA) if ((AA ## t != WTP_INTEGER) && (AA ## t != WTP_STRUCTURE) \
			&& (AA ## t != WTP_SYMBOL) && \
			((AA ## t != WTP_REF) || (!CHK_DELAY(AA)))) \
		FAIL;

#else
#define OK_CSTR_ARG(AA) if ((AA ## t != WTP_INTEGER) && (AA ## t != WTP_DOUBLE) \
			&& (AA ## t != WTP_SYMBOL) && \
			((AA ## t != WTP_REF) || (!CHK_DELAY(AA)))) \
		FAIL;
#endif

/*  ORIGINAL non-macro forms:------------------
#ifndef DoubleType
	if ((Zt != WTP_INTEGER) && (Zt != WTP_STRUCTURE) &&
			((Zt != WTP_REF) || (!CHK_DELAY(Z))))
		FAIL;
	if ((Xt != WTP_INTEGER) && (Xt != WTP_STRUCTURE) &&
			((Xt != WTP_REF) || (!CHK_DELAY(X))))
		FAIL;
	if ((Yt != WTP_INTEGER) && (Yt != WTP_STRUCTURE) &&
			((Yt != WTP_REF) || (!CHK_DELAY(Y))))
		FAIL;
#else
	if ((Zt != WTP_INTEGER) && (Zt != WTP_DOUBLE) &&
			((Zt != WTP_REF) || (!CHK_DELAY(Z))))
		FAIL;
	if ((Xt != WTP_INTEGER) && (Xt != WTP_DOUBLE) &&
			((Xt != WTP_REF) || (!CHK_DELAY(X))))
		FAIL;
	if ((Yt != WTP_INTEGER) && (Yt != WTP_DOUBLE) &&
			((Yt != WTP_REF) || (!CHK_DELAY(Y))))
		FAIL;
#endif
------------------------------------*/

#endif /* defined(INTCONSTR) */


/****************************************************************
   WORKING OUT THE FEATURES:
	[Autoconf tests need to be developed]

   For SPARC/Solaris:

   #define HAVE_IEEE_FP     1
   #define HAVE_NEXTAFTER  1
   #define HAVE_FPSETROUND 1

 ****************************************************************/
