/*================================================================
 |         intrv.h
 |	Copyright (c) 1995 Applied Logic Systems, Inc.
 |
 |	Include file for the generated file intrv.c
 *===============================================================*/

#include "defs.h"

#if defined(INTCONSTR)

#include <math.h>

#define EQ      ==
#define NE      !=
#define LT      <
#define LE      <=
#define GT      >
#define GE      >=
#define AND     &&
#define OR      ||

#undef pi
#ifdef HAVE_M_PI
#define pi()                M_PI
#else 
#define pi()                3.14159265358979323846
#endif
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

/****************
	NEED DFNS or EXTERN DECLS:
deact  -- comes from macro defn of persistent;  needs change
		-- only in "support" routines (e.g., higher)

iaerror		-- 

round	--
	only called at 2 places in wrap

ALSO clarify: w_Error (Error) label in wrap:

****************/

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


#define fp			double
#define int_fp		fp

	/* Drop following externs if/when intaux.c is included in this file */
extern void extract_bds    PARAMS( (PWord *, fp *, fp *) );
extern void iaerror    PARAMS( (void) );
extern void deact    PARAMS( (void) );
	/* Dummy */
extern fp round    PARAMS(( fp ));

#define LOWER_BOUND 0
#define UPPER_BOUND 1

typedef union fpoverlay {
		long l[2] ;
		fp f ;
		} fpoverlay;

#ifdef REVERSE_ENDIAN
#define FIRST   1
#define SECOND  0
#else
#define FIRST   0
#define SECOND  1
#endif

	/* This may be wrong for SunOS (or others) -- check for iszero defined in include */
    /* THis may be unused:
#define iszero(x)	       (x EQ -0.0)
#define int_fp		  fp
    */

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

#define next(x)	 if (x GE 0.0) { \
				if (++(((fpoverlay *)&x)->l[SECOND]) EQ 0) \
					++(((fpoverlay *)&x)->l[FIRST]); \
				} \
			else if (x LT 0.0) { \
				if (--(((fpoverlay *)&x)->l[SECOND]) EQ -1) \
					--(((fpoverlay *)&x)->l[FIRST]); \
				}
#define prev(x)	 if (x GT 0.0) { \
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

#elif HAVE_SWAPRM /* not-HAVE_FPSETROUND */

#define upperbd(y, x)   (void)swapRM(ROUND_TO_PLUS_INFINITY); x = y
#define lowerbd(y, x)   (void)swapRM(ROUND_TO_MINUS_INFINITY); x = y
#define upperbd2(y, x)  upperbd(y, x)
#define lowerbd2(y, x)  lowerbd(y, x)
#define initfpu()	       BNRP_oldRM = swapRM(ROUND_TO_PLUS_INFINITY)
#define resetfpu()	      (void)swapRM(BNRP_oldRM)
int BNRP_oldRM;

#else /* not-HAVE_FPSETROUND,  not-HAVE_SWAPRM */

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

#endif /* defined(INTCONSTR) */
