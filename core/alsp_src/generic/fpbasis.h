/*===========================================================================*
 |              fpbasis.h
 |      Copyright (c) 1996-97 Applied Logic Systems, Inc.
 |
 |      -- Floating point math abstractions
 |
 *===========================================================================*/

#ifndef MacOS
extern int isnan PARAMS( (double) );
extern int isinf PARAMS( (double) );
extern int finite PARAMS( (double) );
#endif

#ifdef HAVE_IEEE_FP
#include <ieeefp.h>
#endif

extern int is_ieee_nan PARAMS( (double) );
extern int is_ieee_inf PARAMS( (double) );

#  define M_PI      3.14159265358979323846
#  define M_PI_2    1.57079632679489661923
#  define M_E       2.7182818284590452354
#  define M_PI_4    0.78539816339744830962
#  define M_1_PI    0.31830988618379067154
#  define M_2_PI    0.63661977236758134308
#  define M_2_SQRTPI    1.12837916709551257390
#  define M_LOG2E   1.4426950408889634074
#  define M_LOG10E  0.43429448190325182765
#  define M_LN2     0.69314718055994530942
#  define M_LN10    2.30258509299404568402
#  define M_SQRT2   1.41421356237309504880
#  define M_SQRT1_2 0.70710678118654752440

/*---- IN arith.c : -------------------------*
double sym_f_cnst[] = {
	M_PI,			* pi         *
	M_PI_2,    		* pi/2       * 
	M_E,       		* e          *
	M_PI_4, 		* pi/4       * 
	M_1_PI,  		* 1/pi       *
	M_2_PI,  		* 2/pi       *
	M_2_SQRTPI,		* 2/sqrt(pi) *
	M_LOG2E, 		* log2(e)    *
	M_LOG10E,		* log10(e)   *
	M_LN2,  		* ln(2)      *
	M_LN10,			* ln(10)     *
	M_SQRT2,        * sqrt(2)    *
	M_SQRT1_2       * sqrt(1/2)  *
};
 *--------------------------------------------*/

#define FPCCASE(the_val) \
	case TK_PI		:	the_val = sym_f_cnst[0]; \
						break;  \
	case TK_PI_2	:	the_val = sym_f_cnst[1]; \
						break; \
	case TK_E		:	the_val = sym_f_cnst[2]; \
						break; \
	case TK_PI_4	:	the_val = sym_f_cnst[3]; \
						break;  \
	case TK_1_PI	:	the_val = sym_f_cnst[4]; \
						break;  \
	case TK_2_PI	:	the_val = sym_f_cnst[5]; \
						break;  \
	case TK_2_SQRTPI :	the_val = sym_f_cnst[6]; \
						break;  \
	case TK_LOG2E	:	the_val = sym_f_cnst[7]; \
						break;  \
	case TK_LOG10E	:	the_val = sym_f_cnst[8]; \
						break;  \
	case TK_LN2		:	the_val = sym_f_cnst[9]; \
						break;  \
	case TK_LN10	:	the_val = sym_f_cnst[10]; \
						break;  \
	case TK_SQRT2	:	the_val = sym_f_cnst[11]; \
						break;  \
	case TK_SQRT1_2	:	the_val = sym_f_cnst[12]; \
						break; 


