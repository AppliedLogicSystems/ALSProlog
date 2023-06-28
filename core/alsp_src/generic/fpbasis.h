/*===========================================================================*
 |              fpbasis.h
 |      Copyright (c) 1996-97 Applied Logic Systems, Inc.
 |
 |      -- Floating point math abstractions
 |
 *===========================================================================*/


#ifdef HAVE_IEEE_FP
#include <ieeefp.h>
#endif

extern int is_ieee_nan (double);
extern int is_ieee_inf (double);

#  define D_PI      3.14159265358979323846
#  define D_PI_2    1.57079632679489661923
#  define D_E       2.7182818284590452354
#  define D_PI_4    0.78539816339744830962
#  define D_1_PI    0.31830988618379067154
#  define D_2_PI    0.63661977236758134308
#  define D_2_SQRTPI    1.12837916709551257390
#  define D_LOG2E   1.4426950408889634074
#  define D_LOG10E  0.43429448190325182765
#  define D_LN2     0.69314718055994530942
#  define D_LN10    2.30258509299404568402
#  define D_SQRT2   1.41421356237309504880
#  define D_SQRT1_2 0.70710678118654752440

/*---- IN arith.c : -------------------------*
double sym_f_cnst[] = {
	D_PI,			* pi         *
	D_PI_2,    		* pi/2       * 
	D_E,       		* e          *
	D_PI_4, 		* pi/4       * 
	D_1_PI,  		* 1/pi       *
	D_2_PI,  		* 2/pi       *
	D_2_SQRTPI,		* 2/sqrt(pi) *
	D_LOG2E, 		* log2(e)    *
	D_LOG10E,		* log10(e)   *
	D_LN2,  		* ln(2)      *
	D_LN10,			* ln(10)     *
	D_SQRT2,        * sqrt(2)    *
	D_SQRT1_2       * sqrt(1/2)  *
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


