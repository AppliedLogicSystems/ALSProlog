/*
****************************************************************
    File:      "ieeemath.h"
    Author:    Tim Hickey
    Date:      15 January 1997

    This contains the machine dependent macros for the
    sound math interval arithmetic package.

    History:
    1/15/97 -- initial version
    1/24/97 -- made Chuck Houpt's changes, and added support
               for non fpsetround systems

****************************************************************
*/


#if defined(macintosh)
#define ALS_BIG_ENDIAN
#elif defined(WIN32)
#define ALS_LITTLE_ENDIAN
#elif defined(UNIX)
    #if defined(UNIX_LINUX)
        #include <endian.h>
        #if __BYTE_ORDER == __LITTLE_ENDIAN
        #define ALS_LITTLE_ENDIAN
        #else
        #define ALS_BIG_ENDIAN
        #endif
    #elif defined(SOLARIS_LINUX)
        #if defined(_BIG_ENDIAN)
        #define ALS_BIG_ENDIAN
        #elif defined(_LITTLE_ENDIAN)
        #define ALS_LITTLE_ENDIAN
        #else
        #error
        #endif
    #elif defined(HPUX_LINUX) || defined(IRIX_LINUX)
    #define ALS_BIG_ENDIAN
    #else
    #error
    #endif
#else
#error
#endif

#if defined (ALS_LITTLE_ENDIAN)
#define HIGH_WORD 1
#define LOW_WORD 0
#elif defined(ALS_BIG_ENDIAN)
#define HIGH_WORD 0
#define LOW_WORD 1
#else
#error
#endif

#ifdef WIN32
#define NOFPSETROUND
#endif

#ifdef macintosh
#include <fp.h>
#include <fenv.h>

#elif defined(__hp9000s800)
#include <math.h>

#elif WIN32
#include <math.h>

#else
#include <stddef.h>
#include <math.h>
#if HAVE_IEEEFP
#include <ieeefp.h>
#endif
#endif

#define FAIL 0
#ifndef macintosh
#define FALSE 0
#define TRUE 1
#endif

#define EQ(x,y) ((x)==(y))
#define NE(x,y) ((x)!=(y))
#define LT(x,y) ((x)<(y))
#define GT(x,y) ((x)>(y))


    /*
    ****************************************************************
    MACROS used in the sound math library
    ****************************************************************
    */

#ifdef macintosh
#define fpsetround fesetround
#define FP_RN	FE_TONEAREST
#define FP_RM	FE_DOWNWARD
#define FP_RP	FE_UPWARD
#endif
#ifdef WIN32
#define fpsetround(x)	0
#define FP_RN
#define FP_RM
#define FP_RP
#endif

#ifdef NOFPSETROUND
double add_hi_fp(double x, double y);
double add_lo_fp(double x, double y);
double sub_lo_fp(double x, double y);
double sub_hi_fp(double x, double y);
double mul_lo_fp(double x, double y);
double mul_hi_fp(double x, double y);
double div_lo_fp(double x, double y);
double div_hi_fp(double x, double y);
double sqrt_lo_fp(double x);
double sqrt_hi_fp(double x);

#define add_hi(x,y) add_hi_fp(x,y) 
#define add_lo(x,y) add_lo_fp(x,y)
#define sub_lo(x,y) sub_lo_fp(x,y) 
#define sub_hi(x,y) sub_hi_fp(x,y) 
#define mul_lo(x,y) mul_lo_fp(x,y) 
#define mul_hi(x,y) mul_hi_fp(x,y) 
#define div_lo(x,y) div_lo_fp(x,y) 
#define div_hi(x,y) div_hi_fp(x,y) 
#define sqrt_lo(x) sqrt_lo_fp(x) 
#define sqrt_hi(x) sqrt_hi_fp(x) 

#else
#define add_lo(x,y) (fpsetround(FP_RM),((x)+(y)))
#define add_hi(x,y) (fpsetround(FP_RP),((x)+(y)))
#define sub_lo(x,y) (fpsetround(FP_RM),((x)-(y)))
#define sub_hi(x,y) (fpsetround(FP_RP),((x)-(y)))
#define mul_lo(x,y) (fpsetround(FP_RM),((x)*(y)))
#define mul_hi(x,y) (fpsetround(FP_RP),((x)*(y)))
#define div_lo(x,y) (fpsetround(FP_RM),((x)/(y)))
#define div_hi(x,y) (fpsetround(FP_RP),((x)/(y)))
#define sqrt_lo(x) (fpsetround(FP_RM),(sqrt(x)))
#define sqrt_hi(x) (fpsetround(FP_RP),(sqrt(x)))
#endif


#define NEGINF NEG_INF.d
#define POSINF POS_INF.d
#define NEGZERO NEG_ZERO.d
#define POSZERO POS_ZERO.d

/* here the parameters x,y must be preevaluated constants */
#ifndef WIN32
#define min(x,y) (((x)<(y))?(x):(y))
#define max(x,y) (((x)<(y))?(y):(x))
#endif

#define min2(x,y) (((x)<(y))?(x):(y))
#define max2(x,y) (((x)<(y))?(y):(x))

/* the following min/max defs have the effect of ignoring NAN values */
/* the variables tmpIE,tmpxIE,tmpyIE are defined in ieeemath.c */
/* also the parameters x,y are only evaluated once in this version */

#define minIEEE(x,y) ((tmpIE =(((tmpxIE=x)<(tmpyIE=y))?(tmpxIE):(tmpyIE)))>NEGINF? tmpIE:NEGINF)
#define maxIEEE(x,y) ((tmpIE =(((tmpxIE=x)>(tmpyIE=y))?(tmpxIE):(tmpyIE)))<POSINF? tmpIE:POSINF)



    /*
    ****************************************************************
    MACROs for testing sign of IEEE doubles and 
    intersection of INTERVAL with POS, NEG, etc.
    ****************************************************************
    */

#define IS_ZERO(x) ((x)==0)
#define NON_ZERO(x) ((x)!=0)
#define POS(x) ((x)>0)
#define NEG(x) ((x)<0)
#define NON_POS(x) ((x)<=0)
#define NON_NEG(x) ((x)>=0)

#define posI(x) (POS((x).lo))
#define non_negI(x) (NON_NEG((x).lo))

#define is_zeroI(x) (IS_ZERO((x).lo) && IS_ZERO((x).hi))
#define splitI(x) (NEG((x).lo) && POS((x).hi))
#define contains_zeroI(x) (NON_POS((x).lo) && NON_NEG((x).hi))

#define non_posI(x) (NON_POS((x).hi))
#define negI(x) (NEG((x).hi))

#define nonemptyI(x) (((x).lo <= (x).hi) && ((x).lo < POSINF) && (NEGINF < (x).hi))
#define emptyI(x) (!(nonemptyI(x)))




    /*
    ****************************************************************
    All of the following data structures are used to initialize
    double precision fields in data structures

    Note: on big-endian systems (MIPS) we can initialize infinity by
        NEWDOUBLE POS_INF       = {{0x7ff0, 0x0000, 0x0000, 0x0000}},
    whereas on little-endian systems (Intel) the byte order is reversed:
        NEWDOUBLE POS_INF       = {{0x0000, 0x0000, 0x0000, 0x7ff0}},
    ****************************************************************
    */
typedef union udouble {
  unsigned int u[2];
  double d;
} UDOUBLE;


typedef union newdouble {
  unsigned short int usi[4];
  double d;
} NEWDOUBLE, NEWDOUBLE3[3];

typedef union newinterval {
  unsigned short int v[8];
  INTERVAL i;
} NEWINTERVAL, *NEWINTERVALP;

typedef struct ext_prec {
  NEWDOUBLE   a;
  NEWINTERVAL b;
} EXT_PREC;

extern NEWDOUBLE NAN_REAL, POS_INF;





