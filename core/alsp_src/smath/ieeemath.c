/*
****************************************************************
  File:     "ieeemath.c" --
  Author:   Tim Hickey
  Date:     15 January 1997

   This file contains the machine dependent variable and procedure
   definitions for the sound math interval arithmetic package.

  History:
  1/16/97-  wrote initial version
  1/24/97-  put in BIG/LITTLE-ENDIAN ifdefs
            also put in support for non-fpsetround systems
  1/28/97-  added PNINF interval constant 
  1/29/97-  renamed NAN to be NAN_REAL to avoid conflicts with MacOS
****************************************************************
*/

#include "smath.h"
#include "ieeemath.h"

double  tmpIE,tmpxIE,tmpyIE; /* these are used in the min/max macros,
                             which therefore can't be nested */


  /* these defs may vary from machine to machine */
#ifdef LITTLE_ENDIAN
NEWDOUBLE
    NAN_REAL      = {{0xffff, 0xffff, 0xffff, 0x7fff}},
    POS_INF       = {{0x0000, 0x0000, 0x0000, 0x7ff0}},
    MAX_REAL      = {{0xffff, 0xffff, 0xffff, 0x7fef}},
    SMALL_REAL    = {{0x0001, 0x0000, 0x0000, 0x0000}},
    POS_ZERO      = {{0x0000, 0x0000, 0x0000, 0x0000}},
    NEG_ZERO      = {{0x0000, 0x0000, 0x0000, 0x8000}},
    NEG_INF       = {{0x0000, 0x0000, 0x0000, 0xfff0}};

NEWINTERVAL
  PNINF          =
    {{0x0000, 0x0000, 0x0000, 0xfff0,
      0x0000, 0x0000, 0x0000, 0x7ff0}};
#else
NEWDOUBLE
    NAN_REAL      = {{0x7fff, 0xffff, 0xffff, 0xffff}},
    POS_INF       = {{0x7ff0, 0x0000, 0x0000, 0x0000}},
    MAX_REAL      = {{0x7fef, 0xffff, 0xffff, 0xffff}},
    SMALL_REAL    = {{0x0000, 0x0000, 0x0000, 0x0001}},
    POS_ZERO      = {{0x0000, 0x0000, 0x0000, 0x0000}},
    NEG_ZERO      = {{0x8000, 0x0000, 0x0000, 0x0000}},
    NEG_INF       = {{0xfff0, 0x0000, 0x0000, 0x0000}};

NEWINTERVAL
  PNINF          =
    {{0xfff0, 0x0000, 0x0000, 0x0000,
      0x7ff0, 0x0000, 0x0000, 0x0000}};

#endif
/* 
  This returns an interval containing 2^n for an integer n.
  This works for all integers and returns point-like intervals
  if n in [-1074,1022] otherwise it returns infinite intervals
          [-inf,-maxreal] for n <= -1075
       or [ maxreal, inf] for n >=  1023
*/
INTERVAL two_to_nDI(int n) {
  INTERVAL a;
  UDOUBLE m;

  if (n > 1023) {
    a.lo=MAX_REAL.d; a.hi=POS_INF.d;}
  else if (n > -1023) {
    m.u[HIGH_WORD] = ((n+1023) << 20);
    m.u[LOW_WORD] = 0;
    a.lo=a.hi = m.d;
  }
  else if (n >= -1042) {
    m.u[HIGH_WORD] = (1 << (n + 1042));
    m.u[LOW_WORD]=0;
    a.lo=a.hi=m.d;
  }
  else if (n >= -1074) {
    m.u[HIGH_WORD] = 0;
    m.u[LOW_WORD] = (1 << (n + 1074));
    a.lo = a.hi = m.d;
  }
  else {
    m.u[HIGH_WORD] = 0;
    m.u[LOW_WORD] = 1;
    a.lo = 0;
    a.hi = m.d;
  }

  return(a);
}



#ifdef NOFPSETROUND
/* 
  these two routines are used 
  when fpsetround is not available
*/

double next_fp(double x) {
  UDOUBLE m;
  if (x >= 0) {
    if (x==POS_INF.d)
      return(POS_INF.d);
    else if (x == 0)
      return(SMALL_REAL.d);
    else {
      m.d = x;
      m.u[LOW_WORD] += 1;
      if (m.u[LOW_WORD] == 0)
        m.u[HIGH_WORD]+=1;
      return(m.d);
    }
  }
  else {
    m.d = x;
    if (m.u[LOW_WORD]==0) {
      m.u[HIGH_WORD] -= 1;
      m.u[LOW_WORD] -= 1;
      return(m.d);
    }
    else {
      m.u[LOW_WORD] -= 1;
      return(m.d);
    }
  }
}

double prev_fp(double x) {
  return(-next_fp(-x));
}

double add_lo_fp(double x, double y) {
  if ((x==0) || (y==0)) return(x+y);
  else return(prev_fp(x+y));
}
double add_hi_fp(double x, double y) {
  if ((x==0) || (y==0)) return(x+y);
  else return(next_fp(x+y));
}

double sub_lo_fp(double x, double y) {
  if ((x==0) || (y==0)) return(x-y);
  else return(prev_fp(x-y));
}
double sub_hi_fp(double x, double y) {
  if ((x==0) || (y==0)) return(x-y);
  else return(next_fp(x-y));
}

double mul_lo_fp(double x, double y) {
  if ((x==0) || (y==0)) return(0);
  else if ((x==1) || (y==1)) return(x*y);
  else return(prev_fp(x*y));
}
double mul_hi_fp(double x, double y) {
  if ((x==0) || (y==0)) return(0);
  else if ((x==1) || (y==1)) return(x*y);
  else return(next_fp(x*y));
}

double div_lo_fp(double x, double y) {
  if (y==0) 
    if (x==0) return(NAN_REAL.d);
       else return(prev_fp(x/y));
  else if (x==0) return(0);
  else if (y==1) return(x);
  else return(prev_fp(x/y));
}
double div_hi_fp(double x, double y) {
  if (y==0) 
    if (x==0) return(NAN_REAL.d);
       else return(next_fp(x/y));
  else if (x==0) return(0);
  else if (y==1) return(x);
  else return(next_fp(x/y));
}

double sqrt_lo_fp(double x) {
  if (x<0) return(NAN_REAL.d);
  else if ((x==0) || (x==1)) return(x);
  else  return(prev_fp(sqrt(x)));
}
double sqrt_hi_fp(double x) {
  if (x<0) return(NAN_REAL.d);
  else if ((x==0) || (x==1)) return(x);
  else return(next_fp(sqrt(x)));
}

#endif
