    /*
    ****************************************************************
    File:     "IAexp.c" -- 
    Author:   Tim Hickey
    Date:     15 January 1997

    Definitions of basic interval exponential procedures:
         exp, log
    and the two symmetric/antisymmetric extensions of the
    power function to negative values
         power_even, power_odd


    History:
    1/16/97 -- implemented version based on one by Tim Hickey/Qun Ju
    1/24/97 -- * replaced get_bits_exp with call to frexp from math.h
               * adjusted headers, 
               * made portability changes,
               * replaced interval arithmetic with exact arithmetic
                 where possible
    1/28/97 -- added defs for exp_hi,exp_lo,log_hi,log_lo in terms of
               expDI and logDI
    2/28/97 -- fixed precision bug in expDI for large x
    ****************************************************************
    */

#include "smath.h"
#include "ieeemath.h"

#include "ieee_exp.c"  /* this loads in several tables of doubles/intervals */

extern NEWDOUBLE MAX_REAL, POS_INF, NEG_INF, POS_ZERO, NEG_ZERO;

double exp_hi(double x) {
  INTERVAL Y;
  Y = expDI(x);
  return(Y.hi);
}

double exp_lo(double x) {
  INTERVAL Y;
  Y = expDI(x);
  return(Y.lo);
}

INTERVAL expDI(double x) {
  double n;   INTERVAL A,R;   int i,deg=15;
  INTERVAL T1,T3,T4;
  double t2;

    /* RANGE REDUCTION to [ln(minreal),ln(maxreal)] */
  if      (x > LOGMAXREAL.d)  return(NEAR_POS_INF.i);
  else if (x < LOGMINREAL.d)  return(NEAR_POS_ZERO.i);
    /* RANGE REDUCTION to A in [-ln(2)/2, ln(2)/2] */
    /* x \in A + n*ln(2),  A = (x-n*ln2a) - n*ln2B */
  n = floor(x/LN2_LO.d+0.5); 

  /* note |n|<1000, so n*LN2_A.d is exact in any rounding mode */
  A = subIII(T1=subDDI(x,t2=n*LN2_A.d),T3=mulDII(n,LN2B.i));

    /* TAYLOR SERIES EVALUATION of exp(A) */
  R = makeDDI(0.5,2); /* R = [1,2] */
  for(i=deg;i>=3;i--) 
    R = addDII(1,divIDI(mulIII(A,R),i));  /* R = 1+A*R/i */
    /* R = 1 + (A + A*(A*R)/2) */
  R = addDII(1.0,addIII(A, divIDI(mulIII(A,mulIII(A,R)),2.0)));

    /* APPLY ADDITION LAW : exp(x) = exp(a+n*ln(2)) = R*2^n */
  if ((n==1024) && (R.hi < 1.0)) {
    R = mulIDI(R,2.0); n--;
  }
  R = mulIII(R,two_to_nDI(n)); 
  return(R);
}



INTERVAL expII(INTERVAL X) {
  INTERVAL Y,T;
  T = expDI(X.lo);
  Y.lo = T.lo;
  T = expDI(X.hi);
  Y.hi = T.hi;
  return(Y);
}




double log_hi(double x) {
  INTERVAL Y;
  Y = logDI(x);
  return(Y.hi);
}

double log_lo(double x) {
  INTERVAL Y;
  Y = logDI(x);
  return(Y.lo);
}



INTERVAL logDI(double x) {
  double a,a1;
  int n,j,i,deg=40;
  INTERVAL A1,R;
  INTERVAL T1,T3,T4;
  double t2;
    /* Range reduction to [1, 1+1/8] */
    /* x = a*2^n,  1<=a < 2 */
    /* a in (1+i/8)*(1 + A1) */

  if (x==POS_INF.d) return(NEAR_POS_INF.i);
  if (x==0.0) return(NEAR_NEG_INF.i);

  /* find a,n st x = a*2^n with a in [1,2) */
  a = frexp(x,&n);  
  if (a != 1.0) {a=a*2.0; n--;}

  i = floor(a*8)-8;
  if (i <= 1) i = 0;
  a1 = i/8.0;
  A1 = divIDI(subDDI(a,(1+a1)),(1+a1));

    /* Taylor evaluation of log(1+A1) */
  R = makeDDI(0,1);
  for (j=deg;j>=2;j--) 
    R = subIII(divDDI(1.0,j),mulIII(A1,R));
  R = subIII(A1,mulIII(A1,mulIII(A1,R)));

    /* Use addition law: 
       ln((1+a1)*(1+A1)*2^n) = ln(1+a1)+ln(1+A1)+n*ln(2) */
  R = addIII((T1=mulDDI(n,LN2_A.d)),
      addDII((t2=log_tab[i].a.d),
      addIII(R,
      addIII((T3=mulDII(n,LN2B.i)),
             (T4=log_tab[i].b.i)))));

  return(R);  
}




INTERVAL logII(INTERVAL X) {
  INTERVAL T,Y;
  if (X.hi < 0) return(FAIL_INT.i);
  if (X.lo < 0) X.lo = 0;
  T = logDI(X.lo);
  Y.lo = T.lo;
  T = logDI(X.hi);
  Y.hi = T.hi;
  return(Y);
}  





INTERVAL power_oddDDI(double x,double y){
  /* this is a quick fix to get power_oddDDI implemented */
  INTERVAL X,Y,Z;
  Z.lo = NEGINF;
  Z.hi = POSINF;
  X.lo = X.hi = x;
  Y.lo = Y.hi = y;
  narrow_pow_odd(&X,&Y,&Z);
  return(Z);
}

INTERVAL power_evenDDI(double x,double y) {
  /* this is a quick fix to get power_evenDDI implemented */
  INTERVAL X,Y,Z;
  Z.lo = NEGINF;
  Z.hi = POSINF;
  X.lo = X.hi = x;
  Y.lo = Y.hi = y;
  narrow_pow_even(&X,&Y,&Z);
  return(Z);
}

INTERVAL power_oddIII(INTERVAL X,INTERVAL Y){
  /* this is a quick fix to get power_oddIII implemented */
  INTERVAL A,B,C;
  C.lo = NEGINF;
  C.hi = POSINF;
  A.lo = X.lo; A.hi = X.hi;
  B.lo = Y.lo; B.hi = Y.hi;
  narrow_pow_odd(&A,&B,&C);
  return(C);
}

INTERVAL power_evenIII(INTERVAL X,INTERVAL Y){
  /* this is a quick fix to get power_evenIII implemented */
  INTERVAL A,B,C;
  C.lo = NEGINF;
  C.hi = POSINF;
  A.lo = X.lo; A.hi = X.hi;
  B.lo = Y.lo; B.hi = Y.hi;
  narrow_pow_even(&A,&B,&C);
  return(C);
}


