    /*
    ****************************************************************
    File:     "IAtrig.c" -- 
    Author:   Tim Hickey
    Date:     15 January 1997

    Definitions of basic interval arithmetic trig procedures:
          sin,cos,tan,asin,acos,atan
    and their integer period versions
          sin2pi, cos2pi, tan2pi, asin2pi, acos2pi, atan2pi



    History:
    1/16/97 -- implemented simple version using Taylor series
    1/28/97 -- reimplemented acos, atan in terms of asin.
               included defs for the soundly rounded functions
               sin_lo, sin_hi, ... ,atan2pi_lo, atan2pi_hi etc
               These are currently implemented using the trigDI
               functions, but could be more efficiently implemented
               directly. Also, made trig constants extern, and
               deleted defs of trigII and intersect_trig fns,
               as these are now defined in trig.c

    2/7/97  -- fixed infinite recursion loop for atan.
    ****************************************************************
    */

#include "smath.h"
#include "ieeemath.h"

extern const NEWDOUBLE 
  PI_OVER_2_A;

extern const NEWINTERVAL
  PNINF,
  TWOPI, PIOVER2, PIOVER2B;


    /*
    ****************************************************************
    soundly rounded trigonometric functions 
    ****************************************************************
    */

double sin2pi0_hi(double x) {
  INTERVAL Y;
  Y = sin2pi0DI(x);
  return(Y.hi);
}

double sin2pi0_lo(double x) {
  INTERVAL Y;
  Y = sin2pi0DI(x);
  return(Y.lo);
}

double cos2pi0_hi(double x) {
  INTERVAL Y;
  Y = cos2pi0DI(x);
  return(Y.hi);
}

double cos2pi0_lo(double x) {
  INTERVAL Y;
  Y = cos2pi0DI(x);
  return(Y.lo);
}

double tan2pi0_hi(double x) {
  INTERVAL Y;
  Y = tan2pi0DI(x);
  return(Y.hi);
}

double tan2pi0_lo(double x) {
  INTERVAL Y;
  Y = tan2pi0DI(x);
  return(Y.lo);
}



double asin2pi_hi(double x) {
  INTERVAL Y;
  Y = asin2piDI(x);
  return(Y.hi);
}

double asin2pi_lo(double x) {
  INTERVAL Y;
  Y = asin2piDI(x);
  return(Y.lo);
}

double acos2pi_hi(double x) {
  INTERVAL Y;
  Y = acos2piDI(x);
  return(Y.hi);
}

double acos2pi_lo(double x) {
  INTERVAL Y;
  Y = acos2piDI(x);
  return(Y.lo);
}

double atan2pi_hi(double x) {
  INTERVAL Y;
  Y = atan2piDI(x);
  return(Y.hi);
}

double atan2pi_lo(double x) {
  INTERVAL Y;
  Y = atan2piDI(x);
  return(Y.lo);
}



    /* 
    ****************************************************************
    sound point evaluation of the trigonometric functions 
    ****************************************************************
    */

INTERVAL sin2pi0DI(double x){ /* 0 <= x <= 1/4 */
  INTERVAL T;
  T = mulDII(x,TWOPI.i);
  /* this is wrong if T.lo and T.hi are on either side of a max or min of sin */
  return(unionIII(sin0DI(T.lo),sin0DI(T.hi))); 
}

INTERVAL cos2pi0DI(double x){ /* 0 <= x <= 1/4 */
  INTERVAL T;
  T = mulDII(x,TWOPI.i);
  return(unionIII(cos0DI(T.lo),cos0DI(T.hi)));
}

INTERVAL tan2pi0DI(double x){ /* 0 <= x <= 1/4 */
  INTERVAL T;
  T = mulDII(x,TWOPI.i);
  return(unionIII(tan0DI(T.lo),tan0DI(T.hi)));
}

INTERVAL asin2piDI(double x){ /* returns y in [-1/4,1/4] */
  return(divIII(asinDI(x),TWOPI.i));
}

INTERVAL acos2piDI(double x){ /* returns y in [   0 1/2] */
  return(divIII(acosDI(x),TWOPI.i));
}

INTERVAL atan2piDI(double x){ /* returns y in [-1/4,1/4] */
  return(divIII(atanDI(x),TWOPI.i));
}




    /*
    ****************************************************************
    soundly rounded trigonometric functions 
    ****************************************************************
    */
double sin0_hi(double x) {
  INTERVAL Y;
  Y = sin0DI(x);
  return(Y.hi);
}

double sin0_lo(double x) {
  INTERVAL Y;
  Y = sin0DI(x);
  return(Y.lo);
}

double cos0_hi(double x) {
  INTERVAL Y;
  Y = cos0DI(x);
  return(Y.hi);
}

double cos0_lo(double x) {
  INTERVAL Y;
  Y = cos0DI(x);
  return(Y.lo);
}

double tan0_hi(double x) {
  INTERVAL Y;
  Y = tan0DI(x);
  return(Y.hi);
}

double tan0_lo(double x) {
  INTERVAL Y;
  Y = tan0DI(x);
  return(Y.lo);
}



double asin_hi(double x) {
  INTERVAL Y;
  Y = asinDI(x);
  return(Y.hi);
}

double asin_lo(double x) {
  INTERVAL Y;
  Y = asinDI(x);
  return(Y.lo);
}

double acos_hi(double x) {
  INTERVAL Y;
  Y = acosDI(x);
  return(Y.hi);
}

double acos_lo(double x) {
  INTERVAL Y;
  Y = acosDI(x);
  return(Y.lo);
}

double atan_hi(double x) {
  INTERVAL Y;
  Y = atanDI(x);
  return(Y.hi);
}

double atan_lo(double x) {
  INTERVAL Y;
  Y = atanDI(x);
  return(Y.lo);
}



    /* 
    ****************************************************************
    sound point evaluation of the trigonometric functions 
    ****************************************************************
    */



INTERVAL sin0DI(double x) {  /* -pi/2 <= x <= pi/2, best if in [-pi/4,pi/4] */ 
  /* Taylor series with deg terms */
 int j,deg4 = 5;
 INTERVAL R,X2;

 R = makeDDI(-1,1);
 X2 = squareDI(x);

    for (j=4*deg4;j>=8;j -= 4) {       /* y = sin(a), R = (1+a^2/((j-1)*(j-2))*(-1 + a^2/(j*(j+1))*R)) */
       R= addDII(-1.0,divIDI(mulIII(X2,R),(j+1)* j));
       R= addDII( 1.0,divIDI(mulIII(X2,R),(j-1)*(j-2)));
     }
    R = addDII(-1.0,divIDI(mulIII(X2,R),20.0));
    R =addDII(x,divIDI(mulDII(x,mulIII(X2,R)),6.0));

    return(R);
}

INTERVAL cos0DI(double x){  /* -pi/2 <= x <= pi/2, best if in [-pi/4,pi/4] */ 
  /* Taylor series with deg terms */
  /* Taylor series with deg terms */
 int j,deg4 = 5;
 INTERVAL R,X2;

 X2 = squareDI(x);
 R = makeDDI(-1,1);

    for (j=4*deg4;j>=8;j -= 4) {   /* y = cos(a), R = (1+a^2/((j-3)*(j-2))*(-1 + a^2/((j-1)*j)*R)) */
       R= addDII(-1.0,divIDI(mulIII(X2,R),(j-1)* j));
       R= addDII(+1.0,divIDI(mulIII(X2,R),(j-3)*(j-2)));
     }

    R = addDII(1.0,
        addIII(negII(divIDI(X2,2.0)),
               divIDI(mulIII(mulIII(X2,X2),R),24.0)));

    return(R);


}

INTERVAL tan0DI(double x){  /* -pi/2 <= x <= pi/2 */ 
  return(divIII(sin0DI(x),cos0DI(x)));
}



    /*
    ****************************************************************
     Inverse trigonometric functions.
     Everything is computed in terms of arcsin, which we compute
     using a taylor series for asin(x)*sqrt(1-x^2).
    ****************************************************************
    */

INTERVAL asinDI(double x){ 
/*
   x is in the range [-1,1]
   returns y in [-pi/2,pi/2] 
   We use the following reductions:

    asin(x) = -asin(-x)                            ,if x < 0

    asin(x) = pi/2 - 2*asin(sqrt((1-x)/2))         ,if x > 0.5 

    asin(x) = T(x)/ sqrt(1-x^2)                    ,if x in [0,0.5]

    T(x) =  x + 2/3*x^3 + (2*4)/(3*5)*x^5 + ... + R*x^(2n+1), 

    |R|<1/(1-x^2)

    Currently we use a 30-term Taylor series.
    Another possibility is to use a rational approximation 
    with provable error bounds.
*/


  INTERVAL y;

  if (x < 0)
    return(negII(asinDI(-x)));
  else if (x > 0.5) {
        /* 0.5 < x <= 1.0
           use arcsin(x) = [-pi/2,pi/2] intersect 
                  (pi/2 - 2*asin(sqrt((1-x)/2)))
           Note: in this case, (1-x)/2.0 will be computed exactly in IEEE 754
           Also, we must intersect with [0.0,0.5] to avoid a potential infinite loop.
        */
    INTERVAL A;
    A = intersectIII(sqrtII(divIDI(subDDI(1.0,x),2.0)),makeDDI(0.0,0.5));
    y = subIII(PIOVER2.i,mulDII(2.0,asinII(A)));
    return(y);
  }
  else { /*  0 <= x <= 0.5, so 
             compute (arcsin(x) * sqrt(1-x^2) via Taylor series
             x + 2/3*x^3 + (2*4)/(3*5)*x^5 + ... + R*x^(2n+1), 
             |R|<1/(1-x^2)
         */ 
    INTERVAL R,X2;
    int j,deg=60;

    X2 = squareDI(x);
    R = divDII(1.0,subDII(1.0,squareDI(x)));

    for (j=deg;j>=4;j-=2)
      R = addDII(1.0,mulIII(divDDI(j,j+1),mulIII(X2,R)));

    R = addDII(x,mulDII(x,mulIII(X2,mulIII(divDDI(2,3),R))));

    R = mulIII(R,sqrtII(subDII(1.0,squareDI(x))));

    return(intersectIII(R,unionIII(negII(PIOVER2.i),PIOVER2.i)));

  }
}



  /* 
  ****************************************************************
  ****************************************************************

          INTERVAL acosDI(double x)

  This uses the following reduction formulae:

  acos(x) = pi   - 2*asin(sqrt((1+x)/2)) ,if x in [-1.0,  -0.5] 
  acos(x) = pi/2 + asin(-x)              ,if x in [-0.5,   0.0]
  acos(x) = pi/2 - asin(x)               ,if x in [ 0.0,   0.5]
  acos(x) =        2*asin(sqrt((1-x)/2)) ,if x in [ 0.5,   1.0]

  ****************************************************************
  ****************************************************************
  */

INTERVAL acosDI(double x) {
  INTERVAL T,S,Y;


  if (x < -1.0) {
    return(PNINF.i);
  }

  else if (x < -0.500) {
  /*
  acos(x) = pi   - 2*asin(sqrt((1+x)/2)) ,if x in [-1.0,  -0.5] 
          = 2.0* (pi/2 - asin(sqrt((1+x)/2)));
  */
       T= sqrtII(mulIDI(addDDI(1.0,x),0.5));
       Y = mulDII(2.0,
             subDII(PI_OVER_2_A.d,
             subIII(asinII(T),
                    PIOVER2B.i)));
     }

  else if (x < 0)
  /*
    acos(x) = pi/2 + asin(-x)              ,if x in [-0.5, 0.0]
  */
       Y = addDII(PI_OVER_2_A.d,
           addIII(asinDI(-x),PIOVER2B.i));

  else if (x <= 0.500)
  /*
    acos(x) = pi/2 - asin(x)               ,if x in [ 0.0,   0.5]
  */
       Y =  
           subDII(PI_OVER_2_A.d,
           subIII(asinDI(x),
                  PIOVER2B.i));
  else if (x <= 1.0) {
  /*
    acos(x) =        2*asin(sqrt((1-x)/2)) ,if x in [ 0.5, 1.0]
  */

    S = sqrtII(divIDI(subDDI(1.0,x),2.0));
    Y = mulDII(2.0,asinII(S));
  }
  else {/* error case */
    return(PNINF.i);
  }

  Y = intersectIII(Y,makeDDI(0,2*PIOVER2.i.hi));

  return(Y);
}




    /*
    ****************************************************************
    atanDI(x)
    Here we reduce to asinII using
        atan(-x) = -atan(x)
        atan(x) = asin(x/sqrt(1+x^2))   (if 0 <= x < 1)
        atan(x) = pi/2 - asin(1/sqrt(1+x^2))   (if 1 <= x)
    ****************************************************************
    */
INTERVAL atanDI(double x) {
  INTERVAL X;

  if (x < 0)
    return(negII(atanDI(-x)));
  else if (x < 1) {
    X = divDII(x,sqrtII(addDII(1,mulDDI(x,x))));
    return(asinII(X));
  }
  else {
    X = divDII(1,sqrtII(addDII(1,mulDDI(x,x))));
    return(subIII(PIOVER2.i,asinII(X)));
  }
}





