    /*
    ****************************************************************
    File:     "exp.c" -- 
    Author:   Tim Hickey
    Date:     15 January 1997

    Definitions of basic nonlogical narrowing primitives 
          exp, pow_odd, pow_even,


    History:
    1/16/97 -- implemented version based on one by Tim Hickey/Qun Ju
               and added initial version of the power_odd, power_even
               narrowing functions.
    1/24/97 -- adjusted headers
    ****************************************************************
    */

#include "smath.h"
#include "ieeemath.h"

extern const NEWDOUBLE MAX_REAL, POS_INF, NEG_INF, POS_ZERO, NEG_ZERO;


/* **************************************************************** */

/* exponential functions */
int narrow_exp(INTERVAL *x, INTERVAL *y) {                       /* y = exp(x) */
  *y = intersectIII(*y,expII(*x));
  *x = intersectIII(*x,logII(*y));
  return(nonemptyI(*x)&&nonemptyI(*y));
}

int narrow_pow_odd(INTERVAL *x, INTERVAL *y, INTERVAL *z){
  /* this currently assumes that y is a constant and hence does no narrowing on y */
        /* z = sgn(x)* abs(x)**y, (or 0 if x = 0) */
        /* x = sgn(z)* abs(z)**(1/y), (or 0 if z = 0) */
  *z = intersectIII(*z,mulIII(sgnII(*x),expII(mulIII(logII(absII(*x)),*y))));
  *x = intersectIII(*x,mulIII(sgnII(*z),expII(mulIII(logII(absII(*z)),divDII(1.0,*y)))));
  return(nonemptyI(*x) && nonemptyI(*y));
}

int narrow_pow_even(INTERVAL *x, INTERVAL *y, INTERVAL *z){  
  /* this currently assumes that y is a constant and hence does no narrowing on y */
        /* z =         abs(x)**y, (or 0 if x = 0) */
        /* abs(x) =         z**(1/y), (or 0 if x = 0) */
  INTERVAL t;
  *z = intersectIII(*z,mulIII(sgnII(*x),expII(mulIII(logII(absII(*x)),*y))));
  t = intersectIII(*x,expII(mulIII(logII(*z),divDII(1.0,*y))));
  return(nonemptyI(*x) && nonemptyI(*z) && narrow_abs(x,&t)); 
}

