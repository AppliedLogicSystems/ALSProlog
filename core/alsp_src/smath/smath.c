    /*
    ****************************************************************
    File:     "smath.c" -- 
    Author:   Tim Hickey
    Date:     15 January 1997

    Definitions of basic arithmetic narrowing primitives 
         core arithmetic (+,*,^2)
         comparisons (=,<,<=,<>)
         type constraints (integer(X), boolean(X))
         misc. (abs,sgn, max,min,flr,ceil)

    Problems:
         this system is not yet designed to handle integer/boolean
         type information, hence narrow_lt and narrow_le are 
         identical.

    History:
    1/16/97 -- complete 1st draft, based on system by Tim Hickey/Qun Ju
    1/24/97 -- modified headers

    ****************************************************************
    */

#include "smath.h"
#include "ieeemath.h"

extern const NEWDOUBLE MAX_REAL, POS_INF, NEG_INF, POS_ZERO, NEG_ZERO;


    /* 
    ****************************************************************
    The Narrowing Functions for Basic Arithmetic Operations
    These return 1 for success and 0 for failure.
    ****************************************************************
    */

/* arithmetic */
int narrow_add(INTERVAL *x, INTERVAL *y, INTERVAL *z) {
  double  t;

  if (LT((t=add_hi(x->hi,y->hi)),z->hi)) 
       {z->hi = t; if (LT(t,z->lo)) return(0);}
  else {x->hi = min(x->hi, sub_hi(z->hi,y->lo));
        if (LT(x->hi,x->lo)) return(0);
        y->hi = min(y->hi, sub_hi(z->hi,x->lo));
        if (LT(y->hi,y->lo)) return(0); }

  if (GT((t=add_lo(x->lo,y->lo)), z->lo)) 
        {z->lo = t; if (GT(t,z->hi)) return(0);}
  else {x->lo = max(x->lo, sub_lo(z->lo,y->hi));
        if (GT(x->lo,x->hi)) return(0);
        y->lo = max(y->lo, sub_lo(z->lo,x->hi));
        if (GT(y->lo,y->hi)) return(0);}

  return(1);
}

int narrow_u_minus(INTERVAL *x, INTERVAL *y){          /*   -x  = y */
  if (LT(x->lo,-(y->hi))) 
     x->lo = -(y->hi);
  else
     y->hi = -(x->lo);

  if (GT(x->hi,-(y->lo)))
    x->hi = - (y->lo);
  else
    y->lo = -(x->hi);

  return(x->lo <= x->hi);
}

int narrow_mul(INTERVAL *x,INTERVAL *y, INTERVAL *z)
{ 
    return(
        intersect_mulIII(*x,*y,z) &&
        intersect_divIII(*z,*y,x) &&
        intersect_divIII(*z,*x,y)
    );
}
      

int narrow_square(INTERVAL *x, INTERVAL *y) {
  INTERVAL t;
  *y = intersectIII(*y,squareII(*x));
  t = sqrtII(*y);
  *x = unionIII(intersectIII(*x,t),intersectIII(*x,negII(t)));
  return(nonemptyI(*x)&&nonemptyI(*y));
}

/* comparison constraints */
int narrow_eq(INTERVAL *x,INTERVAL *y){
  if (x->hi < y->hi)
     y->hi = x->hi;
  else
     x->hi = y->hi;

  if (x->lo > y->lo)
     y->lo = x->lo;
  else
     x->lo = y->lo;

  return(nonemptyI(*x));
}

int narrow_lt(INTERVAL *x,INTERVAL *y){
  if (x->hi > y->hi)
    x->hi = y->hi;
  if (y->lo < x->lo)
    y->lo = x->lo;
  return(nonemptyI(*x)&&nonemptyI(*y)&&narrow_ne(x,y));  /* probably just one would do, check it */
}

int narrow_le(INTERVAL *x,INTERVAL *y){
  if (x->hi > y->hi)
    x->hi = y->hi;
  if (y->lo < x->lo)
    y->lo = x->lo;
  return(nonemptyI(*x)&&nonemptyI(*y));  /* probably just one would do, check it */
}

int narrow_ne(INTERVAL *x,INTERVAL *y){
  /* this fails only if (x == y == constant */
  return((x->lo<x->hi) || (y->lo < y->hi) || (x->lo != y->lo));
}

/* miscellaneous real functions */
int narrow_abs(INTERVAL *x,INTERVAL *y){  /* y = |x| */
  *y = intersectIII(*y,absII(*x));
  *x = intersectIII(*x,
         unionIII(intersectIII(*x,*y),intersectIII(*x,negII(*y))));
  return(nonemptyI(*x)&&nonemptyI(*y));
}

int narrow_sgn(INTERVAL *x,INTERVAL *y){               /* y = sgn(x) = x/|x|  (or 0 if x = 0) */
  *y = intersectIII(*y,sgnII(*x));

  if ((y->lo == 1) && (y->hi == 1)) {
    *x = intersectIII(*x,makeDDI(0,POSINF));
    return(nonemptyI(*y) && nonemptyI(*x) && !(is_zeroI(*x)));
  }
  else if ((y->lo == 0) && (y->hi == 1)) {
    *x = intersectIII(*x,makeDDI(0,POSINF));
    return(nonemptyI(*x));
  }
  else if ((y->lo == 0) && (y->hi == 0)) {
    *x = intersectIII(*x,cnstDI(0));
    return(nonemptyI(*x));
  }   
  else if ((y->lo == -1) && (y->hi == -1)) {
    *x = intersectIII(*x,makeDDI(NEGINF,0));
    return(nonemptyI(*x) && !(is_zeroI(*x)));
  }
  else if ((y->lo == -1) && (y->hi == 0)) {
    *x = intersectIII(*x,makeDDI(NEGINF,0));
    return(nonemptyI(*x));
  }

  return(nonemptyI(*x) && nonemptyI(*y));
}

int narrow_min(INTERVAL *x,INTERVAL *y, INTERVAL *z){
  *z = intersectIII(*z,minIII(*x,*y));

  if (x->lo > z->lo) x->lo = z->lo;
  if (y->lo > z->lo) y->lo = z->lo;

  return(nonemptyI(*x)&&nonemptyI(*y)&&nonemptyI(*z));
}

int narrow_max(INTERVAL *x,INTERVAL *y, INTERVAL *z){
  *z = intersectIII(*z,maxIII(*x,*y));

  if (x->hi < z->hi) x->hi = z->hi;
  if (y->hi < z->hi) y->hi = z->hi;

  return(nonemptyI(*x)&&nonemptyI(*y)&&nonemptyI(*z));
}

int narrow_flr(INTERVAL *x,INTERVAL *y){               /* y = largest integer less than or equal to x */
  /* we are currently ignoring the "integer" property here */
  return(narrow_le(x,y));
}

int narrow_ceil(INTERVAL *x,INTERVAL *y){              /* y = smallest integer greater than or equal to x */
  /* we are currently ignoring the "integer" property here */
  return(narrow_le(y,x));
}








