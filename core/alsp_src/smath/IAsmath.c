    /*
    ****************************************************************
    File:     "IAsmath.c" -- 
    Author:   Tim Hickey
    Date:     15 January 1997

    Definitions of basic INTERVAL arithmetic procedures:
         set operations (intersect, union)
         core arithmetic (add,sub,neg,mul,div,square,sqrt)
         misc functions (abs,sgn,floor,ceil,max,min)


    History:
    1/16/97 -- implemented version based on one by Tim Hickey/Qun Ju
    1/24/97 -- adjusted headers
    1/28/97 -- added unionDDI
    1/29/97 -- fixed bug in intersectIII (now returns [+inf,-inf] for
               empty intervals which works well with unionIII, and
               fixed bug in intersect_divIII which expanded some intervals!
    ****************************************************************
    */

#include <stdio.h>
#include "smath.h"
#include "ieeemath.h"

extern NEWDOUBLE MAX_REAL, POS_INF, NEG_INF, POS_ZERO, NEG_ZERO;

    /*
    ****************************************************************
    constructors
    ****************************************************************
    */

INTERVAL cnstDI(double x) {
  INTERVAL Z;
  Z.lo = Z.hi = x;
  return(Z);
}


INTERVAL makeDDI(double x,double y) {
  INTERVAL Z;
  Z.lo = x; Z.hi = y;
  return(Z);
}


    /*
    ****************************************************************
    intersection and union of two INTERVALs.
    ****************************************************************
    */

INTERVAL intersectIII(INTERVAL x, INTERVAL y) {
  INTERVAL z;
  extern NEWDOUBLE POS_INF, NEG_INF;

  z.hi = min2(x.hi,y.hi);
  z.lo = max2(x.lo,y.lo);

  if (emptyI(z)) 
    {z.lo = POS_INF.d; z.hi = NEG_INF.d;}
  return(z);
}

INTERVAL unionIII(INTERVAL x, INTERVAL y) {
  INTERVAL z;
  z.hi = max2(x.hi,y.hi);
  z.lo = min2(x.lo,y.lo);
  return(z);
}

INTERVAL unionDDI(double x, double y) {
  INTERVAL z;
  if (x <= y) {
    z.lo = x; z.hi = y; }
  else {
    z.lo = y; z.hi = x; }
  return(z);
}



    /*
    ****************************************************************
    addition/subtraction/negation of INTERVALs
    ****************************************************************
    */

/* addition */
INTERVAL addDDI(double x, double y) {
  INTERVAL z;

  z.hi = add_hi(x,y);
  z.lo = add_lo(x,y);
  return(z);
}

INTERVAL addDII(double x, INTERVAL y) {
  INTERVAL z;
  z.hi = add_hi(x,y.hi);
  z.lo = add_lo(x,y.lo);
  return(z);
}

INTERVAL addIDI(INTERVAL x, double y) {
  INTERVAL z;
  z.hi = add_hi(x.hi,y);
  z.lo = add_lo(x.lo,y);
  return(z);
}

INTERVAL addIII(INTERVAL x, INTERVAL y) {
  INTERVAL z;
  z.hi = add_hi(x.hi,y.hi);
  z.lo = add_lo(x.lo,y.lo);
  return(z);
}


/* subtraction */
INTERVAL subDDI(double x, double y) {
  INTERVAL z;
  z.hi = sub_hi(x,y);
  z.lo = sub_lo(x,y);
  return(z);
}

INTERVAL subDII(double x, INTERVAL y) {
  INTERVAL z;
  z.hi = sub_hi(x,y.lo);
  z.lo = sub_lo(x,y.hi);
  return(z);
}

INTERVAL subIDI(INTERVAL x, double y) {
  INTERVAL z;
  z.hi = sub_hi(x.hi,y);
  z.lo = sub_lo(x.lo,y);
  return(z);
}

INTERVAL subIII(INTERVAL x, INTERVAL y) {
  INTERVAL z;
  z.hi = sub_hi(x.hi,y.lo);
  z.lo = sub_lo(x.lo,y.hi);
  return(z);
}

/* negation */
INTERVAL negII(INTERVAL x) {
  INTERVAL z;
  z.hi = -x.lo;
  z.lo = -x.hi;
  return(z);
}

INTERVAL negDI(INTERVAL x) {
  INTERVAL z;
  z.hi = -(x.lo);
  z.lo = -(x.hi);
  return(z);
}


/* multiplication*/
INTERVAL mulDDI(double x, double y) {  /* these have not yet been optimized */
  return(mulIII(cnstDI(x),cnstDI(y)));
}
INTERVAL mulDII(double x, INTERVAL y) {
  return(mulIII(cnstDI(x),y));
}
INTERVAL mulIDI(INTERVAL x, double y) {
  return(mulIII(x,cnstDI(y)));
}

INTERVAL mulIII(INTERVAL x, INTERVAL y) {
 INTERVAL z;
   if (is_zeroI(x) || is_zeroI(y)) {
       z.hi=z.lo=POSZERO;
   }else if (non_negI(x)) {
       if (non_negI(y))
         {z.lo = mul_lo(x.lo,y.lo); z.hi = mul_hi(x.hi,y.hi);}
        else if (non_posI(y))
         {z.lo = mul_lo(x.hi,y.lo); z.hi = mul_hi(x.lo,y.hi);}
        else /* splitI(y) */
         {z.lo = mul_lo(x.hi,y.lo); z.hi=mul_hi(x.hi,y.hi);}
   }else if (non_posI(x)) {
       if (non_negI(y))
         {z.lo = mul_lo(x.lo,y.hi); z.hi = mul_hi(x.hi,y.lo);}
        else if (non_posI(y))
         {z.lo = mul_lo(x.hi,y.hi); z.hi = mul_hi(x.lo,y.lo);}
        else /* splitI(y) */
         {z.lo = mul_lo(x.lo,y.hi); z.hi=mul_hi(x.lo,y.lo);}
   }else /* splitI(x) */
       if (non_negI(y))
         {z.lo = mul_lo(x.lo,y.hi); z.hi = mul_hi(x.hi,y.hi);}
        else if (non_posI(y))
         {z.lo = mul_lo(x.hi,y.lo); z.hi = mul_hi(x.lo,y.lo);}
        else /* splitI(y) */
         {z.lo = min2(mul_lo(x.hi,y.lo),mul_lo(x.lo,y.hi));
          z.hi = max2(mul_hi(x.lo,y.lo),mul_hi(x.hi,y.hi));}
   return(z);
}



/* division */
INTERVAL divDDI(double x, double y) {  /* these have not yet been optimized */
  return(divIII(cnstDI(x),cnstDI(y)));
}
INTERVAL divDII(double x, INTERVAL y) {
  return(divIII(cnstDI(x),y));
}
INTERVAL divIDI(INTERVAL x, double y) {
  return(divIII(x,cnstDI(y)));
}

INTERVAL divIII(INTERVAL x, INTERVAL y) {
/*
  This assumes 0/0 is undefined and hence returns
  [-inf,inf] for x/y if   (0 in x) and (0 in y)
*/
  INTERVAL z;
   if (contains_zeroI(x) && contains_zeroI(y))
    {z.lo = NEGINF; z.hi = POSINF;}
  else {
    if (IS_ZERO(y.lo)) y.lo = POSZERO;
    if (IS_ZERO(y.hi)) y.hi = NEGZERO;

    if (non_negI(x)) {
      if (non_negI(y))
        {z.lo = div_lo(x.lo,y.hi); z.hi=div_hi(x.hi,y.lo);}
      else if (non_posI(y))
        {z.lo = div_lo(x.hi,y.hi); z.hi=div_hi(x.lo,y.lo);}
      else /* splitI(y) */
        {z.lo = NEGINF; z.hi = POSINF;}
    }else if (non_posI(x)) {
      if (non_negI(y))
        {z.lo = div_lo(x.lo,y.lo); z.hi=div_hi(x.hi,y.hi);}
      else if (non_posI(y))
        {z.lo = div_lo(x.hi,y.lo); z.hi=div_hi(x.lo,y.hi);}
      else /* splitI(y) */
        {z.lo = NEGINF; z.hi = POSINF;}
    }else { /* splitI(x) */
      if (non_negI(y))
        {z.lo = div_lo(x.lo,y.lo); z.hi=div_hi(x.hi,y.lo);}
      else if (non_posI(y))
        {z.lo = div_lo(x.hi,y.hi); z.hi=div_hi(x.lo,y.hi);}
      else /* splitI(y) */
        {z.lo = NEGINF; z.hi = POSINF;}
    }
  }
  return(z);
}



int intersect_mulIII(INTERVAL x, INTERVAL y, INTERVAL *z) {
    *z=intersectIII(*z,mulIII(x,y));
    if (emptyI(*z)) return(FALSE);
    return(TRUE);
}


int intersect_divIII(INTERVAL z, INTERVAL x, INTERVAL *y) {  /* *y = *y intersect z/x), return(nonemptyI(*y)) */
  double tmp_pos,tmp_neg;

  if (splitI(x)) {
    if (posI(z)) {
       tmp_neg = div_hi(z.lo,x.lo); 
       tmp_pos = div_lo(z.lo,x.hi);
       if ((GT(y->lo,tmp_neg)) && (y->lo < tmp_pos)) y->lo = tmp_pos;
       if ((LT(y->hi,tmp_pos)) && (y->hi > tmp_neg)) y->hi = tmp_neg;
       return(nonemptyI(*y));
     }
    else if (negI(z)) {
       tmp_neg = div_hi(z.hi,x.hi); 
       tmp_pos = div_lo(z.hi,x.lo);
       if ((GT(y->lo,tmp_neg)) && (y->lo < tmp_pos)) y->lo = tmp_pos;
       if ((LT(y->hi,tmp_pos)) && (y->hi > tmp_neg)) y->hi = tmp_neg;
       return(nonemptyI(*y));
    }
    else return(TRUE);
  }else {
     *y = intersectIII(*y,divIII(z,x));
     return(nonemptyI(*y));
   }
}



INTERVAL squareII(INTERVAL A){
  return(mulIII(A,A));
}

INTERVAL squareDI(double x) {
  return(mulDDI(x,x));
}


INTERVAL sqrtII(INTERVAL A){
  INTERVAL B;

  if (A.hi < 0) {
    B.lo = POSINF; B.hi = NEGINF;
    return(B);
  }
  else {
    B.hi = sqrt_hi(A.hi);
  }

  if (A.lo < 0) B.lo = 0;
  else B.lo = sqrt_lo(A.lo);

  return(B);
}

INTERVAL sqrtDI(double x) {
  return(makeDDI(sqrt_lo(x),sqrt_hi(x)));
}


    /*
    ****************************************************************
    misc functions (abs,sgn,floor,ceil,max,min,two_to_n)    
    ****************************************************************
    */

INTERVAL absII(INTERVAL a){
  INTERVAL b;

  if (a.hi <= 0) {
    b.lo = -a.hi;
    b.hi = -a.lo;
    return(b);
  }
  else if (a.lo < 0) {
    b.lo = 0;
    if (a.hi < -a.lo)
      b.hi = -a.lo;
    else
      b.hi = a.hi;
    return(b);
  }
  else return(a);
}

INTERVAL sgnII(INTERVAL a){
  INTERVAL b;

  b.lo = -1; b.hi = 1;

  if (a.hi < 0) {
    b.hi = -1;
  }
  else if (a.hi == 0) {
    if (a.lo == 0){
      b.lo =  0; b.hi = 0;}
    else {
      b.hi = 0;}
  }
 else /* a.hi > 0 */ {
   if (a.lo < 0) {
      /* b is [-1,1] */; }
   else if (a.lo == 0) {
      b.lo = 0; }
   else if (a.lo > 0) {
    b.lo = 1;}
 }

 return(b);
}

INTERVAL floorII(INTERVAL a) { /* b is the largest integer <= a */
  INTERVAL b;
  printf("floorII not yet fully implemented \n");
  b.lo = a.lo - 1;
  b.hi = a.hi;
  return(b);
}

INTERVAL ceilII(INTERVAL a) { /* b is the smallest integer >= a */
  INTERVAL b;
  printf("floorII not yet fully implemented\n");
  b.lo = a.lo;
  b.hi = a.hi+1;
  return(b);
}

INTERVAL maxIII(INTERVAL a,INTERVAL b) {
  INTERVAL c;
  c.hi = max2(a.hi,b.hi);
  c.lo = max2(a.lo,b.lo);
  return(c);
}

INTERVAL minIII(INTERVAL a,INTERVAL b){
  INTERVAL c;
  c.hi = min2(a.hi,b.hi);
  c.lo = min2(a.lo,b.lo);
  return(c);
}



