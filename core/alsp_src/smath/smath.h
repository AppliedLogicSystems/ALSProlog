/*
  File:     smath.h
  Author:   Tim Hickey
  Date:     15 January 1997

  History:
  1/15/97 -- initial version
  1/28/97 -- changed ranges in comments for trig0DI function,
             inserted prototypes for exp_hi,exp_lo,...,atan2pi_lo

  Contents:
    Data structures and prototypes for the sound math library.
    The library has several types of sound math operations:
      * narrowing operators
      * semi-narrowing intersection operators
      * interval arithmetic operators
      * soundly rounded functions


    More precisely, the smath library implements the
    following four types of functions:
    
    I. Narrowing functions.
       These function have the form narrow_op(A1,...,An)
       where the Ai are pointers to intervals, and "op" is
       a function or relation. Thus,
         narrow_add(&x,&y,&z) will narrow the x,y,z intervals
       without removing any solutions to x+y=z. The narrowing
       functions return false if any of the argument intervals 
       become narrowed to the empty set, i.e., if the relation
       op(x1,...,xn) has been shown to have no solutions with
       (x1,...,xn) in (A1,...,An)
    
    
    II. Semi-narrowing functions.
       These functions have the form intersect_op(A1,...,An,X)
       where the Ai and *X are intervals, and "op" is a function.
       They implement the following operation:
               X = (X intersect op(A1,...,An))
       and they are especially useful when op(A1,...,An) is a
       union of intervals. If the intersection is empty, then
       intersect_op returns FAIL, otherwise it returns TRUE.
    
       Thus, intersect_inv_sin(*a,*b) will intersect b with an 
       enclosure of sin^(-1)(a), but will have no effect on a. 
       Notice that we can implement narrow_sin(*x,*y) as
        return((intersect_sin(x,y)&&intersect_inv_sin(y,x)));
       and indeed, any narrowing function can be implemented
       by "and"ing together the semi-narrowing functions for
       its component "functions".
    
    III. Interval extensions of functions.
      These have the form opII(A1) where A1 is an interval,
      and op is a function, and opII(A1) is an interval.
      Thus, expII(x) returns an interval y which contains
      all points exp(t) with t in x.
    
    IV. Soundly rounded functions.
      These have the form op_lo(x) (resp. op_hi(x)) and they
      return lower (resp. upper) bounds on op(x). Thus,
         exp_lo(x) <= exp(x) <= exp_hi(x),    for all x

*/

    /*
    ****************************************************************
              smath data structures
    ****************************************************************
    */

typedef struct interval{
  double lo;
  double hi;
} INTERVAL, *INTERVALP;



    /* 
    ****************************************************************
    Some convenient I/O functions for debugging
    ****************************************************************
    */

void print_interval(char *s, INTERVAL p);
void print_fp(double x);


    /* 
    ****************************************************************
    The Narrowing Functions 
    These return 1 for success and 0 for failure.
    ****************************************************************
    */

/* core arithmetic */
  /* basic operators */
int narrow_add(INTERVAL *x,INTERVAL *y, INTERVAL *z);  /* x + y = z */
int narrow_mul(INTERVAL *x,INTERVAL *y, INTERVAL *z);  /* x * y = z */
int narrow_u_minus(INTERVAL *x, INTERVAL *y);          /*   -x  = z */
int narrow_square(INTERVAL *x, INTERVAL *y);           /*   x*x = z */
  /* comparisons */
int narrow_eq(INTERVAL *x,INTERVAL *y);                /* x  =  y */
int narrow_lt(INTERVAL *x,INTERVAL *y);                /* x  <  y */
int narrow_le(INTERVAL *x,INTERVAL *y);                /* x <=  y */
int narrow_ne(INTERVAL *x,INTERVAL *y);                /* x !=  y */
  /* type functions */
int narrow_integer(INTERVAL *x);                       /* x is an integer */
int narrow_bool(INTERVAL *x);                          /* x is an integer in the interval [0,1] */
  /* miscellaneous real functions */
int narrow_abs(INTERVAL *x,INTERVAL *y);               /* y = |x| */  
int narrow_sgn(INTERVAL *x,INTERVAL *y);               /* y = sgn(x) = x/|x|  (or 0 if x = 0) */
int narrow_min(INTERVAL *x,INTERVAL *y, INTERVAL *z);  /* z = min(x,y) */
int narrow_max(INTERVAL *x,INTERVAL *y, INTERVAL *z);  /* z = max(x,y) */
int narrow_flr(INTERVAL *x,INTERVAL *y);               /* y = largest integer less than or equal to x */
int narrow_ceil(INTERVAL *x,INTERVAL *y);              /* y = smallest integer greater than or equal to x */

/* boolean functions */
int narrow_or(INTERVAL *x,INTERVAL *y, INTERVAL *z);   /* z = x or y */
int narrow_and(INTERVAL *x,INTERVAL *y, INTERVAL *z);  /* z = x and y */
int narrow_xor(INTERVAL *x,INTERVAL *y, INTERVAL *z);  /* z = x xor y */
int narrow_not(INTERVAL *x,INTERVAL *y);               /* z = not(y) */
int narrow_imp(INTERVAL *x,INTERVAL *y, INTERVAL *z);  /* z = (x => y) */
int narrow_if(INTERVAL *x,INTERVAL *y, INTERVAL *z);   /* z = (x <= y) */
int narrow_lessfn(INTERVAL *x,INTERVAL *y, INTERVAL *z); /* z = (x < y)  */
int narrow_leqfn(INTERVAL *x,INTERVAL *y, INTERVAL *z);  /* z = (x <= y) */
int narrow_eqfn(INTERVAL *x,INTERVAL *y, INTERVAL *z);   /* z = (x = y) */


/* non-logical operations */
int narrow_subset(INTERVAL *x,INTERVAL *y);            /* interval x is contained in interval y */
int narrow_begin_tog(INTERVAL *x, INTERVAL *z);        /* intervals x and y have the same lower bound */
int narrow_finish_tog(INTERVAL *x, INTERVAL *z);       /* intervals x and y have the same upper bound */
int narrow_narrower(INTERVAL *x, INTERVAL *z);         


/* exponential functions */
int narrow_exp(INTERVAL *x, INTERVAL *y);                        /* y = exp(x) */
int narrow_pow_odd(INTERVAL *x, INTERVAL *y, INTERVAL *z);       /* z = sgn(x)* abs(x)**y, (or 0 if x = 0) */
int narrow_pow_even(INTERVAL *x, INTERVAL *y, INTERVAL *z);      /* z =         abs(x)**y, (or 0 if x = 0) */

/* trig functions */
int narrow_sin(INTERVAL *x,INTERVAL *y);         /* y = sin(x) */
int narrow_cos(INTERVAL *x,INTERVAL *y);         /* y = cos(x) */
int narrow_tan(INTERVAL *x,INTERVAL *y);         /* y = tan(x) */

int narrow_sin2pi(INTERVAL *x,INTERVAL *y);         /* y = sin(2*pi*x) */
int narrow_cos2pi(INTERVAL *x,INTERVAL *y);         /* y = cos(2*pi*x) */
int narrow_tan2pi(INTERVAL *x,INTERVAL *y);         /* y = tan(2*pi*x) */

    /* 
    ****************************************************************
    THE INTERSECTION OPERATORS, a.k.a the SEMI-NARROWING FUNCTIONS
    These operators have the form  intersect_f(A,...,*X)
    They are essentially half of the narrowing operation
          X=f(A,...)
    In that they intersect X with the image f(A,...).
    If the intersection is empty, then the functions returns FAIL.
    ****************************************************************
    */
int intersect_mulIII(INTERVAL a, INTERVAL b, INTERVAL *x);
int intersect_divIII(INTERVAL a, INTERVAL b, INTERVAL *x);

int intersect_sin2piII(INTERVAL z, INTERVAL *x);
int intersect_cos2piII(INTERVAL z, INTERVAL *x);
int intersect_tan2piII(INTERVAL z, INTERVAL *x);
int intersect_inv_sin2piII(INTERVAL z, INTERVAL *x);
int intersect_inv_cos2piII(INTERVAL z, INTERVAL *x);
int intersect_inv_tan2piII(INTERVAL z, INTERVAL *x);

int intersect_sinII(INTERVAL z, INTERVAL *x);
int intersect_cosII(INTERVAL z, INTERVAL *x);
int intersect_tanII(INTERVAL z, INTERVAL *x);
int intersect_inv_sinII(INTERVAL z, INTERVAL *x);
int intersect_inv_cosII(INTERVAL z, INTERVAL *x);
int intersect_inv_tanII(INTERVAL z, INTERVAL *x);





    /* 
    ****************************************************************
    The INTERVAL ARITHMETIC OPERATORS
    ****************************************************************
    */

/* constructors */
INTERVAL cnstDI(double a);
INTERVAL makeDDI(double lo,double hi);

/* pure interval arithmetic operators */
INTERVAL addIII(INTERVAL a, INTERVAL b);
INTERVAL subIII(INTERVAL a, INTERVAL b);
INTERVAL mulIII(INTERVAL a, INTERVAL b);
INTERVAL divIII(INTERVAL a, INTERVAL b);
INTERVAL negII(INTERVAL a);
INTERVAL sqrtII(INTERVAL A);
INTERVAL squareII(INTERVAL A);

INTERVAL intersectIII(INTERVAL a, INTERVAL b);
INTERVAL unionIII(INTERVAL A, INTERVAL B);

INTERVAL absII(INTERVAL a);
INTERVAL sgnII(INTERVAL a);
INTERVAL floorII(INTERVAL a);
INTERVAL ceilII(INTERVAL a);

INTERVAL maxIII(INTERVAL a,INTERVAL b);
INTERVAL minIII(INTERVAL a,INTERVAL b);

INTERVAL two_to_nDI(int n);

INTERVAL expII(INTERVAL X);
INTERVAL logII(INTERVAL X);


INTERVAL sin2piII(INTERVAL X);
INTERVAL cos2piII(INTERVAL X);
INTERVAL tan2piII(INTERVAL X);
INTERVAL asin2piII(INTERVAL X);
INTERVAL acos2piII(INTERVAL X);
INTERVAL atan2piII(INTERVAL X);

INTERVAL sinII(INTERVAL X);
INTERVAL cosII(INTERVAL X);
INTERVAL tanII(INTERVAL X);
INTERVAL asinII(INTERVAL X);
INTERVAL acosII(INTERVAL X);
INTERVAL atanII(INTERVAL X);


/* mixed interval/double arithmetic operators */
/* these are convenience functions */
INTERVAL addIDI(INTERVAL a, double b);
INTERVAL subIDI(INTERVAL a, double b);
INTERVAL mulIDI(INTERVAL a, double b);
INTERVAL divIDI(INTERVAL a, double b);

INTERVAL addDII(double a, INTERVAL b);
INTERVAL subDII(double a, INTERVAL b);
INTERVAL mulDII(double a, INTERVAL b);
INTERVAL divDII(double a, INTERVAL b);

INTERVAL addDDI(double a, double b);
INTERVAL subDDI(double a, double b);
INTERVAL mulDDI(double a, double b);
INTERVAL divDDI(double a, double b);

INTERVAL squareDI(double x);
INTERVAL sqrtDI(double x);

INTERVAL expDI(double x);
INTERVAL logDI(double x);


/* 
   These trig functions are only defined 
   for small values of x
*/
INTERVAL sin2pi0DI(double x); /* -1/4 <= x <= 1/4 */
INTERVAL cos2pi0DI(double x); /* -1/4 <= x <= 1/4 */
INTERVAL tan2pi0DI(double x); /* -1/4 <= x <= 1/4 */
INTERVAL asin2piDI(double x); /* returns y in [-1/4,1/4] */
INTERVAL acos2piDI(double x); /* returns y in [   0 1/2] */
INTERVAL atan2piDI(double x); /* returns y in [-1/4,1/4] */

INTERVAL sin0DI(double x);  /* -pi/2 <= x <= pi/2 */ 
INTERVAL cos0DI(double x);  /* -pi/2 <= x <= pi/2 */ 
INTERVAL tan0DI(double x);  /* -pi/2 <= x <= pi/2 */ 
INTERVAL asinDI(double x); /* returns y in [-pi/2,pi/2] */
INTERVAL acosDI(double x); /* returns y in [  0  , pi ] */
INTERVAL atanDI(double x); /* returns y in [-pi/2,pi/2] */

INTERVAL unionDDI(double a, double b); /* returns smallest interval containing a and b */


   /* 
   ****************************************************************
   Below are the soundly rounded versions of the elementary functions
   they are designed to yield upper and lower bounds, but 
   are not necessarily optimally precise 
   ****************************************************************
   */
   

double exp_hi(double x);
double exp_lo(double x);
double log_hi(double x);
double log_lo(double x);

double sin0_hi(double x);
double sin0_lo(double x);
double cos0_hi(double x);
double cos0_lo(double x);
double tan0_hi(double x);
double tan0_lo(double x);
double asin_hi(double x);
double asin_lo(double x);
double acos_hi(double x);
double acos_lo(double x);
double atan_hi(double x);
double atan_lo(double x);

double sin2pi0_hi(double x);
double sin2pi0_lo(double x);
double cos2pi0_hi(double x);
double cos2pi0_lo(double x);
double tan2pi0_hi(double x);
double tan2pi0_lo(double x);
double asin2pi_hi(double x);
double asin2pi_lo(double x);
double acos2pi_hi(double x);
double acos2pi_lo(double x);
double atan2pi_hi(double x);
double atan2pi_lo(double x);



