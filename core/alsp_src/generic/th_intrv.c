/*
  File:     "th_intrv.c"
  Author:    Tim Hickey
  Date:      1/8/97

  This file contains procedures which implement the narrowing primitives
  in the ALS version of clpbnr.

 History:
  1/8: first version, simply redefines i_add using TH routines
  1/10: second version -- interfaces with Tim Hickey's smath library

 Plans:


  Comments:
  * original BNR version used unsound implementation of strict
    inequality! (e.g. in the "higher" operation)

 
*/

#include "defs.h"

#ifdef TH_INTRV
#ifdef INTCONSTR
#include "intrv.h"
#include "freeze.h"     
#include "intrv_pr.h"

#include "smath.h"
/*#include "th_ia.h"*/


	/*--- Vars for args to the primitives: ---*/
extern double zl,zh,xl,xh,yl,yh,ul,uh,vl,vh;
extern int status;


/* prototypes of narrowing procedures currently not in smath library */
int narrow_begin_tog(INTERVAL *x, INTERVAL *z);
int narrow_finish_tog(INTERVAL *x, INTERVAL *z);
int narrow_narrower(INTERVAL *x, INTERVAL *z);
int narrow_pow_odd(INTERVAL *x, INTERVAL *y, INTERVAL *z);
int narrow_pow_even(INTERVAL *x, INTERVAL *y, INTERVAL *z);


    /*
    ****************************************************************
    interface procedures between ALS clpbnr and TH's smath library
    the interfacing is done using the "call_narrowing_proc?" procedure.
    There are two versions: one for arity 2  and one for arity 3
    narrowing primitives.
    ****************************************************************
    */

int call_narrowing_proc3(int (*proc)(INTERVAL *, INTERVAL *, INTERVAL *));


int
call_narrowing_proc3(int (*proc)(INTERVAL *, INTERVAL *, INTERVAL *))
{
  INTERVAL x,y,z;
  int success;

  x.lo=xl; x.hi=xh;    y.lo=yl; y.hi=yh;  z.lo=zl; z.hi=zh;

  success = proc(&x,&y,&z);

  if (!success) { xl=1;xh=0; xlchng();xhchng();  return(0);  }
  else {
    if (x.lo != xl) {xlchng(); xl = x.lo;}
    if (x.hi != xh) {xhchng(); xh = x.hi;}
    if (y.lo != yl) {ylchng(); yl = y.lo;}
    if (y.hi != yh) {yhchng(); yh = y.hi;}
    if (z.lo != zl) {zlchng(); zl = z.lo;}
    if (z.hi != zh) {zhchng(); zh = z.hi;}
    return(0);
  }
}


/****************************************************************/

int call_narrowing_proc3flip(int (*proc)(INTERVAL *, INTERVAL *, INTERVAL *));


int
call_narrowing_proc3flip(int (*proc)(INTERVAL *, INTERVAL *, INTERVAL *))
{
  INTERVAL x,y,z;
  int success;

  x.lo=xl; x.hi=xh;    y.lo=yl; y.hi=yh;  z.lo=zl; z.hi=zh;

  success = proc(&x,&z,&y);

  if (!success) { xl=1;xh=0; xlchng();xhchng();  return(0);  }
  else {
    if (x.lo != xl) {xlchng(); xl = x.lo;}
    if (x.hi != xh) {xhchng(); xh = x.hi;}
    if (y.lo != yl) {ylchng(); yl = y.lo;}
    if (y.hi != yh) {yhchng(); yh = y.hi;}
    if (z.lo != zl) {zlchng(); zl = z.lo;}
    if (z.hi != zh) {zhchng(); zh = z.hi;}
    return(0);
  }
}


/****************************************************************/

int call_narrowing_proc2(int (*proc)(INTERVAL *, INTERVAL *));


int
call_narrowing_proc2(int (*proc)(INTERVAL *, INTERVAL *))
{
  INTERVAL x,z;
  int success;

  x.lo=xl; x.hi=xh;    z.lo=zl; z.hi=zh;

  success = proc(&x,&z);

  if (!success) { xl=1;xh=0; xlchng();xhchng();  return(0);  }
  else {
    if (x.lo != xl) {xlchng(); xl = x.lo;}
    if (x.hi != xh) {xhchng(); xh = x.hi;}
    if (z.lo != zl) {zlchng(); zl = z.lo;}
    if (z.hi != zh) {zhchng(); zh = z.hi;}
    return(0);
  }
}


int call_narrowing_proc2reverse(int (*proc)(INTERVAL *, INTERVAL *));


int
call_narrowing_proc2reverse(int (*proc)(INTERVAL *, INTERVAL *))
{
  INTERVAL x,z;
  int success;

  x.lo=xl; x.hi=xh;    z.lo=zl; z.hi=zh;

  success = proc(&z,&x);

  if (!success) { xl=1;xh=0; xlchng();xhchng();  return(0);  }
  else {
    if (x.lo != xl) {xlchng(); xl = x.lo;}
    if (x.hi != xh) {xhchng(); xh = x.hi;}
    if (z.lo != zl) {zlchng(); zl = z.lo;}
    if (z.hi != zh) {zhchng(); zh = z.hi;}
    return(0);
  }
}


/* **************************************************************** */

 int	i_unequal		PARAMS((void));

int
i_unequal() /* x<>z */
{
  return(call_narrowing_proc2(&narrow_ne));
}

/* **************************************************************** */

 int	i_equal		PARAMS((void));

int
i_equal() /* x=z */
{
  return(call_narrowing_proc2(&narrow_eq));
}

/* **************************************************************** */

 int	i_greatereq		PARAMS((void));

int
i_greatereq() /* z >= x */
{
  return(call_narrowing_proc2(&narrow_le));
}

/* **************************************************************** */

 int	i_higher		PARAMS((void));

int
i_higher() /* x > z  */
{
  return(call_narrowing_proc2reverse(&narrow_lt));
}

/* **************************************************************** */




 int	i_add		PARAMS((void));

int
i_add()
{
  return(call_narrowing_proc3(&narrow_add));
}

/* **************************************************************** */

 int	i_begin_tog		PARAMS((void));

int
i_begin_tog() /* this is a non-logical operation !! */
{
  return(call_narrowing_proc2(&narrow_begin_tog));
}

/* **************************************************************** */

 int	i_cos		PARAMS((void));

int
i_cos()
{
  return(call_narrowing_proc2(&narrow_cos));
}

/* **************************************************************** */

 int	i_finish_tog		PARAMS((void));

int
i_finish_tog()
{
  return(call_narrowing_proc2(&narrow_finish_tog));
}

/* **************************************************************** */

 int	i_inf		PARAMS((void));

int
i_inf()
{
  return(call_narrowing_proc3(&narrow_min));
}

/* **************************************************************** */


 int	i_j_less		PARAMS((void));

int
i_j_less()
{
  return(call_narrowing_proc3flip(&narrow_lessfn));
}

/* **************************************************************** */



 int	i_k_equal		PARAMS((void));

int
i_k_equal()
{
  return(call_narrowing_proc3flip(&narrow_eqfn));
}

/* **************************************************************** */


 int	i_lub		PARAMS((void));

int
i_lub()
{
  return(call_narrowing_proc3(&narrow_max));
}

/* **************************************************************** */

 int	i_mul		PARAMS((void));

int
i_mul()
{
  return(call_narrowing_proc3(&narrow_mul));
}


/* **************************************************************** */


 int	i_narrower		PARAMS((void));

int
i_narrower() /* X <= Z */
{
  return(call_narrowing_proc2(&narrow_subset));

}

/* **************************************************************** */


 int	i_or		PARAMS((void));

int
i_or()
{
  return(call_narrowing_proc3(&narrow_or));
}

/* **************************************************************** */


 int	i_pow_odd		PARAMS((void));

int
i_pow_odd()
{
  return(call_narrowing_proc3(&narrow_pow_odd));
}

/* **************************************************************** */

 int	i_qpow_even		PARAMS((void));

int
i_qpow_even()
{
  return(call_narrowing_proc3(&narrow_pow_even));
}

/* **************************************************************** */

 int	i_rootsquare		PARAMS((void));

int
i_rootsquare()
{
  return(call_narrowing_proc2(&narrow_square));
}

/* **************************************************************** */


 int	i_sin		PARAMS((void));

int
i_sin()
{
  return(call_narrowing_proc2(&narrow_sin));
}

/* **************************************************************** */

 int	i_tan		PARAMS((void));

int
i_tan()
{
  return(call_narrowing_proc2(&narrow_tan));
}

/* **************************************************************** */

 int	i_vabs		PARAMS((void));

int
i_vabs() /* z = abs(x) */
{
  return(call_narrowing_proc2(&narrow_abs));
}

/* **************************************************************** */

 int	i_wrap		PARAMS((void));

int
i_wrap()
{
  /* this is supposed to fold the line into an interval
     i.e., it is the saw tooth function, and it is used
     in the trig functions, currently, we will just
     make it the identity function since our trig
     functions do the range reduction themselves.
  */
  return(call_narrowing_proc2(&narrow_eq));
}

/* **************************************************************** */



 int	i_xp		PARAMS((void));

int
i_xp()
{
  return(call_narrowing_proc2(&narrow_exp));
}

/* **************************************************************** */

#endif


/* ****************************************************************
   ****************************************************************

 SOME IMPLEMENTATION NOTES as of 1/8/97: 
  The intrv.c procedures assume that their inputs
  are the INTERVALs [xl,xh] [yl,yh] [zl,zh],
  where there is a global declaration
      double xl,xh,yl,yh,zl,zh;
  In the case of binary inputs, x and z are used.

  The procedures should narrow the x,y,z bounds
  and they need to set the status flag appropriately.
  The status word has 10 flags the first 9 determine
  whether x,y,or z has flipped, 
  or has had its lower/upper bound changed
  (See intrv.h xlchange for details).
  There are also two additional flags 
    redonode -- this is set when integer rounding changes a bound
    link -- I don't know when this is set, I can't find it anywhere

  These procedures are called from the file int_net.c
  inside a switch, after the switch the following is done:
    if any of the lower bounds is greater than the upper bound, fail
    otherwise, for each variable that changed (determined by status word)
    add corresponding constraints onto the queue.

  So, these procedures should do the narrowing and 
    * if failure is detected, do the following:
          xl=1; xh=0; xlchng();
    * else, set status bits appropriately and return
      use 
        xlchng(); xhchng();
        ylchng(); yhchng();
        zlchng(); zhchng();
      to signal changes to variable bounds.
  All procedures return 0 always, FAIL is a macro for "return(0)"

   ****************************************************************
   **************************************************************** */

#endif /* TH_INTRV */
