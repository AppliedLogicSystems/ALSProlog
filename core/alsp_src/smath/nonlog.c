    /*
    ****************************************************************
    File:     "trig.c" -- 
    Author:   Tim Hickey
    Date:     15 January 1997

    Definitions of basic nonlogical narrowing primitives 
          subset, begin_tog, end_tog, narrower

    History:
    1/16/97 -- implemented version based on one by Tim Hickey/Qun Ju
    1/24/97 -- modified headers

    ****************************************************************
    */

#include "smath.h"
#include "ieeemath.h"

extern NEWDOUBLE MAX_REAL, POS_INF, NEG_INF, POS_ZERO, NEG_ZERO;

/* **************************************************************** */
/* non-logical operations  */
int narrow_subset(INTERVAL *x,INTERVAL *y){            /* interval x is contained in interval y */
  *x = intersectIII(*x,*y);
  return(nonemptyI(*x));
}


int narrow_begin_tog(INTERVAL *x, INTERVAL *y){        /* intervals x and y have the same lower bound */
  if (x->lo < y->lo) 
    x->lo = y->lo;
  else
    y->lo = x->lo;

  return(nonemptyI(*x) && nonemptyI(*y));
}

int narrow_finish_tog(INTERVAL *x, INTERVAL *y){       /* intervals x and y have the same upper bound */
  if (x->hi > y->hi) 
    x->hi = y->hi;
  else
    y->hi = x->hi;

  return(nonemptyI(*x) && nonemptyI(*y));
}

int narrow_narrower(INTERVAL *x, INTERVAL *y){         /* same as narrow_subset */
  *x = intersectIII(*x,*y);
  return(nonemptyI(*x));
}

