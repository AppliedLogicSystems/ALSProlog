    /*
    ****************************************************************
    File:     "bool.c" -- 
    Author:   Tim Hickey
    Date:     15 January 1997

    Definitions of basic boolean narrowing primitives 
         operators (or,and,not,imp,if)
         comparison functions (=,<,<=)

    History:
    1/16/97 -- implemented version based on one by Tim Hickey/Qun Ju
    1/24/97 -- adjusted headers
    ****************************************************************
    */

#include "smath.h"
#include "ieeemath.h"

/* **************************************************************** */
/* boolean functions */


/* ****************************************************************
          intersection(x1,x2, y)
		intersect *y with [x1,x2]
   ************************************************************** */
static intersection(double x1,double x2, INTERVAL *y)
{
    if (x1 > y->lo)   y->lo = x1; 
    if (x2 < y->hi)   y->hi = x2; 

    if (y->lo > y->hi)  {return(FAIL);}
    return(TRUE);
}


narrow_and(INTERVAL *x, INTERVAL *y, INTERVAL *z)
{
/*** narrow  z  ****/                                    /* both x y known */
    if ((x->hi==0.0) || (y->hi==0.0)) 
       return(intersection(0.0, 0.0, z));
    if ((x->lo==1.0) && (y->lo==1.0)) 
       return(intersection(1.0, 1.0, z));

/*** narrow x, y ****/                            /* x and y not all known */
    if (z->hi==0.0)  {
        if (x->lo==1.0)      y->hi=0.0; 
        if (y->lo==1.0)      x->hi=0.0;
      }
    else if (z->lo==1.0)  {
        x->lo=1.0; 
        y->lo=1.0;
      }
    return((x->lo <= x->hi) && (y->lo <= y->hi));
}



narrow_or(INTERVAL *x, INTERVAL *y, INTERVAL *z)
{
/*** narrow  z  ****/                                    /* both x y kown */
    if ((x->lo==1.0) || (y->lo==1.0))
       return(intersection(1.0, 1.0, z));
    if ((x->hi==0.0) && (y->hi==0.0))
       return(intersection(0.0, 0.0, z));

/*** narrow x, y ****/                            /* x and y not all known */
    if (z->lo==1.0)   {
        if (x->hi==0.0)     y->lo=1.0;
        if (y->hi==0.0)     x->lo=1.0;
      }
    else if (z->hi==0.0)  {
        x->hi=0.0; 
        y->hi=0.0;
      }
   return((x->lo <= x->hi) && (y->lo <= y->hi));
}



narrow_not(INTERVAL *x, INTERVAL *y)
{
/*** narrow  z  ****/                                            /* x kown */
    if (x->lo==1.0)
        return(intersection(0.0, 0.0, y));
    if (x->hi==0.0)
        return(intersection(1.0, 1.0, y));

/*** narrow  z  ****/                                /* x unknown, y known */  
    if (y->lo==1.0)
        x->hi=0.0;
    else if (y->hi==0.0)
        x->lo=1.0;
    return(x->lo <= x->hi);
}



narrow_imp(INTERVAL *x, INTERVAL *y, INTERVAL *z)
{
/*** narrow  z  ****/    
    if ((x->lo==1.0) && (y->lo==y->hi))                /* x = 1 and y known */
        return(intersection(y->lo, y->hi, z)); 
    if ((x->hi==0.0) || (y->lo==1.0))                  /* x = 0 or y=1 */
        return(intersection(1.0, 1.0, z));

/*** narrow x, y ****/    /* x=1 and y unkown, x unknown and y=0 or unknown */
    if (z->hi==0.0)                                          /* z = 0 */
      { x->lo=1.0;  y->hi=0.0; }
    else if (z->lo==1.0)                                          /* z = 1 */
           {if (x->lo==1.0)        /* x=1 */      
                 y->lo=1,0;
            else if (y->hi==0.0)   /* x unknow , y=0 */
                  x->hi=0.0;
	  }
   return((x->lo <= x->hi) && (y->lo <= y->hi));

}


/****************************************************************/

int narrow_if(INTERVAL *x,INTERVAL *y,INTERVAL *z) {
   return(narrow_imp(y,x,z));
}


/****************************************************************/


narrow_xor(INTERVAL *x, INTERVAL *y, INTERVAL *z)
{
/*** narrow  z  ****/    
    if ((x->lo!=x->hi) && (y->lo!=y->hi))  return(TRUE); /* both x, y unknown*/
    if ((x->lo==x->hi) && (y->lo==y->hi))                /* both x, y  known */
       if  (x->lo==y->lo)       
           return(intersection(0.0, 0.0, z));
       else                     
           return(intersection(1.0, 1.0, z));     
    
/*** narrow x, y ****/                                  /* x or y unknown */
    if (z->hi==0.0)                                        /* z = 0 */
      {
         if (x->lo==x->hi)   
	     { y->lo=x->lo;  y->hi=x->lo; } 
         else  
	     { x->lo=y->lo;  x->hi=y->lo; } 
         return((x->lo <= x->hi) && (y->lo <= y->hi));
       }
    else if (z->lo==1.0)                                   /* z = 1 */
         return(narrow_not(x,y));

}


narrow_lessfn(INTERVAL *x, INTERVAL *y, INTERVAL *r)
{
/*** narrow x, y ****/    
     if (r->lo==1.0)                                   /* r = 1 */
          return(narrow_lt(x,y));
     if (r->hi==0.0)                                   /* r = 0 */
          return(narrow_le(y,x));

/*** narrow r ****/                             
     if (x->hi<y->lo) 
          r->lo=1.0; 
     else if (x->lo>=y->hi)
          r->hi=0.0; 
     return(r->lo <= r->hi);
}


narrow_leqfn(INTERVAL *x, INTERVAL *y, INTERVAL *r)
{
/*** narrow x, y ****/    
     if (r->lo==1.0)                                   /* r = 1 */
          return(narrow_le(x,y));
     if (r->hi==0.0)                                   /* r = 0 */
          return(narrow_lt(y,x));

/*** narrow r ****/    
     if (x->hi<=y->lo) 
          r->lo=1.0;  
     else if (x->lo>y->hi)
          r->hi=0.0;
     return(r->lo <= r->hi);
}


narrow_eqfn(INTERVAL *x, INTERVAL *y, INTERVAL *r)
{
/*** narrow x, y ****/    
     if (r->lo==1.0)                                   /* r = 1 */
          return(narrow_eq(x,y));
     if (r->hi==0.0)                                   /* r = 0 */
          return(narrow_ne(y,x));

/*** narrow r ****/                                    
     if ((x->lo==x->hi) && (y->lo==y->hi) && (x->lo==y->lo))
          r->lo = 1.0;
     else if ((x->hi<y->lo) || (x->lo>y->hi))
          r->hi = 0.0;
     return(r->lo <= r->hi);
}

