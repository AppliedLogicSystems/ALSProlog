/*
 * winaux.c
 *
 */

#include "alspi.h"
#include "cinterf.h"

int CI_get_farptr(farptr, v, t)
	 char _Far ** farptr;
	 PWord v;
	 int t;
{
  double temp;
  PWord arg;
  int argtype;

  switch(t)
	{
	case PI_INT : 
	  *farptr = (char _Far *) v;
	  return( 1 );

	case PI_UIA : 
	  *farptr = (char _Far *) PI_getuianame(0,v,0);
	  return ( 1 );

	case PI_DOUBLE :
	  PI_getdouble(&temp,v);
	  *farptr = (char _Far *)(long) temp;
	  return( 1 );

	case PI_STRUCT :
	  PI_getargn(&arg, &argtype, v, 1);
	  *(unsigned short *) farptr = (unsigned short) arg;

	  PI_getargn(&arg, &argtype, v, 2);
	  *(((unsigned short *) &farptr)+1) = (unsigned short) arg;

	  PI_getargn(&arg, &argtype, v, 3);
	  *(((unsigned short *) &farptr)+2) = (unsigned short) arg;
	  return( 1 );
	}
  return( 0 );
}

