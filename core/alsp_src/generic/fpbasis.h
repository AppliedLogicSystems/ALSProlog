/*===========================================================================*
 |              fpbasis.h
 |      Copyright (c) 1996-97 Applied Logic Systems, Inc.
 |
 |      -- Floating point math abstractions
 |
 *===========================================================================*/

#ifndef MacOS
extern int isnan PARAMS( (double) );
extern int isinf PARAMS( (double) );
extern int finite PARAMS( (double) );
#endif

#ifdef HAVE_IEEE_FP
#include <ieeefp.h>
#endif

extern int is_ieee_nan PARAMS( (double) );
extern int is_ieee_inf PARAMS( (double) );
