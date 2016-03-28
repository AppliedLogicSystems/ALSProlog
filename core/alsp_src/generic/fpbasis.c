/*===========================================================================*
 |              fpbasis.c
 |      Copyright (c) 1996-97 Applied Logic Systems, Inc.
 |
 |      -- Floating point math abstractions
 |
 *===========================================================================*/
#include "defs.h"
#include "fpbasis.h"

#include <math.h>

int is_ieee_nan PARAMS( (double) );
int is_ieee_inf PARAMS( (double) );

int
is_ieee_nan(v)
	double v;
{
	return isnan(v);
}

int
is_ieee_inf(v)
	double v;
{
#if defined(SOLARIS) || defined(UNIX_SOLARIS)
	switch (fpclass(v)) {
	case FP_NINF:
		return(1);
	case FP_PINF:
		return(1);
	default:
		return(0);
	}
#elif defined(AIX)
	return !finite(v);
#elif defined(__MWERKS__) && (defined(WIN32) || defined(MacOS))
	return !isfinite(v);
#elif (defined(__sgi) && defined(__mips))
	return(!finite(v));
#else
	return isinf(v);
#endif
}
