/*===========================================================================*
 |              fpbasis.c
 |      Copyright (c) 1996-97 Applied Logic Systems, Inc.
 |
 |      -- Floating point math abstractions
 |
 *===========================================================================*/
#include "defs.h"
#include "fpbasis.h"
#ifdef MacOS
#include <fp.h>
#elif defined(MSWin32)
#include <fdlibm.h>
#endif

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
#ifdef SOLARIS
	switch (fpclass(v)) {
	case FP_NINF:
		return(1);
	case FP_PINF:
		return(1);
	default:
		return(0);
	}
#elif defined(WIN32) || defined(AIX)
	return !finite(v);
#elif defined(MacOS)
	return !isfinite(v);
#elif (defined(__sgi) && defined(__mips))
	return(!finite(v));
#else
	return isinf(v);
#endif
}
