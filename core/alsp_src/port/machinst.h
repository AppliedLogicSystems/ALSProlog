/*
 * machinst.h		-- include file for byte/threaded code generator
 *	Copyright (c) 1993 Applied Logic Systems, Inc.
 *
 * Author: Kevin A. Buettner
 * Creation Date: 9/15/93
 *
 */

#include "wamops.h"

#define ic_put(data)	*ic_ptr++ = (data)
#define ic_puti(inst)	*ic_ptr++ = abinst(inst)
