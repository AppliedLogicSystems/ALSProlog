/*===================================================================*
 |		mtypes.h
 |	Copyright (c) 1987-95 Applied Logic Systems, Inc.
 |
 |		Low-level prolog types: M68k version
 |
 | This file contains all machine dependent manipulation of the machine
 | level Prolog types (what the WAM uses), mostly thru including hztypes.h
 |
 | Author: Kevin A. Buettner
 | Created: 2/17/87
 | 03/10/89	K.Hughes -- Consolidated all low-level Prolog type/type
 |			manipulation information to mtypes.h; kept in
 |			machine-specific directories
 | 03/01/95 K.Bowen -- Created hztypes.h in generic from mtypes.h in
 |			portable directory; all "horizontal" tagging implementations
 |			now include this, via: the old mtypes.h includes this, and
 |			contains some specific info (e.g., max integer size)
 *===================================================================*/

#include "hztypes.h"

