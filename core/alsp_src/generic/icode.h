/*
 *	icode.h
 *	Copyright (c) 1988-91 Applied Logic Systems, Inc.
 *| Enum definitions for WAM ICODES;
 */

#define ICODE(a,b,c,d) a,

enum {
#include "icodedef.h"
    _ICODE_END
};

#undef ICODE
