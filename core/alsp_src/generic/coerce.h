/*
 * coerce.h
 *	Copyright (c) 1988-1993, Applied Logic Systems, Inc.
 *
 * Handle type conversions in a functional syntax.
 *
 * This is done for two reasons. I hate the current method for
 * doing pointer coercions and second, C++ allows functional
 * syntax for coercions and the system should be converted
 * someday.
 *
 * Written by Keith Hughes 12/13/88
 */

#define IntPtr(p)	(*(int **)&(p))
#define CharPtr(p)	(*(char **)&(p))
#define ShortPtr(p)	(*(short **)&(p))
#define LongPtr(p)	(*(long **)&(p))
#define CodePtrPtr(p)	(*(CodePtr **)&(p))

#define IntVal(p)	((int)(p))
#define CharVal(p)	((char)(p))
#define ShortVal(p)	((short)(p))
#define LongVal(p)	((long)(p))

