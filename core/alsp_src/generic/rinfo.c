
/*
 * rinfo.c              -- Relocation information related routines
 *
 * Copyright (c) 1991-1993, Applied Logic Systems, Inc.
 *
 * Author: Ilyas Cicekli
 * Date  : 01/03/91
 */


#include "defs.h"


#ifdef PACKAGE

#include<stdio.h>
#include "rinfo.h"


unsigned long rinfo_buf[MAX_RINFO_ENTRIES];
unsigned long *rinfo_ptr = 0;

int   rinfo_flag = 0;
int   package_rinfo_flag = 1;


/*
 * Initialize rinfo_ptr
 */
reloc_init()
{
    rinfo_ptr = &rinfo_buf[0];
}


/*
 * Size of relocation information buffer incuding the long word to
 * hold the size information.
 */
rinfo_size()
{
    long  sz;

    if (rinfo_ptr == (unsigned long *) 0) {
	fprintf(stderr,
	"\nInternal Error: Relocation information buffer not initialized.");
	return (0);
    }

    sz = (long) ((unsigned long *) rinfo_ptr - (unsigned long *) &rinfo_buf[0]);
    sz++;
    return (sz);
}


copy_rinfo(clsrinfobuf)
    unsigned long *clsrinfobuf;
{
    long  sz;
    register unsigned long *p, *q, *s;

    if (rinfo_ptr == (unsigned long *) 0)
	return (0);

    sz = (long) ((unsigned long *) rinfo_ptr - (unsigned long *) &rinfo_buf[0]);

    p = clsrinfobuf;
    *p++ = (sz * sizeof (long));
    q = p + sz;
    s = (unsigned long *) &rinfo_buf[0];
    for (; p < q; p++, s++)
	*p = *s;

    rinfo_ptr = (unsigned long *) 0;

}


#endif /* PACKAGE */
