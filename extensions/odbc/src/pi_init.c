/*=====================================================================*
 |		pi_init.c
 | Copyright (c) 1988-1994, Applied Logic Systems, Inc.
 |
 |	- Foreign interface initialization
 |		- a segment of the old pimain.c
 | Revision History:
 |	11/16/94, C. Houpt -- Added header file with prototype.
 *=====================================================================*/

#include "pi_init.h"

extern void odbc_init(void);
extern void odbcaux_init(void);

void pi_init(void)
{
	odbc_init();
	odbcaux_init();
}
